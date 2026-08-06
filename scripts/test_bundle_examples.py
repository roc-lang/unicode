#!/usr/bin/env python3
"""Validate roc-unicode and run every example case against a served bundle."""

from __future__ import annotations

import argparse
import difflib
import functools
import http.server
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Sequence


ROOT = Path(__file__).resolve().parents[1]
SPEC_PATH = ROOT / "examples" / "spec.json"
UNICODE_MANIFEST_PATH = ROOT / "vendor" / "unicode" / "manifest.json"
LOCAL_PACKAGE_PATH = "../package/main.roc"
PACKAGE_DEPENDENCY_RE = re.compile(
    r'(?m)^(?P<indent>\s*)unicode:\s*"(?P<dependency>[^"]+)",(?P<suffix>\s*(?:#.*)?)$'
)
ANSI_SGR_RE = re.compile(r"\x1b\[[0-9;]*m")
SCHEMA_VERSION = 2
ROOT_FIELDS = frozenset({"schema_version", "apps"})
APP_FIELDS = frozenset({"path", "cases"})
STREAMS = ("stdout", "stderr")
ASSERTION_KINDS = ("exact", "contains", "regex")


def assertion_field(stream: str, kind: str) -> str:
    return stream if kind == "exact" else f"{stream}_{kind}"


OUTPUT_ASSERTION_FIELDS = frozenset(
    assertion_field(stream, kind)
    for stream in STREAMS
    for kind in ASSERTION_KINDS
)
CASE_FIELDS = frozenset(
    {
        "name",
        "args",
        "stdin",
        "exit_code",
        "timeout_seconds",
    }
) | OUTPUT_ASSERTION_FIELDS
DEFAULT_STDIN = ""
DEFAULT_EXIT_CODE = 0
DEFAULT_TIMEOUT_SECONDS = 10
SERVER_READY_TIMEOUT_SECONDS = 5
SERVER_REQUEST_TIMEOUT_SECONDS = 0.5
SERVER_POLL_INTERVAL_SECONDS = 0.05


class TestFailure(RuntimeError):
    pass


def configure_output_encoding() -> None:
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(encoding="utf-8", errors="backslashreplace")


def display_path(path: Path) -> str:
    try:
        return path.resolve().relative_to(ROOT).as_posix()
    except ValueError:
        return path.resolve().as_posix()


def command_text(args: Sequence[str | Path]) -> str:
    values = [str(arg) for arg in args]
    return subprocess.list2cmdline(values) if os.name == "nt" else " ".join(values)


def run(
    args: Sequence[str | Path],
    *,
    cwd: Path = ROOT,
    env: dict[str, str] | None = None,
    capture: bool = False,
) -> subprocess.CompletedProcess[str]:
    values = [str(arg) for arg in args]
    print(f"+ {command_text(values)}", flush=True)
    completed = subprocess.run(
        values,
        cwd=cwd,
        env=env,
        text=True,
        encoding="utf-8",
        errors="replace",
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
        check=False,
    )
    if completed.returncode != 0:
        if capture and completed.stdout:
            print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n")
        raise TestFailure(
            f"command exited {completed.returncode}: {command_text(values)}"
        )
    return completed


def verify_pinned_roc(roc: str) -> str:
    result = run([roc, "version"], capture=True)
    actual = result.stdout.strip()
    pinned = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    pinned_revision = pinned.rsplit("-", 1)[-1]
    if pinned not in actual and pinned_revision not in actual:
        raise TestFailure(f"repository requires {pinned}, got {actual!r}")
    return actual


def string_list(owner: str, value: object) -> list[str]:
    if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
        raise TestFailure(f"{owner} must be a list of strings")
    return value


def validate_spec(
    data: object, discovered: set[str]
) -> list[dict[str, object]]:
    if not isinstance(data, dict) or set(data) != ROOT_FIELDS:
        raise TestFailure(f"{SPEC_PATH}: fields must be exactly schema_version and apps")
    if data["schema_version"] != SCHEMA_VERSION:
        raise TestFailure(f"{SPEC_PATH}: unsupported schema_version")
    apps = data["apps"]
    if (
        not isinstance(apps, list)
        or not apps
        or not all(isinstance(app, dict) for app in apps)
    ):
        raise TestFailure(f"{SPEC_PATH}: apps must be a non-empty list of objects")

    paths = [app.get("path") for app in apps]
    if not all(isinstance(path, str) and path for path in paths):
        raise TestFailure(f"{SPEC_PATH}: every app needs a non-empty string path")
    if len(paths) != len(set(paths)):
        raise TestFailure(f"{SPEC_PATH}: app paths must be unique")

    specified = set(paths)
    if discovered != specified:
        raise TestFailure(
            f"example spec drift: missing={sorted(discovered - specified)}, "
            f"stale={sorted(specified - discovered)}"
        )

    for app in apps:
        path = str(app["path"])
        if set(app) != APP_FIELDS:
            raise TestFailure(f"{path}: fields must be exactly {sorted(APP_FIELDS)}")
        cases = app.get("cases")
        if (
            not isinstance(cases, list)
            or not cases
            or not all(isinstance(case, dict) for case in cases)
        ):
            raise TestFailure(f"{path}: cases must be a non-empty list of objects")
        names = [case.get("name") for case in cases]
        if not all(isinstance(name, str) and name for name in names):
            raise TestFailure(f"{path}: every case needs a non-empty string name")
        if len(names) != len(set(names)):
            raise TestFailure(f"{path}: case names must be unique")
        for case in cases:
            validate_case(path, case)
        exit_codes = [expected_exit_code(case) for case in cases]
        if not any(exit_code == 0 for exit_code in exit_codes):
            raise TestFailure(f"{path}: cases must include a successful path")
        if not any(exit_code != 0 for exit_code in exit_codes):
            raise TestFailure(f"{path}: cases must include an error path")
    return apps


def load_spec() -> list[dict[str, object]]:
    try:
        data = json.loads(SPEC_PATH.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise TestFailure(f"unable to read {SPEC_PATH}: {error}") from error

    discovered = {
        path.relative_to(ROOT).as_posix() for path in (ROOT / "examples").glob("*.roc")
    }
    return validate_spec(data, discovered)


def expected_exit_code(case: dict[str, object]) -> int:
    return int(case.get("exit_code", DEFAULT_EXIT_CODE))


def validate_case(path: str, case: dict[str, object]) -> None:
    unknown = set(case) - CASE_FIELDS
    if unknown:
        raise TestFailure(f"{path} [{case['name']}]: unknown fields {sorted(unknown)}")
    string_list(f"{path} [{case['name']}] args", case.get("args", []))
    stdin = case.get("stdin", DEFAULT_STDIN)
    if not isinstance(stdin, str):
        raise TestFailure(f"{path} [{case['name']}]: stdin must be a string")
    exit_code = case.get("exit_code", DEFAULT_EXIT_CODE)
    if not isinstance(exit_code, int) or isinstance(exit_code, bool):
        raise TestFailure(f"{path} [{case['name']}]: exit_code must be an integer")
    timeout = case.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)
    if not isinstance(timeout, (int, float)) or isinstance(timeout, bool) or timeout <= 0:
        raise TestFailure(
            f"{path} [{case['name']}]: timeout_seconds must be a positive number"
        )

    if not OUTPUT_ASSERTION_FIELDS.intersection(case):
        raise TestFailure(f"{path} [{case['name']}]: at least one output assertion is required")
    for stream in STREAMS:
        exact = case.get(stream)
        if exact is not None and not isinstance(exact, str):
            raise TestFailure(f"{path} [{case['name']}]: {stream} must be a string")
        for kind in ASSERTION_KINDS[1:]:
            field = assertion_field(stream, kind)
            values = string_list(
                f"{path} [{case['name']}] {field}",
                case.get(field, []),
            )
            if kind == "regex":
                for pattern in values:
                    try:
                        re.compile(pattern)
                    except re.error as error:
                        raise TestFailure(
                            f"{path} [{case['name']}]: invalid {stream}_regex {pattern!r}: {error}"
                        ) from error
    if "stdout" not in case:
        raise TestFailure(f"{path} [{case['name']}]: exact stdout is required")
    if expected_exit_code(case) != 0 and "stderr" not in case:
        raise TestFailure(
            f"{path} [{case['name']}]: failing cases require exact stderr"
        )


def bundle_package(bundle_dir: Path, roc: str) -> Path:
    env = os.environ.copy()
    env["ROC"] = roc
    completed = run(
        [ROOT / "scripts" / "bundle.sh", "--output-dir", bundle_dir],
        env=env,
        capture=True,
    )
    print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n")
    match = re.search(r"^Created:\s+(.+\.(?:tar\.br|tar\.zst))\s*$", completed.stdout, re.MULTILINE)
    if match is None:
        raise TestFailure("could not find bundle path in roc bundle output")
    bundle = Path(match.group(1))
    if not bundle.is_absolute():
        bundle = (ROOT / bundle).resolve()
    if not bundle.is_file():
        raise TestFailure(f"bundle was not created: {bundle}")
    return bundle


def isolated_roc_environment(cache_dir: Path) -> dict[str, str]:
    env = os.environ.copy()
    cache = str(cache_dir.resolve())
    env["ROC_CACHE_DIR"] = cache
    env["XDG_CACHE_HOME"] = cache
    return env


class QuietRequestHandler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, _format: str, *_args: object) -> None:
        pass

    def do_GET(self) -> None:
        self.server.bundle_get_requests += 1  # type: ignore[attr-defined]
        super().do_GET()


class BundleServer:
    def __init__(self, bundle: Path) -> None:
        handler = functools.partial(QuietRequestHandler, directory=str(bundle.parent))
        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
        self.server.bundle_get_requests = 0  # type: ignore[attr-defined]
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.url = f"http://127.0.0.1:{self.server.server_port}/{bundle.name}"

    @property
    def get_requests(self) -> int:
        return int(self.server.bundle_get_requests)  # type: ignore[attr-defined]

    def __enter__(self) -> BundleServer:
        self.thread.start()
        deadline = time.monotonic() + SERVER_READY_TIMEOUT_SECONDS
        while True:
            try:
                request = urllib.request.Request(self.url, method="HEAD")
                with urllib.request.urlopen(
                    request, timeout=SERVER_REQUEST_TIMEOUT_SECONDS
                ) as response:
                    if response.status == 200:
                        return self
            except (OSError, urllib.error.URLError):
                if time.monotonic() >= deadline:
                    self.__exit__()
                    raise TestFailure(f"bundle server did not become ready: {self.url}")
                time.sleep(SERVER_POLL_INTERVAL_SECONDS)

    def __exit__(self, *_args: object) -> None:
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()


def copy_examples_with_bundle_url(destination: Path, bundle_url: str) -> dict[str, Path]:
    target_dir = destination / "examples"
    shutil.copytree(ROOT / "examples", target_dir)
    rewritten: dict[str, Path] = {}

    for source in sorted(target_dir.glob("*.roc")):
        contents = source.read_text(encoding="utf-8")
        matches = [
            match
            for match in PACKAGE_DEPENDENCY_RE.finditer(contents)
            if match.group("dependency") == LOCAL_PACKAGE_PATH
        ]
        if len(matches) != 1:
            raise TestFailure(
                f"{source.name} must declare exactly one {LOCAL_PACKAGE_PATH!r} Unicode dependency"
            )
        match = matches[0]
        updated = (
            contents[: match.start()]
            + f'{match.group("indent")}unicode: "{bundle_url}",{match.group("suffix")}'
            + contents[match.end() :]
        )
        encoded = updated.encode("utf-8")
        if b"\r\n" in encoded:
            raise TestFailure(f"{source.name}: rewritten Roc source contains CRLF line endings")
        source.write_bytes(encoded)
        rewritten[f"examples/{source.name}"] = source
    return rewritten


def generated_roc_sources() -> set[Path]:
    try:
        manifest = json.loads(UNICODE_MANIFEST_PATH.read_text(encoding="utf-8"))
        artifacts = manifest["artifacts"]
    except (KeyError, OSError, json.JSONDecodeError, TypeError) as error:
        raise TestFailure(
            f"unable to read generated outputs from {UNICODE_MANIFEST_PATH}: {error}"
        ) from error
    if not isinstance(artifacts, dict):
        raise TestFailure(f"{UNICODE_MANIFEST_PATH}: artifacts must be an object")

    sources: set[Path] = set()
    for name, artifact in artifacts.items():
        if not isinstance(artifact, dict) or not isinstance(artifact.get("output"), str):
            raise TestFailure(
                f"{UNICODE_MANIFEST_PATH}: artifact {name!r} needs a string output"
            )
        source = (ROOT / str(artifact["output"])).resolve()
        if source.parent != (ROOT / "package").resolve() or source.suffix != ".roc":
            raise TestFailure(
                f"{UNICODE_MANIFEST_PATH}: artifact {name!r} output is not a package Roc module"
            )
        sources.add(source)
    return sources


def validate_package(roc: str, docs_dir: Path, env: dict[str, str]) -> None:
    print("\n=== FORMAT ===")
    generated_sources = generated_roc_sources()
    maintained_package_sources = [
        source
        for source in sorted((ROOT / "package").glob("*.roc"))
        if source.resolve() not in generated_sources
    ]
    for source in maintained_package_sources + sorted((ROOT / "examples").glob("*.roc")):
        run([roc, "fmt", "--check", display_path(source)], env=env)

    print("\n=== PACKAGE ===")
    run([roc, "check", "package/main.roc", "--no-cache"], env=env)
    run([roc, "test", "package/main.roc", "--no-cache"], env=env)
    run(
        [roc, "docs", "package/main.roc", f"--output={display_path(docs_dir)}"],
        env=env,
    )


def normalize_output(value: str) -> str:
    newlines = value.replace("\r\n", "\n").replace("\r", "\n")
    return ANSI_SGR_RE.sub("", newlines)


def assert_output(path: str, case: dict[str, object], stream: str, actual: str) -> None:
    name = str(case["name"])
    normalized = normalize_output(actual)
    raw_normalized = actual.replace("\r\n", "\n").replace("\r", "\n")
    raw_note = "" if raw_normalized == normalized else f"\n--- raw {stream} ---\n{raw_normalized!r}"
    if "[ROC CRASHED]" in normalized:
        raise TestFailure(f"{path} [{name}]: Roc runtime crash\n{normalized}{raw_note}")

    assertions = {assertion_field(stream, kind) for kind in ASSERTION_KINDS}
    expected = case.get(stream)
    if stream == "stderr" and not assertions.intersection(case):
        expected = ""
    if isinstance(expected, str):
        expected_normalized = normalize_output(expected)
        if normalized != expected_normalized:
            diff = "".join(
                difflib.unified_diff(
                    expected_normalized.splitlines(keepends=True),
                    normalized.splitlines(keepends=True),
                    fromfile=f"expected {stream}",
                    tofile=f"actual {stream}",
                )
            )
            raise TestFailure(f"{path} [{name}]: unexpected {stream}\n{diff}{raw_note}")
    for expected_text in case.get(f"{stream}_contains", []):
        if expected_text not in normalized:
            raise TestFailure(
                f"{path} [{name}]: missing {stream} output {expected_text!r}"
                f"\n--- {stream} ---\n{normalized}{raw_note}"
            )
    for pattern in case.get(f"{stream}_regex", []):
        if re.search(pattern, normalized, re.MULTILINE) is None:
            raise TestFailure(
                f"{path} [{name}]: {stream} did not match {pattern!r}"
                f"\n--- {stream} ---\n{normalized}{raw_note}"
            )


def run_case(path: str, binary: Path, case: dict[str, object]) -> None:
    name = str(case["name"])
    args = [str(binary), *string_list(f"{path} [{name}] args", case.get("args", []))]
    print(f"CASE {path} [{name}]", flush=True)
    try:
        result = subprocess.run(
            args,
            cwd=ROOT,
            input=str(case.get("stdin", DEFAULT_STDIN)).encode("utf-8"),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=float(case.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)),
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        raise TestFailure(f"{path} [{name}]: timed out after {error.timeout}s") from error
    stdout = result.stdout.decode("utf-8", errors="replace")
    stderr = result.stderr.decode("utf-8", errors="replace")
    expected_exit = expected_exit_code(case)
    if result.returncode != expected_exit:
        raise TestFailure(
            f"{path} [{name}]: exited with {result.returncode}, expected {expected_exit}"
            f"\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}"
        )
    assert_output(path, case, "stdout", stdout)
    assert_output(path, case, "stderr", stderr)


def run_examples(
    apps: list[dict[str, object]],
    sources: dict[str, Path],
    build_dir: Path,
    roc: str,
    env: dict[str, str],
) -> None:
    print("\n=== BUNDLED EXAMPLES ===")
    build_dir.mkdir(parents=True, exist_ok=True)
    binaries: dict[str, Path] = {}
    suffix = ".exe" if os.name == "nt" else ""

    for app in apps:
        path = str(app["path"])
        source = sources[path]
        source_arg = display_path(source)
        run([roc, "fmt", "--check", source_arg], env=env)
        run([roc, "check", source_arg, "--no-cache"], env=env)
        run([roc, "test", source_arg, "--no-cache"], env=env)
        binary = build_dir / f"{source.stem}{suffix}"
        run(
            [
                roc,
                "build",
                source_arg,
                "--opt=speed",
                f"--output={display_path(binary)}",
                "--no-cache",
            ],
            env=env,
        )
        binaries[path] = binary

    print("\n=== EXAMPLE SPEC CASES ===")
    total = 0
    for app in apps:
        path = str(app["path"])
        cases = app["cases"]
        assert isinstance(cases, list)
        for case in cases:
            assert isinstance(case, dict)
            run_case(path, binaries[path], case)
            total += 1
    print(f"\nAll {total} example spec cases passed.")


def main() -> int:
    configure_output_encoding()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--bundle-path", type=Path, help="test an existing package bundle")
    parser.add_argument(
        "--examples-only",
        action="store_true",
        help="skip package validation when an earlier CI job already performed it",
    )
    args = parser.parse_args()

    try:
        if shutil.which(args.roc) is None:
            raise TestFailure(f"{args.roc!r} was not found on PATH")
        actual = verify_pinned_roc(args.roc)
        print(f"Using {actual}")
        apps = load_spec()

        temp_parent = Path(os.environ.get("ROC_UNICODE_TMPDIR", ROOT / ".roc-unicode-tmp"))
        temp_parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(prefix="roc-unicode-examples-", dir=temp_parent) as temp:
            temp_dir = Path(temp)
            roc_env = isolated_roc_environment(temp_dir / "roc-cache")
            if args.bundle_path is None:
                bundle_dir = temp_dir / "bundle"
                bundle_dir.mkdir()
                bundle = bundle_package(bundle_dir, args.roc)
            else:
                bundle = args.bundle_path.resolve()
                if not bundle.is_file():
                    raise TestFailure(f"bundle does not exist: {bundle}")

            with BundleServer(bundle) as bundle_server:
                print(f"Bundle: {bundle_server.url}")
                sources = copy_examples_with_bundle_url(
                    temp_dir / "rewritten", bundle_server.url
                )
                if not args.examples_only:
                    validate_package(args.roc, temp_dir / "docs", roc_env)
                run_examples(apps, sources, temp_dir / "build", args.roc, roc_env)
                if bundle_server.get_requests == 0:
                    raise TestFailure("examples never requested the served package bundle")
    except (OSError, TestFailure) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
