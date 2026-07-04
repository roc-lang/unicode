#!/usr/bin/env python3
from __future__ import annotations

import argparse
import functools
import http.server
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LOCAL_PACKAGE = 'unicode: "../package/main.roc"'
ROC = os.environ.get("ROC", "roc")


def run(cmd: list[str], *, cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    print("+", " ".join(cmd))
    completed = subprocess.run(
        cmd,
        cwd=cwd,
        text=True,
        encoding="utf-8",
        errors="replace",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    if completed.returncode != 0:
        if completed.stdout:
            print(completed.stdout)
        if completed.stderr:
            print(completed.stderr, file=sys.stderr)
        raise SystemExit(f"command failed with exit code {completed.returncode}: {' '.join(cmd)}")

    if completed.stdout:
        print(completed.stdout)
    if completed.stderr:
        print(completed.stderr, file=sys.stderr)

    return completed


def find_free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return int(sock.getsockname()[1])


def bundle_package(bundle_dir: Path) -> Path:
    completed = run(["scripts/bundle.sh", "--output-dir", str(bundle_dir)])
    output = f"{completed.stdout}\n{completed.stderr}"
    match = re.search(r"^Created:\s+(.+)$", output, re.MULTILINE)

    if match is None:
        raise SystemExit("Could not find bundle path in roc bundle output")

    bundle_path = Path(match.group(1).strip())
    if not bundle_path.is_absolute():
        bundle_path = (ROOT / bundle_path).resolve()
    if not bundle_path.exists():
        raise SystemExit(f"Bundle was not created: {bundle_path}")

    return bundle_path


def start_server(directory: Path) -> tuple[http.server.ThreadingHTTPServer, str]:
    port = find_free_port()
    handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=str(directory))
    server = http.server.ThreadingHTTPServer(("127.0.0.1", port), handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    return server, f"http://127.0.0.1:{port}"


def copy_examples_with_bundle_url(examples_dir: Path, bundle_url: str) -> list[Path]:
    target_dir = examples_dir / "examples"
    target_dir.mkdir()

    examples: list[Path] = []
    for source_path in sorted((ROOT / "examples").glob("*.roc")):
        target_path = target_dir / source_path.name
        source = source_path.read_text(encoding="utf-8")
        if LOCAL_PACKAGE not in source:
            raise SystemExit(f"{source_path.name} does not use the expected local package dependency")

        target_path.write_text(source.replace(LOCAL_PACKAGE, f'unicode: "{bundle_url}"'), encoding="utf-8")
        examples.append(target_path)

    if not examples:
        raise SystemExit("No examples found to test")

    return examples


def run_example_checks(examples: list[Path]) -> None:
    for example in examples:
        run([ROC, "check", example.name, "--no-cache"], cwd=example.parent)


def run_example_tests(examples: list[Path]) -> None:
    for example in examples:
        if re.search(r"(?m)^\s*expect\b", example.read_text(encoding="utf-8")):
            run([ROC, "test", example.name, "--no-cache"], cwd=example.parent)


def run_example_apps(examples: list[Path]) -> None:
    for example in examples:
        run([ROC, example.name, "--no-cache"], cwd=example.parent)


def build_and_run_examples(examples: list[Path], build_dir: Path) -> None:
    build_dir.mkdir(parents=True, exist_ok=True)
    exe_suffix = ".exe" if os.name == "nt" else ""

    for example in examples:
        output = build_dir / f"{example.stem}{exe_suffix}"
        run([ROC, "build", example.name, f"--output={output}", "--no-cache"], cwd=example.parent)
        run([str(output)])


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--bundle-path", type=Path, help="Use an existing bundle instead of creating one")
    parser.add_argument("--skip-build-run", action="store_true", help="Skip compiled example execution")
    args = parser.parse_args()

    default_tmp = ROOT / ".roc-unicode-tmp"
    tmp_parent = Path(os.environ.get("ROC_UNICODE_TMPDIR", default_tmp))
    tmp_parent.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(prefix="roc-unicode-bundle-", dir=tmp_parent) as tmp:
        tmp_dir = Path(tmp)
        bundle_dir = tmp_dir / "bundle"
        examples_dir = tmp_dir / "rewritten"
        build_dir = tmp_dir / "build"

        bundle_dir.mkdir()
        examples_dir.mkdir()

        if args.bundle_path is None:
            bundle_path = bundle_package(bundle_dir)
        else:
            source_bundle = args.bundle_path.resolve()
            if not source_bundle.exists():
                raise SystemExit(f"Bundle does not exist: {source_bundle}")

            bundle_path = bundle_dir / source_bundle.name
            shutil.copy2(source_bundle, bundle_path)

        server, base_url = start_server(bundle_dir)
        try:
            bundle_url = f"{base_url}/{bundle_path.name}"
            examples = copy_examples_with_bundle_url(examples_dir, bundle_url)

            print(f"Testing examples with bundled package: {bundle_url}")
            run_example_checks(examples)
            run_example_tests(examples)
            run_example_apps(examples)

            if not args.skip_build_run:
                build_and_run_examples(examples, build_dir)
        finally:
            server.shutdown()
            server.server_close()


if __name__ == "__main__":
    main()
