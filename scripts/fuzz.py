#!/usr/bin/env python3
"""Build and operate roc-unicode's coverage-guided fuzz targets."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import re
import signal
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

import unicode_data


ROOT = Path(__file__).resolve().parents[1]
FUZZ_ROOT = ROOT / "fuzz"
WORK_ROOT = ROOT / ".roc-unicode-tmp" / "fuzz"
BIN_ROOT = WORK_ROOT / "bin"
PERSISTENT_CORPUS_ROOT = WORK_ROOT / "corpus" / "plan-v1"
REPORT_ROOT = WORK_ROOT / "reports"
PLATFORM_RELEASE = "0.8.0"
PLATFORM_BUNDLE_ID = "BUNcJDHxCd7xLVSe3WBioyXqR6tm1MqXwzwGgtnmqK43"
PLATFORM_SHA256 = "8d7a2df0c33658e33968e774f9b62669a290fbbf05d477982458a382b89fa664"
SMOKE_SEED = 50
INPUT_TIMEOUT = 5
SUPERVISOR_MARGIN_SECONDS = 30
MEMORY_LIMIT = 2048
CONFORMANCE_SEED_LIMIT = 256
SEED_PATH = FUZZ_ROOT / "seeds.json"
NAME_PATTERN = re.compile(r"[a-z0-9]+(?:-[a-z0-9]+)*\Z")


class FuzzFailure(RuntimeError):
    pass


@dataclass(frozen=True)
class Target:
    name: str
    suite: str
    max_input_size: int

    @property
    def declared_name(self) -> str:
        return f"unicode-{self.name}"

    @property
    def source(self) -> Path:
        return FUZZ_ROOT / f"{self.suite}.roc"

    @property
    def binary(self) -> Path:
        return BIN_ROOT / self.suite


TARGETS = {
    target.name: target
    for target in (
        Target("utf8", "source_suite", 512),
        Target("utf8-model", "source_suite", 4),
        Target("grapheme", "source_suite", 385),
        Target("word", "source_suite", 385),
        Target("line-break", "source_suite", 385),
        Target("case", "scalar_suite", 385),
        Target("property", "scalar_suite", 385),
        Target("bidi", "analysis_suite", 385),
        Target("script", "analysis_suite", 385),
        Target("thai", "thai_suite", 66),
    )
}
BUILTIN_EXAMPLE_TARGETS = {"utf8-model", "thai"}


def command(
    args: Sequence[str | Path],
    *,
    cwd: Path = ROOT,
    capture: bool = False,
    timeout: float | None = None,
) -> subprocess.CompletedProcess[str]:
    values = [str(value) for value in args]
    print("+", " ".join(values), flush=True)
    with subprocess.Popen(
        values,
        cwd=cwd,
        text=True,
        encoding="utf-8",
        errors="replace",
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
        start_new_session=timeout is not None,
    ) as process:
        try:
            output, _ = process.communicate(timeout=timeout)
        except subprocess.TimeoutExpired as error:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.communicate()
            raise FuzzFailure(
                f"command exceeded the {timeout:g}-second deadline: {' '.join(values)}"
            ) from error
        completed = subprocess.CompletedProcess(values, process.returncode, output)
    if completed.returncode != 0:
        if capture and completed.stdout:
            print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n")
        raise FuzzFailure(
            f"command exited {completed.returncode}: {' '.join(values)}"
        )
    return completed


def verify_host() -> None:
    system = platform.system()
    machine = platform.machine().lower()
    supported = (system == "Linux" and machine == "x86_64") or (
        system == "Darwin" and machine == "arm64"
    )
    if not supported:
        raise FuzzFailure(
            f"roc-fuzz {PLATFORM_RELEASE} supports Linux x86-64 and Apple Silicon macOS; "
            f"this host reports {system} {machine}"
        )


def verify_pinned_roc(roc: str) -> None:
    result = command([roc, "version"], capture=True)
    actual = result.stdout.strip()
    lines = (ROOT / ".roc-version").read_text(encoding="utf-8").splitlines()
    if len(lines) != 1 or not lines[0].startswith("nightly-"):
        raise FuzzFailure(".roc-version must contain exactly one Roc nightly tag")
    pinned = lines[0]
    revision = pinned.rsplit("-", 1)[-1]
    if pinned not in actual and revision not in actual:
        raise FuzzFailure(f"repository requires {pinned}, got {actual!r}")


def selected_targets(name: str) -> list[Target]:
    if name == "all":
        return list(TARGETS.values())
    return [TARGETS[name]]


def validate_seed_entry(owner: str, value: object) -> tuple[str, bytes]:
    if not isinstance(value, dict) or set(value) != {"name", "bytes_hex"}:
        raise FuzzFailure(f"{owner} entries must contain exactly name and bytes_hex")
    name = value["name"]
    bytes_hex = value["bytes_hex"]
    if not isinstance(name, str) or NAME_PATTERN.fullmatch(name) is None:
        raise FuzzFailure(f"{owner} seed name must be lowercase kebab-case: {name!r}")
    if not isinstance(bytes_hex, str):
        raise FuzzFailure(f"{owner}/{name} bytes_hex must be a string")
    try:
        payload = bytes.fromhex(bytes_hex)
    except ValueError as error:
        raise FuzzFailure(f"{owner}/{name} has invalid hexadecimal bytes") from error
    if payload.hex().upper() != bytes_hex:
        raise FuzzFailure(
            f"{owner}/{name} bytes_hex must be whitespace-free uppercase hexadecimal"
        )
    return name, payload


def load_seeds() -> dict[str, list[tuple[str, bytes]]]:
    try:
        data = json.loads(SEED_PATH.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise FuzzFailure(f"unable to load {SEED_PATH}: {error}") from error
    owners = set(TARGETS) - BUILTIN_EXAMPLE_TARGETS
    if not isinstance(data, dict) or set(data) != {"schema_version", *owners}:
        raise FuzzFailure("fuzz seed manifest inventory does not match its targets")
    if data["schema_version"] != 1:
        raise FuzzFailure("unsupported fuzz seed manifest schema")

    result: dict[str, list[tuple[str, bytes]]] = {}
    for target_name in sorted(owners):
        entries = data[target_name]
        if not isinstance(entries, list):
            raise FuzzFailure(f"{target_name} seeds must be a list")
        parsed = [
            validate_seed_entry(target_name, value) for value in entries
        ]
        names = [name for name, _payload in parsed]
        if len(names) != len(set(names)):
            raise FuzzFailure(f"{target_name} seed names must be unique")
        if target_name != "utf8":
            for name, payload in parsed:
                try:
                    payload.decode("utf-8")
                except UnicodeDecodeError as error:
                    raise FuzzFailure(f"{target_name}/{name} must be valid UTF-8") from error
        result[target_name] = parsed
    return result


def scalar_entropy(
    code_points: Sequence[int], *, config: int = 0, planned: bool = False
) -> bytes:
    result = bytearray([config])
    for index, code_point in enumerate(code_points):
        if not (0 <= code_point <= 0x10FFFF) or 0xD800 <= code_point <= 0xDFFF:
            raise FuzzFailure(f"U+{code_point:04X} is not a Unicode scalar")
        rank = code_point if code_point <= 0xD7FF else code_point - 0x800
        action = (index % 8) if planned else 0
        result.extend((rank | (action << 21)).to_bytes(3, "little"))
    return bytes(result)


def target_seed_payloads(target: Target) -> list[tuple[str, bytes]]:
    if target.name in BUILTIN_EXAMPLE_TARGETS:
        return []
    manifest = load_seeds()
    if target.name == "utf8":
        return manifest["utf8"]

    seeds = []
    for name, payload in manifest[target.name]:
        points = [ord(character) for character in payload.decode("utf-8")]
        seeds.append((name, scalar_entropy(points)))
        for config in (1, 2, 3, 6, 8, 12, 16, 32, 48, 64):
            seeds.append((f"{name}-plan-{config}", scalar_entropy(points, config=config, planned=True)))

    unicode_manifest = unicode_data.load_manifest()
    release = unicode_data.release_version(unicode_manifest, "unicode")
    parse_tests = {
        "grapheme": unicode_data.parse_grapheme_tests,
        "word": unicode_data.parse_word_break_tests,
        "line-break": unicode_data.parse_line_break_tests,
        "bidi": unicode_data.parse_bidi_character_tests,
    }
    if target.name in parse_tests:
        cases = [
            case for case in parse_tests[target.name](unicode_manifest)
            if 1 + 3 * len(case.code_points) <= target.max_input_size
        ]
        for case in evenly_sample(cases):
            config = case.paragraph_mode if target.name == "bidi" else 0
            seeds.append((f"unicode-{release}-{case.line}", scalar_entropy(case.code_points, config=config)))
    elif target.name in ("case", "property"):
        canonical = unicode_data.load_canonical_properties(unicode_manifest)
        if target.name == "case":
            case_data = unicode_data.load_case_data(unicode_manifest, canonical)
            for entry in evenly_sample((*case_data.special, *case_data.folding)):
                seeds.append((f"case-source-{entry.line}-{entry.source}", scalar_entropy([entry.source])))
        else:
            public = unicode_data.load_public_properties(unicode_manifest, canonical)
            fields = (
                canonical.general_category, canonical.canonical_combining_class,
                public.bidi_class.records, public.bidi_class.defaults,
                public.joining_type.records, public.joining_group,
                public.indic_syllabic_category.records, public.indic_positional_category.records,
                public.vertical_orientation.records, public.vertical_orientation.defaults,
                public.default_ignorable, public.variation_selector,
            )
            points = sorted({
                point for records in fields for record in records
                for point in (record.start - 1, record.start, record.end, record.end + 1)
                if 0 <= point <= 0x10FFFF and not 0xD800 <= point <= 0xDFFF
            })
            for index in range(0, len(points), 128):
                seeds.append((f"property-boundaries-{index}", scalar_entropy(points[index:index + 128])))
    return seeds


def evenly_sample(cases: Sequence) -> Sequence:
    if len(cases) <= CONFORMANCE_SEED_LIMIT:
        return cases
    return [
        cases[index * (len(cases) - 1) // (CONFORMANCE_SEED_LIMIT - 1)]
        for index in range(CONFORMANCE_SEED_LIMIT)
    ]


def write_corpus_entry(destination: Path, payload: bytes) -> None:
    digest = hashlib.sha256(payload).hexdigest()
    path = destination / digest
    if not path.exists():
        path.write_bytes(payload)


def populate_corpus(target: Target, destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    seeds = target_seed_payloads(target)
    for name, payload in seeds:
        if len(payload) <= target.max_input_size:
            write_corpus_entry(destination, payload)
        else:
            print(f"Skipping {target.name}/{name}, which exceeds the input bound")
    print(f"Prepared {target.name} corpus in {destination}")


def build_targets(roc: str, targets: Sequence[Target]) -> None:
    verify_host()
    verify_pinned_roc(roc)
    BIN_ROOT.mkdir(parents=True, exist_ok=True)
    command([roc, "fmt", "--check", *sorted(FUZZ_ROOT.glob("*.roc"))])
    for suite in dict.fromkeys(target.suite for target in targets):
        members = [target for target in TARGETS.values() if target.suite == suite]
        representative = members[0]
        expected_platform = (
            f'https://github.com/lukewilliamboswell/roc-fuzz/releases/download/'
            f'{PLATFORM_RELEASE}/{PLATFORM_BUNDLE_ID}.tar.zst'
        )
        if f'fuzz: platform "{expected_platform}"' not in representative.source.read_text(encoding="utf-8"):
            raise FuzzFailure(f"{suite} platform URL differs from the runner pin")
        command([roc, "check", representative.source, "--no-cache"])
        command([
            roc, "build", "--fuzz", "--opt=speed", representative.source,
            f"--output={representative.binary}", "--no-cache",
        ])
        listed = command([representative.binary, "--list-targets"], capture=True).stdout.splitlines()
        if sorted(listed) != sorted(target.declared_name for target in members):
            raise FuzzFailure(f"{suite} binary inventory differs from the runner: {listed!r}")


def run_models(roc: str, targets: Sequence[Target]) -> None:
    verify_pinned_roc(roc)
    for source in dict.fromkeys(target.source for target in targets):
        command([roc, "test", source, "--no-cache"])


def new_report_directory(target: Target, operation: str) -> Path:
    REPORT_ROOT.mkdir(parents=True, exist_ok=True)
    return Path(tempfile.mkdtemp(prefix=f"{target.name}-{operation}-", dir=REPORT_ROOT))


def execution_arguments(
    target: Target, operation: str, report: Path | None = None
) -> list[str | Path]:
    revision = command(["git", "rev-parse", "HEAD"], capture=True).stdout.strip()
    dirty = command(["git", "status", "--porcelain"], capture=True).stdout
    if dirty:
        revision += "-dirty"
    compiler = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    if report is None:
        report = new_report_directory(target, operation)
    return [
        target.binary, operation, f"--target={target.declared_name}", f"--report-dir={report}",
        f"--source-revision={revision}", f"--roc-version={compiler}",
        f"--platform-release={PLATFORM_RELEASE}", f"--platform-sha256={PLATFORM_SHA256}",
        f"--timeout={INPUT_TIMEOUT}",
    ]


def explore(target: Target, corpus: Path, seconds: int, runs: int, seed: int) -> None:
    report_dir = new_report_directory(target, "run")
    command([
        *execution_arguments(target, "run", report_dir), corpus,
        f"--time={seconds}", f"--seed={seed}",
        f"--max-input-size={target.max_input_size}", f"--memory-limit={MEMORY_LIMIT}",
        *([f"--runs={runs}"] if runs else []),
    ], timeout=seconds + SUPERVISOR_MARGIN_SECONDS)
    report = json.loads((report_dir / "report.json").read_text(encoding="utf-8"))
    if (
        report.get("target") != target.declared_name
        or report.get("outcome") != "passed"
        or not report.get("examples")
        or report.get("exploration", {}).get("accepted", 0) <= 0
    ):
        raise FuzzFailure(f"no successful example or exploration evidence for {target.name}: {report_dir}")


def smoke(roc: str, targets: Sequence[Target], runs: int) -> None:
    if runs < 1:
        raise FuzzFailure("smoke run count must be positive")
    build_targets(roc, targets)
    WORK_ROOT.mkdir(parents=True, exist_ok=True)
    for target in targets:
        with tempfile.TemporaryDirectory(prefix=f"smoke-{target.name}-", dir=WORK_ROOT) as temporary:
            corpus = Path(temporary) / "corpus"
            populate_corpus(target, corpus)
            explore(target, corpus, 60, runs, SMOKE_SEED)


def reduce_corpus(target: Target, source: Path, output: Path, seconds: int) -> None:
    source = source.resolve(strict=True)
    output = output.resolve()
    if source == output or source in output.parents or output in source.parents:
        raise FuzzFailure("corpus source and output must be separate directories")
    if output.exists():
        raise FuzzFailure("choose a new output directory to preserve the earlier corpus")
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="reduce-", dir=output.parent) as temporary:
        staging = Path(temporary) / "input"
        reduced = Path(temporary) / "output"
        staging.mkdir()
        for path in sorted(source.iterdir()):
            if path.is_file() and not path.is_symlink():
                payload = path.read_bytes()
                if len(payload) > target.max_input_size:
                    raise FuzzFailure(f"oversize artifact in corpus: {path}")
                write_corpus_entry(staging, payload)
        if not any(staging.iterdir()):
            raise FuzzFailure("cannot reduce a corpus without inputs")
        command(
            [*execution_arguments(target, "reduce-corpus"), staging, reduced, f"--time={seconds}"],
            timeout=seconds + SUPERVISOR_MARGIN_SECONDS,
        )
        reduced.rename(output)


def positive_seconds(value: str) -> int:
    try:
        seconds = int(value)
    except ValueError as error:
        raise argparse.ArgumentTypeError("seconds must be an integer") from error
    if seconds < 1:
        raise argparse.ArgumentTypeError("seconds must be positive")
    return seconds


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    commands = parser.add_subparsers(dest="operation", required=True)
    for operation in ("build", "models", "smoke", "campaign"):
        selection = commands.add_parser(operation)
        selection.add_argument("target", choices=("all", *TARGETS), default="all", nargs="?")
        if operation == "smoke":
            selection.add_argument("--runs", type=int, default=2000)
        if operation == "campaign":
            selection.add_argument("--time", type=positive_seconds, default=60, help="seconds per target")
            selection.add_argument("--seed", type=int, default=SMOKE_SEED)
    for operation in ("reproduce", "minimize", "reduce"):
        selection = commands.add_parser(operation)
        selection.add_argument("target", choices=tuple(TARGETS))
        selection.add_argument("input", type=Path)
        if operation != "reproduce":
            selection.add_argument("output", type=Path)
            selection.add_argument("--time", type=positive_seconds, default=60)

    args = parser.parse_args(argv)
    try:
        targets = selected_targets(args.target)
        if args.operation == "models":
            run_models(args.roc, targets)
        elif args.operation == "smoke":
            smoke(args.roc, targets, args.runs)
        else:
            build_targets(args.roc, targets)
            if args.operation == "campaign":
                for target in targets:
                    corpus = PERSISTENT_CORPUS_ROOT / target.name
                    populate_corpus(target, corpus)
                    explore(target, corpus, args.time, 0, args.seed)
            elif args.operation == "reproduce":
                target = targets[0]
                artifact = args.input.resolve(strict=True)
                payload = artifact.read_bytes()
                print(f"raw-bytes={len(payload)} hex={payload.hex().upper()}")
                replay_deadline = INPUT_TIMEOUT + SUPERVISOR_MARGIN_SECONDS
                command(
                    [target.binary, f"--target={target.declared_name}", "show", artifact],
                    timeout=replay_deadline,
                )
                command(
                    [*execution_arguments(target, "replay"), artifact, "--detailed"],
                    timeout=replay_deadline,
                )
            elif args.operation == "minimize":
                command([
                    *execution_arguments(targets[0], "minimize"),
                    args.input.resolve(strict=True), args.output.resolve(), f"--time={args.time}",
                ], timeout=args.time + SUPERVISOR_MARGIN_SECONDS)
            elif args.operation == "reduce":
                reduce_corpus(targets[0], args.input, args.output, args.time)
    except (FuzzFailure, OSError, ValueError, subprocess.TimeoutExpired) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
