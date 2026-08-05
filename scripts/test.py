#!/usr/bin/env python3
"""Build once and run roc-unicode's data-driven test suites."""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import os
import platform
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Sequence

from unicode_data import (
    DataError,
    GraphemeCase,
    MAX_CODE_POINT,
    RangeRecord,
    load_manifest,
    load_property_data,
    parse_grapheme_tests,
    validate_all,
)


ROOT = Path(__file__).resolve().parents[1]
TEST_TMP = ROOT / ".roc-unicode-tmp" / "tests"
EXAMPLE_SPEC = ROOT / "examples" / "spec.json"
APP_ROOT = ROOT / "tests" / "apps"
APP_NAMES = {"grapheme": "grapheme", "properties": "properties", "allocation": "allocation"}
GCB_CODES = {
    "CR": 1,
    "LF": 2,
    "Control": 3,
    "Extend": 4,
    "ZWJ": 5,
    "Regional_Indicator": 6,
    "Prepend": 7,
    "SpacingMark": 8,
    "L": 9,
    "V": 10,
    "T": 11,
    "LV": 12,
    "LVT": 13,
}
EAW_CODES = {"N": 0, "A": 1, "F": 2, "H": 3, "Na": 4, "W": 5}
EMOJI_BITS = {
    "Emoji": 1,
    "Emoji_Presentation": 2,
    "Emoji_Modifier": 4,
    "Emoji_Modifier_Base": 8,
    "Emoji_Component": 16,
    "Extended_Pictographic": 32,
}
VALID_SCALAR_COUNT = MAX_CODE_POINT + 1 - 0x800


class TestFailure(RuntimeError):
    pass


def configure_output() -> None:
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(encoding="utf-8", errors="replace")


def command(
    args: Sequence[str],
    *,
    cwd: Path = ROOT,
    input_text: str | None = None,
    timeout: int = 300,
) -> subprocess.CompletedProcess[str]:
    print("+", " ".join(str(arg) for arg in args))
    completed = subprocess.run(
        list(args),
        cwd=cwd,
        input=input_text,
        text=True,
        encoding="utf-8",
        errors="replace",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=timeout,
    )
    if completed.returncode != 0:
        detail = (completed.stderr or completed.stdout).strip()
        raise TestFailure(
            f"command exited {completed.returncode}: {' '.join(args)}"
            + (f"\n{detail}" if detail else "")
        )
    return completed


def default_jobs() -> int:
    configured = os.environ.get("ROC_UNICODE_JOBS")
    if configured:
        try:
            jobs = int(configured)
        except ValueError as err:
            raise TestFailure("ROC_UNICODE_JOBS must be an integer") from err
        if jobs < 1:
            raise TestFailure("ROC_UNICODE_JOBS must be positive")
        return jobs
    return min(os.cpu_count() or 1, 8)


def executable_suffix() -> str:
    return ".exe" if os.name == "nt" else ""


def load_app_specs() -> dict[str, dict[str, object]]:
    expected = set(APP_NAMES)
    directories = {path.name: path for path in APP_ROOT.iterdir() if path.is_dir()}
    if set(directories) != expected:
        raise TestFailure(
            f"test app discovery drift: missing={sorted(expected - set(directories))}, "
            f"stale={sorted(set(directories) - expected)}"
        )
    specs: dict[str, dict[str, object]] = {}
    required_fields = {
        "grapheme": {
            "schema_version",
            "kind",
            "suite",
            "timeout_seconds",
            "unicode_manifest_file",
        },
        "properties": {
            "schema_version",
            "kind",
            "suite",
            "timeout_seconds",
            "unicode_manifest_files",
            "scalar_domain",
        },
        "allocation": {
            "schema_version",
            "kind",
            "suites",
            "timeout_seconds",
            "baseline_file",
            "exact_baseline_target",
        },
    }
    for name, directory in sorted(directories.items()):
        source = directory / "main.roc"
        spec_path = directory / "spec.json"
        if not source.is_file() or not spec_path.is_file():
            raise TestFailure(f"{name}: each test app needs adjacent main.roc and spec.json")
        try:
            spec = json.loads(spec_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as err:
            raise TestFailure(f"unable to read {spec_path}: {err}") from err
        if not isinstance(spec, dict) or set(spec) != required_fields[name]:
            raise TestFailure(f"{name}: spec fields must be exactly {sorted(required_fields[name])}")
        if spec.get("schema_version") != 1 or spec.get("kind") != name:
            raise TestFailure(f"{name}: adjacent spec schema/kind mismatch")
        if not isinstance(spec.get("timeout_seconds"), int) or spec["timeout_seconds"] < 1:
            raise TestFailure(f"{name}: timeout_seconds must be positive")
        specs[name] = spec
    manifest_files = set(load_manifest()["files"])
    if specs["grapheme"]["suite"] != "grapheme":
        raise TestFailure("grapheme spec suite has drifted")
    if specs["grapheme"]["unicode_manifest_file"] != "grapheme_break_test":
        raise TestFailure("grapheme spec references an unknown manifest file")
    property_sources = set(specs["properties"]["unicode_manifest_files"])
    if property_sources != {
        "grapheme_break_property",
        "east_asian_width",
        "emoji_data",
    } or not property_sources <= manifest_files:
        raise TestFailure("properties spec must cover all three production property files")
    if specs["properties"]["scalar_domain"] != "valid-unicode-scalars":
        raise TestFailure("properties spec must cover the valid Unicode scalar domain")
    allocation_baseline = ROOT / str(specs["allocation"]["baseline_file"])
    if allocation_baseline.parent != APP_ROOT / "allocation" or not allocation_baseline.is_file():
        raise TestFailure("allocation baselines must be adjacent to their app")
    return specs


def build_platform(zig: str) -> None:
    command(
        [
            zig,
            "build",
            "--build-file",
            "tests/platform/build.zig",
            "native",
            "-Doptimize=ReleaseFast",
        ]
    )


def build_apps(roc: str, names: Sequence[str], zig: str, *, skip_build: bool) -> dict[str, Path]:
    TEST_TMP.mkdir(parents=True, exist_ok=True)
    paths = {
        name: TEST_TMP / f"{APP_NAMES[name]}{executable_suffix()}"
        for name in names
    }
    if skip_build:
        missing = [str(path) for path in paths.values() if not path.exists()]
        if missing:
            raise TestFailure(f"--skip-build requested but binaries are missing: {missing}")
        return paths
    build_platform(zig)
    for name in names:
        source = APP_ROOT / name / "main.roc"
        command([roc, "check", str(source), "--no-cache"])
        command(
            [
                roc,
                "build",
                str(source),
                "--opt=speed",
                f"--output={paths[name]}",
                "--no-cache",
            ]
        )
    return paths


@dataclass(frozen=True)
class Shard:
    index: int
    start: int
    end: int

def make_shards(count: int, jobs: int) -> list[Shard]:
    if count == 0:
        return []
    shard_count = min(count, jobs)
    base, extra = divmod(count, shard_count)
    result = []
    start = 0
    for index in range(shard_count):
        length = base + (1 if index < extra else 0)
        result.append(Shard(index, start, start + length))
        start += length
    return result


def protocol_payload(suite: str, rows: Sequence[str]) -> bytes:
    text = f"ROC_UNICODE_TEST_V1\t{suite}\t{len(rows)}\n" + "\n".join(rows) + "\n"
    return text.encode("utf-8")


def invoke_app(binary: Path, suite: str, rows: Sequence[str], timeout: int) -> str | None:
    try:
        completed = subprocess.run(
            [str(binary)],
            cwd=ROOT,
            input=protocol_payload(suite, rows),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return f"timed out after {timeout}s"
    expected = f"PASS\t{suite}\t{len(rows)}"
    stdout = completed.stdout.decode("utf-8", errors="replace").strip()
    stderr = completed.stderr.decode("utf-8", errors="replace").strip()
    if completed.returncode == 0 and stdout == expected:
        return None
    details = []
    if completed.returncode != 0:
        details.append(f"exit={completed.returncode}")
    if stdout:
        details.append(f"stdout={stdout!r}")
    if stderr:
        details.append(f"stderr={stderr!r}")
    if not details:
        details.append("empty or malformed response")
    return ", ".join(details)


def isolate_failure(binary: Path, suite: str, rows: Sequence[str], timeout: int) -> str:
    error = invoke_app(binary, suite, rows, timeout)
    if error is None:
        return "failure was not reproducible"
    if len(rows) == 1:
        case_id = rows[0].split("\t", 1)[0]
        return f"{case_id}: {error}"
    midpoint = len(rows) // 2
    left = rows[:midpoint]
    right = rows[midpoint:]
    if invoke_app(binary, suite, left, timeout) is not None:
        return isolate_failure(binary, suite, left, timeout)
    if invoke_app(binary, suite, right, timeout) is not None:
        return isolate_failure(binary, suite, right, timeout)
    return f"shard-only failure affecting {len(rows)} cases: {error}"


def run_parallel_suite(
    binary: Path,
    suite: str,
    count: int,
    row_at: Callable[[int], str],
    *,
    jobs: int,
    timeout: int = 120,
) -> None:
    shards = make_shards(count, jobs)

    def run(shard: Shard) -> tuple[int, str | None]:
        rows = [row_at(index) for index in range(shard.start, shard.end)]
        error = invoke_app(binary, suite, rows, timeout)
        if error is not None:
            error = isolate_failure(binary, suite, rows, timeout)
        return shard.index, error

    results: list[tuple[int, str | None]] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as executor:
        futures = [executor.submit(run, shard) for shard in shards]
        for future in concurrent.futures.as_completed(futures):
            results.append(future.result())
    failures = []
    for index, error in sorted(results):
        if error is None:
            print(f"PASS {suite} shard {index + 1}/{len(shards)}")
        else:
            failures.append(f"shard {index + 1}/{len(shards)}: {error}")
    if failures:
        raise TestFailure(f"{suite} failed:\n" + "\n".join(failures))
    print(f"PASS {suite}: {count} cases")


def grapheme_row(case: GraphemeCase) -> str:
    code_points = ",".join(f"{code_point:04X}" for code_point in case.code_points)
    offsets = ",".join(str(offset) for offset in case.break_offsets)
    return f"{case.case_id}\t{code_points}\t{offsets}"


def run_grapheme(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    manifest = load_manifest()
    cases = parse_grapheme_tests(manifest)
    run_parallel_suite(
        binary,
        "grapheme",
        len(cases),
        lambda index: grapheme_row(cases[index]),
        jobs=jobs,
        timeout=int(spec["timeout_seconds"]),
    )


def fill_property_table(records: list[RangeRecord], codes: dict[str, int], default: int) -> bytearray:
    values = bytearray([default]) * (MAX_CODE_POINT + 1)
    for record in records:
        values[record.start : record.end + 1] = bytes([codes[record.property]]) * (
            record.end - record.start + 1
        )
    return values


def build_property_tables() -> tuple[str, bytearray, bytearray, bytearray]:
    manifest = load_manifest()
    gcb, eaw, emoji, _incb = load_property_data(manifest)
    gcb_values = fill_property_table(gcb, GCB_CODES, 0)
    eaw_values = fill_property_table(eaw, EAW_CODES, 0)
    emoji_values = bytearray(MAX_CODE_POINT + 1)
    for record in emoji:
        bit = EMOJI_BITS[record.property]
        for code_point in range(record.start, record.end + 1):
            emoji_values[code_point] |= bit
    return str(manifest["unicode_version"]), gcb_values, eaw_values, emoji_values


def scalar_at(index: int) -> int:
    return index if index < 0xD800 else index + 0x800


def run_properties(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    if spec["suite"] != "properties":
        raise TestFailure("properties spec suite has drifted")
    version, gcb, eaw, emoji = build_property_tables()

    def row(index: int) -> str:
        code_point = scalar_at(index)
        return (
            f"{version}:scalar:{code_point:06X}\t{code_point:X}\t"
            f"{gcb[code_point]}\t{eaw[code_point]}\t{emoji[code_point]}"
        )

    run_parallel_suite(
        binary,
        str(spec["suite"]),
        VALID_SCALAR_COUNT,
        row,
        jobs=jobs,
        timeout=int(spec["timeout_seconds"]),
    )


def utf8_hex(value: str) -> str:
    return ",".join(f"{byte:02X}" for byte in value.encode("utf-8"))


ALLOCATION_FIXTURES = {
    "empty": "",
    "ascii": "hello",
    "combining": "a\u0308",
    "regional-indicator": "🇦🇺",
    "emoji-zwj": "👩‍🚀",
    "long": "abc" * 512,
}


def run_serial_suite(binary: Path, suite: str, rows: Sequence[str], timeout: int = 120) -> None:
    error = invoke_app(binary, suite, rows, timeout)
    if error is not None:
        raise TestFailure(isolate_failure(binary, suite, rows, timeout))
    print(f"PASS {suite}: {len(rows)} cases")


def run_allocations(binary: Path, spec: dict[str, object]) -> None:
    calibration = [
        f"zero\t\tzero",
        f"positive\t{utf8_hex('allocation calibration input')}\tpositive",
    ]
    timeout = int(spec["timeout_seconds"])
    if spec["exact_baseline_target"] != "linux-x64":
        raise TestFailure("allocation spec exact baseline target has drifted")
    suites = spec["suites"]
    if suites != ["allocation-calibration", "allocation-baselines"]:
        raise TestFailure("allocation spec suites have drifted")
    run_serial_suite(binary, suites[0], calibration, timeout=timeout)

    machine = platform.machine().lower()
    if platform.system() != "Linux" or machine not in ("x86_64", "amd64"):
        print("SKIP allocation-baselines: exact baselines are Linux x64 only")
        return
    try:
        baseline_path = ROOT / str(spec["baseline_file"])
        baseline = json.loads(baseline_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as err:
        raise TestFailure(f"unable to read allocation baselines: {err}") from err
    pinned_roc = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    if (
        baseline.get("schema_version") != 1
        or baseline.get("platform") != "roc-platform-template-zig-1.1.0+alloc-count"
        or baseline.get("target") != "x64musl"
        or baseline.get("optimize") != "speed"
    ):
        raise TestFailure("allocation baseline metadata has drifted")
    if baseline.get("roc_version") != pinned_roc:
        raise TestFailure("allocation baseline Roc version does not match .roc-version")
    expected = baseline.get("fixtures")
    if not isinstance(expected, dict) or set(expected) != set(ALLOCATION_FIXTURES):
        raise TestFailure("allocation baseline fixture set has drifted")
    rows = [
        f"{name}\t{utf8_hex(value)}\t{expected[name]}"
        for name, value in ALLOCATION_FIXTURES.items()
    ]
    run_serial_suite(binary, suites[1], rows, timeout=timeout)


def verify_pinned_roc(roc: str) -> None:
    completed = command([roc, "version"])
    pinned = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    if pinned not in completed.stdout:
        raise TestFailure(
            f"exact allocation baselines require {pinned}, got {completed.stdout.strip()!r}"
        )


def load_example_spec() -> dict[str, dict[str, object]]:
    try:
        raw = json.loads(EXAMPLE_SPEC.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as err:
        raise TestFailure(f"unable to read {EXAMPLE_SPEC}: {err}") from err
    if raw.get("schema_version") != 1 or not isinstance(raw.get("examples"), dict):
        raise TestFailure("invalid example test spec schema")
    return raw["examples"]


def run_examples(roc: str, examples_dir: Path) -> None:
    spec = load_example_spec()
    sources = {path.name: path for path in sorted(examples_dir.glob("*.roc"))}
    if set(spec) != set(sources):
        raise TestFailure(
            f"example spec drift: missing={sorted(set(sources) - set(spec))}, "
            f"stale={sorted(set(spec) - set(sources))}"
        )
    build_dir = TEST_TMP / "examples"
    build_dir.mkdir(parents=True, exist_ok=True)
    for name, source in sources.items():
        item = spec[name]
        required = {"args", "stdin", "exit_code", "stdout", "stderr", "test"}
        if set(item) != required:
            raise TestFailure(f"{name}: spec fields must be exactly {sorted(required)}")
        if (
            not isinstance(item["args"], list)
            or not all(isinstance(arg, str) for arg in item["args"])
            or not all(isinstance(item[field], str) for field in ("stdin", "stdout", "stderr"))
            or not isinstance(item["exit_code"], int)
            or not isinstance(item["test"], bool)
        ):
            raise TestFailure(f"{name}: invalid example spec field type")
        command([roc, "check", str(source), "--no-cache"])
        if item["test"]:
            command([roc, "test", str(source), "--no-cache"])
        output = build_dir / f"{Path(name).stem}{executable_suffix()}"
        command(
            [roc, "build", str(source), "--opt=speed", f"--output={output}", "--no-cache"]
        )
        completed = subprocess.run(
            [str(output), *item["args"]],
            cwd=source.parent,
            input=item["stdin"],
            text=True,
            encoding="utf-8",
            errors="replace",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=30,
        )
        for field, got in (
            ("exit_code", completed.returncode),
            ("stdout", completed.stdout),
            ("stderr", completed.stderr),
        ):
            if got != item[field]:
                raise TestFailure(f"{name}: {field} expected {item[field]!r}, got {got!r}")
        print(f"PASS example {name}")


def run_data_checks() -> None:
    validate_all(load_manifest())
    command([sys.executable, "scripts/unicode_data.py", "generate", "--check"])
    command([sys.executable, "-m", "unittest", "scripts/test_unicode_data.py"])


def main(argv: list[str] | None = None) -> int:
    configure_output()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "suite",
        nargs="?",
        choices=("all", "data", "grapheme", "properties", "allocations", "examples"),
        default="all",
    )
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default=os.environ.get("ZIG", "zig"))
    parser.add_argument("--jobs", type=int, default=default_jobs())
    parser.add_argument("--skip-build", action="store_true")
    parser.add_argument("--examples-dir", type=Path, default=ROOT / "examples")
    args = parser.parse_args(argv)
    if args.jobs < 1:
        parser.error("--jobs must be positive")
    try:
        app_specs = load_app_specs()
        if args.suite in ("all", "data"):
            run_data_checks()
        if args.suite in ("all", "examples"):
            run_examples(args.roc, args.examples_dir.resolve())
        requested_apps = []
        if args.suite in ("all", "grapheme"):
            requested_apps.append("grapheme")
        if args.suite in ("all", "properties"):
            requested_apps.append("properties")
        if args.suite in ("all", "allocations"):
            requested_apps.append("allocation")
        if requested_apps:
            if (
                "allocation" in requested_apps
                and platform.system() == "Linux"
                and platform.machine().lower() in ("x86_64", "amd64")
            ):
                verify_pinned_roc(args.roc)
            binaries = build_apps(
                args.roc, requested_apps, args.zig, skip_build=args.skip_build
            )
            if "grapheme" in requested_apps:
                run_grapheme(binaries["grapheme"], args.jobs, app_specs["grapheme"])
            if "properties" in requested_apps:
                run_properties(binaries["properties"], args.jobs, app_specs["properties"])
            if "allocation" in requested_apps:
                run_allocations(binaries["allocation"], app_specs["allocation"])
    except (DataError, TestFailure, subprocess.TimeoutExpired) as err:
        print(f"error: {err}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
