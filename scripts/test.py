#!/usr/bin/env python3
"""Build once and run roc-unicode's data-driven test suites."""

from __future__ import annotations

import argparse
from array import array
import concurrent.futures
import json
import os
import platform
import random
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable, Sequence

import unicode_data

from unicode_data import (
    BidiCharacterCase,
    BidiTestCase,
    DataError,
    GraphemeCase,
    LineBreakCase,
    WordBreakCase,
    MAX_CODE_POINT,
    MissingDefault,
    RangeRecord,
    load_manifest,
    load_canonical_properties,
    load_property_data,
    load_public_properties,
    parse_grapheme_tests,
    parse_bidi_character_tests,
    parse_bidi_tests,
    parse_line_break_tests,
    parse_word_break_tests,
    release_version,
    validate_all,
)
from bidi_reduce import capture, minimize


ROOT = Path(__file__).resolve().parents[1]
TEST_TMP = ROOT / ".roc-unicode-tmp" / "tests"
APP_ROOT = ROOT / "tests" / "apps"
APP_NAMES = {
    "bidi": "bidi",
    "grapheme": "grapheme",
    "line-break": "line-break",
    "word": "word",
    "case": "case",
    "properties": "properties",
    "allocation": "allocation",
}
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
BIDI_CODES = {
    "L": 0,
    "AL": 1,
    "AN": 2,
    "B": 3,
    "BN": 4,
    "CS": 5,
    "EN": 6,
    "ES": 7,
    "ET": 8,
    "FSI": 9,
    "LRE": 10,
    "LRI": 11,
    "LRO": 12,
    "NSM": 13,
    "ON": 14,
    "PDF": 15,
    "PDI": 16,
    "R": 17,
    "RLE": 18,
    "RLI": 19,
    "RLO": 20,
    "S": 21,
    "WS": 22,
}
BIDI_ABSENT_SCALAR = 0xFFFFFFFF
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
        "line-break": {
            "schema_version",
            "kind",
            "suite",
            "timeout_seconds",
            "unicode_manifest_file",
        },
        "word": {
            "schema_version",
            "kind",
            "suite",
            "timeout_seconds",
            "unicode_manifest_file",
        },
        "case": {
            "schema_version",
            "kind",
            "suite",
            "timeout_seconds",
            "unicode_manifest_files",
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
        "bidi": {
            "schema_version",
            "kind",
            "suites",
            "timeout_seconds",
            "unicode_manifest_files",
            "bidi_test_executions",
            "bidi_character_test_executions",
            "metamorphic_seed",
            "metamorphic_cases",
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
    manifest_files = set(load_manifest()["sources"])
    if specs["grapheme"]["suite"] != "grapheme":
        raise TestFailure("grapheme spec suite has drifted")
    if specs["grapheme"]["unicode_manifest_file"] != "grapheme_break_test":
        raise TestFailure("grapheme spec references an unknown manifest file")
    if specs["line-break"]["suite"] != "line-break":
        raise TestFailure("line-break spec suite has drifted")
    if specs["line-break"]["unicode_manifest_file"] != "line_break_test":
        raise TestFailure("line-break spec references an unknown manifest file")
    bidi_sources = set(specs["bidi"]["unicode_manifest_files"])
    if bidi_sources != {"bidi_test", "bidi_character_test"} or not bidi_sources <= manifest_files:
        raise TestFailure("bidi spec must cover both official Unicode bidi conformance files")
    if specs["bidi"]["suites"] != ["bidi-test", "bidi-character-test", "bidi-metamorphic"]:
        raise TestFailure("bidi spec suites have drifted")
    for field in ("bidi_test_executions", "bidi_character_test_executions", "metamorphic_seed", "metamorphic_cases"):
        if not isinstance(specs["bidi"][field], int) or specs["bidi"][field] < 1:
            raise TestFailure(f"bidi spec {field} must be positive")
    if specs["word"]["suite"] != "word":
        raise TestFailure("word spec suite has drifted")
    if specs["word"]["unicode_manifest_file"] != "word_break_test":
        raise TestFailure("word spec references an unknown manifest file")
    case_sources = set(specs["case"]["unicode_manifest_files"])
    if specs["case"]["suite"] != "case" or case_sources != {
        "unicode_data",
        "special_casing",
        "case_folding",
        "derived_core_properties",
        "prop_list",
        "derived_combining_class",
    } or not case_sources <= manifest_files:
        raise TestFailure("case spec must name the complete Case data source graph")
    property_sources = set(specs["properties"]["unicode_manifest_files"])
    if property_sources != {
        "grapheme_break_property",
        "east_asian_width",
        "emoji_data",
        "derived_bidi_class",
        "unicode_data",
        "bidi_mirroring",
        "bidi_brackets",
    } or not property_sources <= manifest_files:
        raise TestFailure("properties spec must cover every production property view")
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
        if suite == "bidi-metamorphic":
            fields = rows[0].split("\t")
            values = fields[1].split(",")
            signature = error.split(";", 1)[0]

            def reproduces(candidate: list[str]) -> bool:
                candidate_row = "\t".join([fields[0], ",".join(candidate), fields[2]])
                retry = invoke_app(binary, suite, [candidate_row], timeout)
                return retry is not None and retry.split(";", 1)[0] == signature

            reduced = minimize(values, reproduces)
            reduced_row = "\t".join([fields[0], ",".join(reduced), fields[2]])
            path = capture(f"bidi-metamorphic-{case_id.rsplit(':', 1)[-1]}.tsv", f"# import with suite bidi-metamorphic\n{reduced_row}\n")
            return f"{case_id}: {error}; minimized={reduced_row!r}; regression-artifact={path.relative_to(ROOT)}"
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


def run_parallel_stream(
    binary: Path,
    suite: str,
    rows: Iterable[str],
    expected_count: int,
    *,
    jobs: int,
    timeout: int = 120,
    batch_size: int = 8192,
) -> None:
    """Run a large protocol suite without retaining every serialized row."""
    if expected_count < 1 or batch_size < 1:
        raise TestFailure("parallel stream count and batch size must be positive")

    def run(index: int, batch: list[str]) -> tuple[int, int, str | None]:
        error = invoke_app(binary, suite, batch, timeout)
        if error is not None:
            error = isolate_failure(binary, suite, batch, timeout)
        return index, len(batch), error

    submitted = 0
    results: list[tuple[int, int, str | None]] = []
    in_flight: set[concurrent.futures.Future[tuple[int, int, str | None]]] = set()
    batch_count = 0

    def collect_completed(wait_for_all: bool) -> None:
        if not in_flight:
            return
        done, _ = concurrent.futures.wait(
            in_flight,
            return_when=(
                concurrent.futures.ALL_COMPLETED
                if wait_for_all
                else concurrent.futures.FIRST_COMPLETED
            ),
        )
        for future in done:
            in_flight.remove(future)
            results.append(future.result())

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as executor:
        batch: list[str] = []
        batch_index = 0
        for row in rows:
            batch.append(row)
            if len(batch) == batch_size:
                in_flight.add(executor.submit(run, batch_index, batch))
                submitted += len(batch)
                batch = []
                batch_index += 1
                batch_count += 1
                if len(in_flight) >= jobs * 2:
                    collect_completed(False)
        if batch:
            in_flight.add(executor.submit(run, batch_index, batch))
            submitted += len(batch)
            batch_count += 1
        if submitted != expected_count:
            raise TestFailure(
                f"{suite} parser execution-count drift: expected {expected_count}, got {submitted}"
            )
        collect_completed(True)

    failures = []
    for index, size, error in sorted(results):
        if error is None:
            print(f"PASS {suite} batch {index + 1}/{batch_count} ({size} cases)")
        else:
            failures.append(f"batch {index + 1}/{batch_count}: {error}")
    if failures:
        raise TestFailure(f"{suite} failed:\n" + "\n".join(failures))
    print(f"PASS {suite}: {submitted} cases")


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


def line_break_row(case: LineBreakCase) -> str:
    code_points = ",".join(f"{code_point:04X}" for code_point in case.code_points)
    offsets = ",".join(str(offset) for offset in case.break_offsets)
    return f"17.0.0:LineBreakTest.txt:{case.line}\t{code_points}\t{offsets}"


def run_line_break(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    manifest = load_manifest()
    cases = parse_line_break_tests(manifest)
    run_parallel_suite(
        binary,
        "line-break",
        len(cases),
        lambda index: line_break_row(cases[index]),
        jobs=jobs,
        timeout=int(spec["timeout_seconds"]),
    )


def bidi_levels(levels: tuple[int | None, ...]) -> str:
    return ",".join("x" if level is None else str(level) for level in levels)


def bidi_reorder(reorder: tuple[int, ...]) -> str:
    return ",".join(str(index) for index in reorder) if reorder else "-"


def bidi_test_rows(cases: Iterable[BidiTestCase], version: str) -> Iterable[str]:
    for case in cases:
        classes = ",".join(case.classes)
        levels = bidi_levels(case.levels)
        reorder = bidi_reorder(case.reorder)
        for mode in case.paragraph_modes:
            yield f"{version}:BidiTest.txt:{case.line}:mode-{mode}\t{classes}\t{mode}\t{levels}\t{reorder}"


def bidi_character_rows(cases: Iterable[BidiCharacterCase], version: str) -> Iterable[str]:
    for case in cases:
        code_points = ",".join(f"{code_point:04X}" for code_point in case.code_points)
        yield (
            f"{version}:BidiCharacterTest.txt:{case.line}\t{code_points}\t"
            f"{case.paragraph_mode}\t{case.paragraph_level}\t"
            f"{bidi_levels(case.levels)}\t{bidi_reorder(case.reorder)}"
        )


BIDI_METAMORPHIC_SCALARS = (
    0x0061,  # L
    0x05D0,  # R
    0x0627,  # AL
    0x0030,  # EN
    0x0660,  # AN
    0x002B,  # ES
    0x0024,  # ET
    0x002C,  # CS
    0x0300,  # NSM
    0x200C,  # ZWNJ
    0x200D,  # ZWJ
    0x000D,  # CR: P1 boundary coverage
    0x000A,  # LF: P1 boundary coverage
    0x2029,  # B: P1 boundary coverage
    0x0009,  # S
    0x0020,  # WS
    0x0021,  # ON
    0x202A,  # LRE
    0x202B,  # RLE
    0x202C,  # PDF
    0x202D,  # LRO
    0x202E,  # RLO
    0x2066,  # LRI
    0x2067,  # RLI
    0x2068,  # FSI
    0x2069,  # PDI
    0x200B,  # BN
    0x0028,  # paired bracket
    0x0029,
    0x3008,
    0x3009,
    0x2201,  # Bidi_Mirrored without Bidi_Mirroring_Glyph
    0xE000,  # private-use scalar
    0xFDD0,  # noncharacter
    0x1E8C5,  # unassigned scalar in a DerivedBidiClass R default range
    0x1F600,  # supplementary scalar
)


def bidi_metamorphic_rows(seed: int, count: int, version: str) -> list[str]:
    """Deterministic malformed-and-well-nested-control invariant coverage."""
    generator = random.Random(seed)
    rows = []
    for index in range(count):
        length = generator.randrange(1, 97)
        code_points = [generator.choice(BIDI_METAMORPHIC_SCALARS) for _ in range(length)]
        code_points_text = ",".join(f"{code_point:04X}" for code_point in code_points)
        # BidiTest's compact encoding: 0 Auto, 1 explicit LTR, 2 explicit RTL.
        mode = generator.randrange(3)
        rows.append(f"{version}:metamorphic:seed-{seed}:case-{index}\t{code_points_text}\t{mode}")
    return rows


def run_bidi(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    manifest = load_manifest()
    version = release_version(manifest, "unicode")
    timeout = int(spec["timeout_seconds"])
    run_parallel_stream(
        binary,
        "bidi-test",
        bidi_test_rows(parse_bidi_tests(manifest), version),
        int(spec["bidi_test_executions"]),
        jobs=jobs,
        timeout=timeout,
    )
    metamorphic_rows = bidi_metamorphic_rows(
        int(spec["metamorphic_seed"]), int(spec["metamorphic_cases"]), version
    )
    run_parallel_suite(
        binary,
        "bidi-metamorphic",
        len(metamorphic_rows),
        lambda index: metamorphic_rows[index],
        jobs=jobs,
        timeout=timeout,
    )
    run_parallel_stream(
        binary,
        "bidi-character-test",
        bidi_character_rows(parse_bidi_character_tests(manifest), version),
        int(spec["bidi_character_test_executions"]),
        jobs=jobs,
        timeout=timeout,
    )


def word_row(case: WordBreakCase) -> str:
    code_points = ",".join(f"{code_point:04X}" for code_point in case.code_points)
    offsets = ",".join(str(offset) for offset in case.break_offsets)
    return f"{case.case_id}\t{code_points}\t{offsets}"


def run_word(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    manifest = load_manifest()
    cases = parse_word_break_tests(manifest)
    run_parallel_suite(
        binary,
        "word",
        len(cases),
        lambda index: word_row(cases[index]),
        jobs=jobs,
        timeout=int(spec["timeout_seconds"]),
    )


FOLD_PROFILES = ("full", "simple", "turkic-full", "turkic-simple")


def _range_contains(records: Sequence[RangeRecord], code_point: int) -> bool:
    return any(record.start <= code_point <= record.end for record in records)


def _range_value(
    records: Sequence[RangeRecord], code_point: int, default: int
) -> int:
    for record in records:
        if record.start <= code_point <= record.end:
            return int(record.property)
    return default


def _case_maps(case: unicode_data.CaseData) -> dict[str, dict[int, int]]:
    return {
        "lower": {item.source: item.target for item in case.simple_lower},
        "upper": {item.source: item.target for item in case.simple_upper},
        "title": {item.source: item.target for item in case.simple_title},
    }


def _languages_match(languages: Sequence[str], profile: str) -> bool:
    if not languages:
        return True
    return (
        profile == "turkic" and any(language in ("az", "tr") for language in languages)
    ) or (profile == "lithuanian" and "lt" in languages)


def _left_context(
    source: Sequence[int], stop: int, case: unicode_data.CaseData,
    canonical: unicode_data.CanonicalProperties,
) -> tuple[bool, bool, bool]:
    before_final_sigma = False
    after_soft_dotted = False
    after_i = False
    for scalar in source[:stop]:
        case_ignorable = _range_contains(case.case_ignorable, scalar)
        ccc = _range_value(
            canonical.canonical_combining_class,
            scalar,
            canonical.canonical_combining_class_default,
        )
        if not case_ignorable:
            before_final_sigma = _range_contains(case.cased, scalar)
        if _range_contains(case.soft_dotted, scalar):
            after_soft_dotted = True
        elif ccc == 0 or ccc == 230:
            after_soft_dotted = False
        if scalar == 0x49:
            after_i = True
        elif ccc == 0 or ccc == 230:
            after_i = False
    return before_final_sigma, after_soft_dotted, after_i


def _more_above(
    source: Sequence[int], start: int, canonical: unicode_data.CanonicalProperties
) -> bool:
    for scalar in source[start:]:
        ccc = _range_value(
            canonical.canonical_combining_class,
            scalar,
            canonical.canonical_combining_class_default,
        )
        if ccc == 230:
            return True
        if ccc == 0:
            return False
    return False


def _before_dot(
    source: Sequence[int], start: int, canonical: unicode_data.CanonicalProperties
) -> bool:
    for scalar in source[start:]:
        if scalar == 0x307:
            return True
        ccc = _range_value(
            canonical.canonical_combining_class,
            scalar,
            canonical.canonical_combining_class_default,
        )
        if ccc == 0 or ccc == 230:
            return False
    return False


def _contexts_match(
    contexts: Sequence[str], source: Sequence[int], index: int,
    case: unicode_data.CaseData, canonical: unicode_data.CanonicalProperties,
) -> bool:
    before_final_sigma, after_soft_dotted, after_i = _left_context(source, index, case, canonical)
    for context in contexts:
        if context == "Final_Sigma":
            following_cased = next(
                (
                    _range_contains(case.cased, scalar)
                    for scalar in source[index + 1:]
                    if not _range_contains(case.case_ignorable, scalar)
                ),
                False,
            )
            matched = before_final_sigma and not following_cased
        elif context == "After_Soft_Dotted":
            matched = after_soft_dotted
        elif context == "More_Above":
            matched = _more_above(source, index + 1, canonical)
        elif context == "Before_Dot":
            matched = _before_dot(source, index + 1, canonical)
        elif context == "After_I":
            matched = after_i
        elif context == "Not_Before_Dot":
            matched = not _before_dot(source, index + 1, canonical)
        else:  # The strict generator parser makes new conditions an explicit update.
            raise TestFailure(f"unsupported SpecialCasing context {context!r}")
        if not matched:
            return False
    return True


def _case_mapping(
    operation: str, profile: str, source: Sequence[int], index: int,
    case: unicode_data.CaseData, canonical: unicode_data.CanonicalProperties,
    maps: dict[str, dict[int, int]],
) -> tuple[tuple[int, ...], bool]:
    scalar = source[index]
    chosen: unicode_data.SpecialCaseMapping | None = None
    chosen_specificity = -1
    for special in case.special:
        if special.source != scalar:
            continue
        if not _languages_match(special.languages, profile):
            continue
        if not _contexts_match(special.contexts, source, index, case, canonical):
            continue
        specificity = len(special.languages) + len(special.contexts)
        if specificity > chosen_specificity:
            chosen = special
            chosen_specificity = specificity
    if chosen is not None:
        return getattr(chosen, operation), bool(chosen.contexts)
    target = maps[operation].get(scalar)
    return ((scalar,) if target is None else (target,)), False


def _case_oracle(
    operation: str, profile: str, source: Sequence[int], case: unicode_data.CaseData,
    canonical: unicode_data.CanonicalProperties, maps: dict[str, dict[int, int]],
) -> tuple[tuple[tuple[int, ...], ...], tuple[bool, ...]]:
    if operation == "title":
        # Generated SpecialCasing witnesses contain one unbroken word segment.
        # Model R3 directly: the first cased scalar title-maps, every later
        # scalar lower-maps, and an uncased leading prefix stays unchanged.
        after_first_cased = False
        mapped: list[tuple[tuple[int, ...], bool]] = []
        for index, scalar in enumerate(source):
            if not after_first_cased and not _range_contains(case.cased, scalar):
                mapped.append(((scalar,), True))
            else:
                selected_operation = "lower" if after_first_cased else "title"
                mapped.append(_case_mapping(
                    selected_operation, profile, source, index, case, canonical, maps
                ))
                after_first_cased = True
        return tuple(item[0] for item in mapped), (True,) * len(mapped)
    mapped = [_case_mapping(operation, profile, source, index, case, canonical, maps) for index in range(len(source))]
    return tuple(item[0] for item in mapped), tuple(item[1] for item in mapped)


def _fold_mapping(
    profile: str, scalar: int, folds: dict[tuple[int, str], tuple[int, ...]]
) -> tuple[int, ...]:
    common = folds.get((scalar, "C"))
    full = folds.get((scalar, "F"))
    simple = folds.get((scalar, "S"))
    turkic = folds.get((scalar, "T"))
    if profile in ("turkic-full", "turkic-simple") and turkic is not None:
        return turkic
    if profile in ("full", "turkic-full"):
        return full or common or (scalar,)
    return simple or common or (scalar,)


def _encode_scalars(values: Sequence[int]) -> str:
    return ",".join(f"{value:X}" for value in values) if values else "_"


def _encode_case_row(
    case_id: str, operation: str, profile: str, source: Sequence[int],
    mappings: Sequence[Sequence[int]], contextual: Sequence[bool],
) -> str:
    if len(source) != len(mappings) or len(source) != len(contextual):
        raise TestFailure(f"{case_id}: oracle fact count drifted")
    return "\t".join((
        case_id,
        operation,
        profile,
        _encode_scalars(source),
        "/".join(_encode_scalars(mapping) for mapping in mappings) if mappings else "_",
        "/".join("1" if value else "0" for value in contextual) if contextual else "_",
    ))


def _special_witness(record: unicode_data.SpecialCaseMapping, *, negative: bool) -> tuple[int, ...]:
    # The pinned context vocabulary is conjunction-only. These witnesses are
    # intentionally constructed from raw scalar facts, not from Case runtime
    # behavior, so a contextual implementation cannot certify itself.
    prefix: list[int] = []
    suffix: list[int] = []
    if "Final_Sigma" in record.contexts:
        if negative:
            suffix.append(0x41)
        else:
            prefix.append(0x41)
    if "After_Soft_Dotted" in record.contexts:
        if negative:
            prefix.append(0x41)
        else:
            prefix.append(0x69)
    if "After_I" in record.contexts:
        if negative:
            prefix.append(0x41)
        else:
            prefix.append(0x49)
    if "More_Above" in record.contexts:
        suffix.append(0x41 if negative else 0x301)
    if "Before_Dot" in record.contexts:
        suffix.append(0x41 if negative else 0x307)
    if "Not_Before_Dot" in record.contexts:
        suffix.append(0x307 if negative else 0x41)
    return tuple(prefix + [record.source] + suffix)


def _special_profile(record: unicode_data.SpecialCaseMapping) -> str:
    if any(language in ("az", "tr") for language in record.languages):
        return "turkic"
    if "lt" in record.languages:
        return "lithuanian"
    return "default"


def case_rows() -> list[str]:
    manifest = load_manifest()
    canonical = unicode_data.load_canonical_properties(manifest)
    case = unicode_data.load_case_data(manifest, canonical)
    maps = _case_maps(case)
    hand_rows = [_encode_case_row("empty", "lower", "default", (), (), ())]
    unicode_rows: list[str] = []
    special_witness_rows: list[str] = []
    locale_mismatch_rows: list[str] = []
    false_context_rows: list[str] = []
    folding_rows: list[str] = []

    # Each UnicodeData simple mapping is exercised through the public full API.
    # The oracle separately applies SpecialCasing, so these rows also catch an
    # accidental omission of a full override for a simple source mapping.
    for operation, records in (
        ("lower", case.simple_lower),
        ("upper", case.simple_upper),
        ("title", case.simple_title),
    ):
        for record in records:
            source = (record.source,)
            output, contextual = _case_oracle(operation, "default", source, case, canonical, maps)
            unicode_rows.append(_encode_case_row(
                f"UnicodeData:{record.line}:{operation}", operation, "default", source, output, contextual
            ))

    language_rules = [record for record in case.special if record.languages]
    context_rules = [record for record in case.special if record.contexts]
    language_context_rules = [record for record in case.special if record.languages and record.contexts]
    if (len(case.special), len(language_rules), len(context_rules), len(language_context_rules)) != (119, 15, 9, 8):
        raise TestFailure("Unicode 17 SpecialCasing conditional rule inventory drifted")

    for record in case.special:
        profile = _special_profile(record)
        witness = _special_witness(record, negative=False)
        for operation in ("lower", "title", "upper"):
            output, contextual = _case_oracle(operation, profile, witness, case, canonical, maps)
            special_witness_rows.append(_encode_case_row(
                f"SpecialCasing:{record.line}:{operation}:witness", operation, profile,
                witness, output, contextual,
            ))
            if record.languages:
                # A named-language rule must not leak into the Unicode default
                # profile even when every source-context predicate is true.
                output, contextual = _case_oracle(
                    operation, "default", witness, case, canonical, maps
                )
                locale_mismatch_rows.append(_encode_case_row(
                    f"SpecialCasing:{record.line}:{operation}:locale-mismatch", operation,
                    "default", witness, output, contextual,
                ))
            if record.contexts:
                # A matching Turkic/Lithuanian profile must still reject its
                # row when the source-context condition is false.
                false_context = _special_witness(record, negative=True)
                output, contextual = _case_oracle(
                    operation, profile, false_context, case, canonical, maps
                )
                false_context_rows.append(_encode_case_row(
                    f"SpecialCasing:{record.line}:{operation}:false-context", operation,
                    profile, false_context, output, contextual,
                ))

    folds = {(record.source, record.status): record.mapping for record in case.folding}
    for record in case.folding:
        for profile in FOLD_PROFILES:
            source = (record.source,)
            folding_rows.append(_encode_case_row(
                f"CaseFolding:{record.line}:{record.status}:{profile}", "fold", profile,
                source, (_fold_mapping(profile, record.source, folds),), (False,),
            ))

    # R3 titlecasing must follow Word segments: uncased leading scalars do not
    # consume the first-cased position, apostrophes remain inside their word,
    # and the next segment restarts title selection. The maps are intentionally
    # spelled here rather than derived from Word runtime output.
    title_source = tuple(ord(value) for value in "123ABC's XYZ")
    title_maps = tuple((scalar,) for scalar in title_source[:3]) + (
        (ord("A"),), (ord("b"),), (ord("c"),), (ord("'"),), (ord("s"),),
        (ord(" "),), (ord("X"),), (ord("y"),), (ord("z"),),
    )
    hand_rows.append(_encode_case_row(
        "R3:word-boundary-title", "title", "default", title_source, title_maps,
        (True,) * len(title_source),
    ))

    # Context probes can be arbitrarily long. These two rows exercise both
    # outcomes of Final_Sigma through more than a thousand case-ignorable
    # scalars, without normalization.
    long_marks = (0x301,) * 1024
    for name, source in (
        ("long-final-sigma", (0x391, 0x3A3, *long_marks)),
        ("long-nonfinal-sigma", (0x391, 0x3A3, *long_marks, 0x391)),
    ):
        output, contextual = _case_oracle("lower", "default", source, case, canonical, maps)
        hand_rows.append(_encode_case_row(f"context:{name}", "lower", "default", source, output, contextual))

    # Canonically equivalent-looking inputs remain distinct transformations;
    # Case never normalizes them.
    for name, source in (
        ("no-normalization-composed", (0x130,)),
        ("no-normalization-decomposed", (0x49, 0x307)),
        ("supplementary-deseret", (0x10400,)),
    ):
        output, contextual = _case_oracle("lower", "default", source, case, canonical, maps)
        hand_rows.append(_encode_case_row(f"identity:{name}", "lower", "default", source, output, contextual))
    source = (0x390,)
    output, contextual = _case_oracle("upper", "default", source, case, canonical, maps)
    hand_rows.append(_encode_case_row(
        "identity:no-normalization-expansion", "upper", "default", source, output, contextual
    ))
    # The app's focused exact-limit probes use these same sources; retaining
    # them as protocol rows also keeps their ordinary facts and provenance
    # independently checked when limit coverage evolves.
    for name, operation, profile, source in (
        ("limit-empty", "lower", "default", ()),
        ("limit-ascii", "lower", "default", (0x41,)),
        ("limit-multibyte", "lower", "default", (0x130,)),
        ("limit-expansion", "upper", "default", (0xDF,)),
        ("limit-deletion", "lower", "turkic", (0x49, 0x307)),
    ):
        output, contextual = _case_oracle(operation, profile, source, case, canonical, maps)
        hand_rows.append(_encode_case_row(
            f"limits:{name}", operation, profile, source, output, contextual
        ))
    # These deliberately pin the committed Unicode 17 corpus and each harness
    # projection. A vendor/parser change cannot silently reduce one family.
    component_counts = (
        len(unicode_rows),
        len(special_witness_rows),
        len(locale_mismatch_rows),
        len(false_context_rows),
        len(folding_rows),
        len(hand_rows),
    )
    if component_counts != (4502, 357, 45, 27, 6472, 13):
        raise TestFailure(f"Case conformance row inventory drifted: {component_counts}")
    rows = [
        *unicode_rows,
        *special_witness_rows,
        *locale_mismatch_rows,
        *false_context_rows,
        *folding_rows,
        *hand_rows,
    ]
    if len(rows) != 11416:
        raise TestFailure(f"Case conformance total drifted: {len(rows)}")
    return rows


def run_case(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    rows = case_rows()
    run_parallel_suite(
        binary,
        "case",
        len(rows),
        lambda index: rows[index],
        jobs=jobs,
        timeout=int(spec["timeout_seconds"]),
    )


def fill_property_table(
    records: Iterable[RangeRecord],
    codes: dict[str, int],
    default: int,
    defaults: Iterable[MissingDefault] = (),
) -> bytearray:
    values = bytearray([default]) * (MAX_CODE_POINT + 1)
    for declaration in defaults:
        values[declaration.start : declaration.end + 1] = bytes([codes[declaration.value]]) * (
            declaration.end - declaration.start + 1
        )
    for record in records:
        values[record.start : record.end + 1] = bytes([codes[record.property]]) * (
            record.end - record.start + 1
        )
    return values


def build_property_tables() -> tuple[
    str,
    bytearray,
    bytearray,
    bytearray,
    bytearray,
    bytearray,
    array,
    array,
    bytearray,
]:
    manifest = load_manifest()
    properties = load_property_data(manifest)
    public = load_public_properties(manifest, load_canonical_properties(manifest))
    gcb_values = fill_property_table(properties.grapheme.records, GCB_CODES, 0)
    eaw_values = fill_property_table(
        properties.east_asian_width.records,
        EAW_CODES,
        0,
        properties.east_asian_width.defaults,
    )
    emoji_values = bytearray(MAX_CODE_POINT + 1)
    for record in properties.emoji.records:
        bit = EMOJI_BITS[record.property]
        for code_point in range(record.start, record.end + 1):
            emoji_values[code_point] |= bit
    bidi_values = fill_property_table(
        public.bidi_class.records,
        BIDI_CODES,
        BIDI_CODES["L"],
        public.bidi_class.defaults,
    )
    mirrored_values = fill_property_table(public.bidi_mirrored, {"Y": 1}, 0)
    mirroring_glyphs = array("I", [BIDI_ABSENT_SCALAR]) * (MAX_CODE_POINT + 1)
    for record in public.bidi_mirroring_glyph:
        mirroring_glyphs[record.source] = record.target
    bracket_targets = array("I", [BIDI_ABSENT_SCALAR]) * (MAX_CODE_POINT + 1)
    bracket_kinds = bytearray(MAX_CODE_POINT + 1)
    for record in public.bidi_brackets:
        bracket_targets[record.source] = record.target
        bracket_kinds[record.source] = 1 if record.kind == "o" else 2
    return (
        release_version(manifest, "unicode"),
        gcb_values,
        eaw_values,
        emoji_values,
        bidi_values,
        mirrored_values,
        mirroring_glyphs,
        bracket_targets,
        bracket_kinds,
    )


def scalar_at(index: int) -> int:
    return index if index < 0xD800 else index + 0x800


def run_properties(binary: Path, jobs: int, spec: dict[str, object]) -> None:
    if spec["suite"] != "properties":
        raise TestFailure("properties spec suite has drifted")
    version, gcb, eaw, emoji, bidi, mirrored, mirroring_glyph, bracket_target, bracket_kind = build_property_tables()

    def row(index: int) -> str:
        code_point = scalar_at(index)
        return (
            f"{version}:scalar:{code_point:06X}\t{code_point:X}\t"
            f"{gcb[code_point]}\t{eaw[code_point]}\t{emoji[code_point]}\t"
            f"{bidi[code_point]}\t{mirrored[code_point]}\t{mirroring_glyph[code_point]}\t"
            f"{bracket_target[code_point]}\t{bracket_kind[code_point]}"
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

WORD_ALLOCATION_MODES = (
    "iterator",
    "cursor",
    "ranges",
    "slices",
    "owned",
)

WORD_ALLOCATION_FIXTURES = {
    **ALLOCATION_FIXTURES,
    "segmented": "a a",
}
ALLOCATION_BIDI_FIXTURES = {
    "ltr": "The quick brown fox has 123 words.",
    "mixed": "abc אבג 123 العربية",
    "isolates": "a \u2068אב (12)\u2069 ب",
}
ALLOCATION_BIDI_SCALING_FIXTURES = {
    "ltr": ("abc 123 " * 64, "abc 123 " * 256),
    "mixed": ("a אב(12) ب " * 64, "a אב(12) ب " * 256),
    "isolates": ("a \u2068אב (12)\u2069 ب " * 64, "a \u2068אב (12)\u2069 ب " * 256),
}

CASE_ALLOCATION_MODES = (
    "lower-default",
    "lower-turkic",
    "upper-default",
    "upper-lithuanian",
    "title-default",
    "title-turkic",
    "fold-full",
    "fold-simple",
    "fold-turkic-full",
    "fold-turkic-simple",
    "limits",
)

CASE_ALLOCATION_SUCCESS_MODES = CASE_ALLOCATION_MODES[:-1]
CASE_ALLOCATION_ASCII_HALF_SCALARS = 320
CASE_ALLOCATION_ASCII_SCALARS = 640
# Doubling the ASCII input must not double the allocation count: the result
# buffer and the fact list both grow geometrically, so the extra 320 scalars
# may only add the few reallocations that growth schedule needs. A slope
# anywhere near the added scalar count means per-scalar copying is back.
CASE_ALLOCATION_ASCII_MAX_DELTA = 8

# These sources make result ownership and the Case-specific context paths
# visible to exact allocation baselines across distinct scalar and byte-count
# regimes, rather than letting a baseline change hide in one ASCII example.
CASE_ALLOCATION_FIXTURES = {
    "ascii-half": "ASCII Case" * 32,
    "ascii": "ASCII Case" * 64,
    "expansion-heavy": "\u0390" * 256,
    "long-case-ignorable": "\u0391\u03a3" + "\u0301" * 1024,
    "alternating-sigma": "\u0391\u03a3" * 512,
    "turkic": ("I\u0307I\u0323\u0307i" * 128),
    "lithuanian": ("I\u0301J\u0300\u012e\u0301" * 128),
    "many-title-words": ("123ABC's XYZ " * 256),
}

CASE_LIMIT_FIXTURES = {
    "input-bytes": "A",
    "input-scalars": "A",
    "output-bytes": "A",
    "output-scalars": "A",
    "facts": "A",
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
    if suites != [
        "allocation-calibration",
        "allocation-aliases",
        "allocation-line-break-cursor",
        "allocation-bidi-analysis",
        "allocation-bidi-scaling",
        "allocation-word-iterator",
        "allocation-word-cursor",
        "allocation-word-ranges",
        "allocation-word-slices",
        "allocation-word-owned",
        "allocation-case-lower-default",
        "allocation-case-lower-turkic",
        "allocation-case-upper-default",
        "allocation-case-upper-lithuanian",
        "allocation-case-title-default",
        "allocation-case-title-turkic",
        "allocation-case-fold-full",
        "allocation-case-fold-simple",
        "allocation-case-fold-turkic-full",
        "allocation-case-fold-turkic-simple",
        "allocation-case-limits",
        "allocation-baselines",
    ]:
        raise TestFailure("allocation spec suites have drifted")
    run_serial_suite(binary, suites[0], calibration, timeout=timeout)
    run_serial_suite(binary, suites[1], ["all\t\t0"], timeout=timeout)
    line_break_rows = [
        f"{name}\t{utf8_hex(value)}\t0"
        for name, value in ALLOCATION_FIXTURES.items()
    ]
    run_serial_suite(binary, suites[2], line_break_rows, timeout=timeout)
    bidi_rows = [
        f"{name}\t{utf8_hex(value)}\tpositive"
        for name, value in ALLOCATION_BIDI_FIXTURES.items()
    ]
    run_serial_suite(binary, suites[3], bidi_rows, timeout=timeout)
    bidi_scaling_rows = [
        f"{name}\t{utf8_hex(small)}|{utf8_hex(large)}\tlinear"
        for name, (small, large) in ALLOCATION_BIDI_SCALING_FIXTURES.items()
    ]
    run_serial_suite(binary, suites[4], bidi_scaling_rows, timeout=timeout)

    machine = platform.machine().lower()
    if platform.system() != "Linux" or machine not in ("x86_64", "amd64"):
        print("SKIP allocation-baselines: exact baselines are Linux x64 only")
        return
    try:
        baseline_path = ROOT / str(spec["baseline_file"])
        baseline = json.loads(baseline_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as err:
        raise TestFailure(f"unable to read allocation baselines: {err}") from err
    if (
        baseline.get("schema_version") != 5
        or baseline.get("platform") != "roc-platform-template-zig-1.1.0+alloc-count"
        or baseline.get("target") != "x64musl"
        or baseline.get("optimize") != "speed"
    ):
        raise TestFailure("allocation baseline metadata has drifted")
    expected = baseline.get("fixtures")
    if not isinstance(expected, dict) or set(expected) != set(ALLOCATION_FIXTURES):
        raise TestFailure("allocation baseline fixture set has drifted")
    word = baseline.get("word")
    if not isinstance(word, dict) or set(word) != set(WORD_ALLOCATION_MODES):
        raise TestFailure("word allocation baseline modes have drifted")
    for mode in WORD_ALLOCATION_MODES:
        expected_word = word[mode]
        if not isinstance(expected_word, dict) or set(expected_word) != set(WORD_ALLOCATION_FIXTURES):
            raise TestFailure(f"word allocation baseline fixture set has drifted for {mode}")
    case = baseline.get("case")
    if not isinstance(case, dict) or set(case) != set(CASE_ALLOCATION_MODES):
        raise TestFailure("case allocation baseline modes have drifted")
    case_ascii_doubling_deltas = baseline.get("case_ascii_doubling_deltas")
    if not isinstance(case_ascii_doubling_deltas, dict) or set(case_ascii_doubling_deltas) != set(CASE_ALLOCATION_SUCCESS_MODES):
        raise TestFailure("case ASCII doubling allocation baseline modes have drifted")
    if (
        len(CASE_ALLOCATION_FIXTURES["ascii-half"]) != CASE_ALLOCATION_ASCII_HALF_SCALARS
        or len(CASE_ALLOCATION_FIXTURES["ascii"]) != CASE_ALLOCATION_ASCII_SCALARS
    ):
        raise TestFailure("case ASCII doubling fixtures have drifted")
    for mode in CASE_ALLOCATION_MODES:
        expected_case = case[mode]
        fixtures = CASE_LIMIT_FIXTURES if mode == "limits" else CASE_ALLOCATION_FIXTURES
        if not isinstance(expected_case, dict) or set(expected_case) != set(fixtures):
            raise TestFailure(f"case allocation baseline fixture set has drifted for {mode}")
        if mode in CASE_ALLOCATION_SUCCESS_MODES:
            documented_delta = case_ascii_doubling_deltas[mode]
            actual_delta = expected_case["ascii"] - expected_case["ascii-half"]
            if (
                not isinstance(documented_delta, int)
                or not 0 <= documented_delta <= CASE_ALLOCATION_ASCII_MAX_DELTA
                or actual_delta != documented_delta
            ):
                raise TestFailure(f"case ASCII allocation scaling has drifted for {mode}")
        rows = [
            f"{name}\t{utf8_hex(value)}\t{expected_case[name]}"
            for name, value in fixtures.items()
        ]
        run_serial_suite(binary, f"allocation-case-{mode}", rows, timeout=timeout)
    rows = [
        f"{name}\t{utf8_hex(value)}\t{expected[name]}"
        for name, value in ALLOCATION_FIXTURES.items()
    ]
    for offset, mode in enumerate(WORD_ALLOCATION_MODES, start=5):
        word_rows = [
            f"{name}\t{utf8_hex(value)}\t{word[mode][name]}"
            for name, value in WORD_ALLOCATION_FIXTURES.items()
        ]
        run_serial_suite(binary, suites[offset], word_rows, timeout=timeout)

    run_serial_suite(binary, suites[-1], rows, timeout=timeout)


def verify_pinned_roc(roc: str) -> None:
    completed = command([roc, "version"])
    pinned = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    pinned_revision = pinned.rsplit("-", 1)[-1]
    if pinned not in completed.stdout and pinned_revision not in completed.stdout:
        raise TestFailure(
            f"repository requires {pinned}, got {completed.stdout.strip()!r}"
        )


def run_data_checks() -> None:
    validate_all(load_manifest())
    command([sys.executable, "scripts/unicode_data.py", "generate", "--check"])
    command([
        sys.executable,
        "-m",
        "unittest",
        "scripts/test_unicode_data.py",
        "scripts/test_bidi_reduce.py",
    ])


def main(argv: list[str] | None = None) -> int:
    configure_output()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "suite",
        nargs="?",
        choices=("all", "data", "bidi", "grapheme", "line-break", "word", "case", "properties", "allocations"),
        default="all",
    )
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default=os.environ.get("ZIG", "zig"))
    parser.add_argument("--jobs", type=int, default=default_jobs())
    parser.add_argument("--skip-build", action="store_true")
    args = parser.parse_args(argv)
    if args.jobs < 1:
        parser.error("--jobs must be positive")
    try:
        app_specs = load_app_specs()
        if args.suite != "data":
            verify_pinned_roc(args.roc)
        if args.suite in ("all", "data"):
            run_data_checks()
        requested_apps = []
        if args.suite in ("all", "bidi"):
            requested_apps.append("bidi")
        if args.suite in ("all", "grapheme"):
            requested_apps.append("grapheme")
        if args.suite in ("all", "line-break"):
            requested_apps.append("line-break")
        if args.suite in ("all", "word"):
            requested_apps.append("word")
        if args.suite in ("all", "case"):
            requested_apps.append("case")
        if args.suite in ("all", "properties"):
            requested_apps.append("properties")
        if args.suite in ("all", "allocations"):
            requested_apps.append("allocation")
        if requested_apps:
            binaries = build_apps(
                args.roc, requested_apps, args.zig, skip_build=args.skip_build
            )
            if "grapheme" in requested_apps:
                run_grapheme(binaries["grapheme"], args.jobs, app_specs["grapheme"])
            if "bidi" in requested_apps:
                run_bidi(binaries["bidi"], args.jobs, app_specs["bidi"])
            if "line-break" in requested_apps:
                run_line_break(binaries["line-break"], args.jobs, app_specs["line-break"])
            if "word" in requested_apps:
                run_word(binaries["word"], args.jobs, app_specs["word"])
            if "case" in requested_apps:
                run_case(binaries["case"], args.jobs, app_specs["case"])
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
