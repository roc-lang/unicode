#!/usr/bin/env python3
"""Build and compare direct and composite Unicode property queries."""

from __future__ import annotations

import argparse
import json
import os
import platform
import shutil
import statistics
import subprocess
import time
from pathlib import Path
from typing import Sequence


ROOT = Path(__file__).resolve().parents[2]
BENCH = Path(__file__).resolve().parent
BUILD = ROOT / ".roc-unicode-tmp" / "benchmarks" / "properties"
DEFAULT_OUTPUT = BUILD / "results.json"
DEFAULT_TARGET_BYTES = 32 * 1024
DEFAULT_SAMPLES = 7


class BenchmarkFailure(RuntimeError):
    pass


def command(args: Sequence[str], *, cwd: Path = ROOT) -> None:
    printable = [str(arg) for arg in args]
    print("+", " ".join(printable), flush=True)
    subprocess.run(printable, cwd=cwd, check=True)


def capture(args: Sequence[str], *, cwd: Path = ROOT) -> str:
    return subprocess.run(
        [str(arg) for arg in args],
        cwd=cwd,
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()


def resolve_tool(value: str) -> str:
    path = Path(value).expanduser()
    if path.parent == Path("."):
        return value
    return str(path.resolve())


def repeated(unit: str, target_bytes: int) -> bytes:
    encoded = unit.encode("utf-8")
    return encoded * max(1, target_bytes // len(encoded))


def corpora(target_bytes: int) -> dict[str, bytes]:
    return {
        "ascii": repeated(
            "The quick brown fox jumps over 13 lazy dogs. ", target_bytes
        ),
        "bmp": repeated(
            "Καλημέρα κόσμε Привет мир مرحبا بالعالم "
            "नमस्ते दुनिया 日本語の文章 한국어 문장 ",
            target_bytes,
        ),
        "supplementary": repeated(
            "𐀀𐐀𓀀𞤀𠀀𝄞𝔘𑄃𑻠𞸀", target_bytes
        ),
        "mixed": repeated(
            "Latin A\u0301 العربية नमस्ते 日本語 𐐀𓀀 👩🏽‍💻 🏳️‍🌈 \u2067(42)\u2069 ",
            target_bytes,
        ),
    }


def build(roc: str, zig: str) -> dict[str, Path]:
    BUILD.mkdir(parents=True, exist_ok=True)
    command(
        [zig, "build", "native", "-Doptimize=ReleaseFast"],
        cwd=ROOT / "tests/platform",
    )
    binaries: dict[str, Path] = {}
    for name in (
        "direct",
        "composite",
        "validate",
        "validate-row",
        "validate-aliases",
        "validate-runs",
        "validate-emoji",
        "validate-bidi",
        "allocations",
    ):
        source = BENCH / f"{name}.roc"
        binary = BUILD / name
        command([roc, "check", str(source), "--no-cache"])
        command(
            [roc, "build", str(source), "--opt=speed", f"--output={binary}"]
        )
        binaries[name] = binary
    return binaries


def affinity_prefix(cpu: int | None) -> list[str]:
    taskset = shutil.which("taskset")
    if cpu is None or taskset is None:
        return []
    return [taskset, "--cpu-list", str(cpu)]


def run_once(binary: Path, corpus: bytes, cpu: int | None) -> tuple[str, float]:
    start = time.perf_counter_ns()
    completed = subprocess.run(
        [*affinity_prefix(cpu), str(binary)],
        cwd=ROOT,
        check=True,
        input=corpus,
        stdout=subprocess.PIPE,
    )
    elapsed_seconds = (time.perf_counter_ns() - start) / 1_000_000_000
    return completed.stdout.decode("utf-8").strip(), elapsed_seconds


def run_output(binary: Path, corpus: bytes) -> str:
    return subprocess.run(
        [str(binary)],
        cwd=ROOT,
        check=True,
        input=corpus,
        stdout=subprocess.PIPE,
    ).stdout.decode("utf-8").strip()


def validate_semantics(binaries: dict[str, Path]) -> dict[str, object]:
    outputs: dict[str, str] = {}
    for name in (
        "validate",
        "validate-row",
        "validate-aliases",
        "validate-runs",
        "validate-emoji",
        "validate-bidi",
    ):
        output = run_output(binaries[name], b"runtime-salt")
        if not output.startswith("PASS\t"):
            raise BenchmarkFailure(f"{name} failed semantic validation: {output}")
        outputs[name] = output

    lengths = (1, 2, 4, 8, 16, 32, 64, 128, 256)
    homogeneous: dict[int, tuple[int, int, int, int]] = {}
    early_stop: dict[int, int] = {}
    for length in lengths:
        for label, corpus in (
            ("homogeneous", b"A" * length),
            ("early", "Aα".encode("utf-8") + b"A" * length),
        ):
            fields = run_output(binaries["allocations"], corpus).split("\t")
            if len(fields) != 6:
                raise BenchmarkFailure(
                    f"allocations/{label}/{length} produced malformed output {fields!r}"
                )
            (
                property_allocs,
                run_allocs,
                first_allocs,
                alias_allocs,
                _alias_result,
                checksum,
            ) = (int(field) for field in fields)
            if checksum == 0:
                raise BenchmarkFailure(
                    f"allocations/{label}/{length} checksum was optimized away"
                )
            if alias_allocs != 0:
                raise BenchmarkFailure(
                    f"loose matching allocated {alias_allocs} times for {label}/{length}"
                )
            if any(value > 1 for value in (property_allocs, run_allocs, first_allocs)):
                raise BenchmarkFailure(
                    f"indexed scan allocations exceeded fixed inline-view cost for "
                    f"{label}/{length}: {(property_allocs, run_allocs, first_allocs)!r}"
                )
            if label == "homogeneous":
                homogeneous[length] = (
                    property_allocs,
                    run_allocs,
                    first_allocs,
                    alias_allocs,
                )
            else:
                early_stop[length] = first_allocs

    alias_fields = run_output(
        binaries["allocations"], b"is\tUppercase - Letter"
    ).split("\t")
    if (
        len(alias_fields) != 6
        or int(alias_fields[3]) != 0
        or int(alias_fields[4]) != 2
    ):
        raise BenchmarkFailure(
            f"allocation-free loose alias probe failed: {alias_fields!r}"
        )

    for length, values in homogeneous.items():
        expected_view_cost = 1 if length <= 16 else 0
        if values[:3] != (expected_view_cost,) * 3:
            raise BenchmarkFailure(
                f"unexpected pinned indexed-view allocation count for {length}: "
                f"{values[:3]!r}"
            )
        if early_stop[length] != expected_view_cost:
            raise BenchmarkFailure(
                f"unexpected pinned early-stop allocation count for {length}: "
                f"{early_stop[length]}"
            )

    # Inline strings may materialize one fixed indexed byte view; heap-backed
    # strings borrow it. No scan is allowed to grow allocations with length.
    for column, name in enumerate(("Property.iter", "iter_runs", "first run")):
        observed = {values[column] for values in homogeneous.values()}
        if not observed <= {0, 1}:
            raise BenchmarkFailure(f"{name} allocations grew with input: {observed}")
    if set(early_stop.values()) - {0, 1}:
        raise BenchmarkFailure(
            f"early-stop run allocations grew with suffix length: {early_stop!r}"
        )

    return {
        "executables": outputs,
        "allocation_lengths": list(lengths),
        "homogeneous_allocations": {
            str(length): list(values) for length, values in homogeneous.items()
        },
        "early_stop_allocations": {
            str(length): value for length, value in early_stop.items()
        },
    }


def machine() -> dict[str, object]:
    return {
        "system": platform.system(),
        "release": platform.release(),
        "machine": platform.machine(),
        "processor": platform.processor(),
        "cpu_count": os.cpu_count(),
    }


def benchmark(
    binaries: dict[str, Path],
    cases: dict[str, bytes],
    samples: int,
    cpu: int | None,
) -> dict[str, object]:
    results: dict[str, object] = {}
    for case_name, corpus in cases.items():
        # Establish semantic agreement before recording a timing sample.
        checksums = {
            name: run_output(binaries[name], corpus)
            for name in ("direct", "composite")
        }
        if checksums["direct"] != checksums["composite"]:
            raise BenchmarkFailure(
                f"direct/composite preflight mismatch for {case_name}: "
                f"{checksums['direct']} != {checksums['composite']}"
            )
        timings: dict[str, list[float]] = {"direct": [], "composite": []}
        # Alternate order per sample so thermal or scheduling drift is not
        # systematically assigned to one implementation.
        for sample in range(samples):
            order = ("direct", "composite") if sample % 2 == 0 else ("composite", "direct")
            for name in order:
                checksum, elapsed = run_once(binaries[name], corpus, cpu)
                previous = checksums[name]
                if checksum != previous:
                    raise BenchmarkFailure(
                        f"{name}/{case_name} produced a nondeterministic checksum"
                    )
                timings[name].append(elapsed)
        if checksums["direct"] != checksums["composite"]:
            raise BenchmarkFailure(
                f"direct/composite checksum mismatch for {case_name}: "
                f"{checksums['direct']} != {checksums['composite']}"
            )

        case_result: dict[str, object] = {
            "bytes": len(corpus),
            "checksum": checksums["direct"],
        }
        medians: dict[str, float] = {}
        for name in ("direct", "composite"):
            median = statistics.median(timings[name])
            medians[name] = median
            case_result[name] = {
                "median_seconds": median,
                "mad_seconds": statistics.median(
                    abs(value - median) for value in timings[name]
                ),
                "decimal_mb_per_second": len(corpus) / median / 1_000_000,
                "samples_seconds": timings[name],
            }
        case_result["direct_over_composite"] = (
            medians["direct"] / medians["composite"]
        )
        results[case_name] = case_result
    return results


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default="zig")
    parser.add_argument("--samples", type=int, default=DEFAULT_SAMPLES)
    parser.add_argument("--target-bytes", type=int, default=DEFAULT_TARGET_BYTES)
    parser.add_argument("--cpu", type=int, default=0)
    parser.add_argument("--no-affinity", action="store_true")
    parser.add_argument("--no-build", action="store_true")
    parser.add_argument("--validate-only", action="store_true")
    parser.add_argument("--case", action="append", choices=tuple(corpora(1)))
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.samples < 1 or args.target_bytes < 1:
        raise BenchmarkFailure("--samples and --target-bytes must be positive")
    roc = resolve_tool(args.roc)
    zig = resolve_tool(args.zig)
    binaries = (
        {
            name: BUILD / name
            for name in (
                "direct",
                "composite",
                "validate",
                "validate-row",
                "validate-aliases",
                "validate-runs",
                "validate-emoji",
                "validate-bidi",
                "allocations",
            )
        }
        if args.no_build
        else build(roc, zig)
    )
    for binary in binaries.values():
        if not binary.is_file():
            raise BenchmarkFailure(f"missing benchmark binary: {binary}")

    validation = validate_semantics(binaries)
    print(json.dumps({"validation": validation}, indent=2))
    if args.validate_only:
        return 0

    selected = corpora(args.target_bytes)
    if args.case:
        selected = {name: selected[name] for name in args.case}
    cpu = None if args.no_affinity else args.cpu
    report = {
        "schema": 1,
        "machine": machine(),
        "roc_version": capture([roc, "version"]),
        "samples": args.samples,
        "cpu_affinity": cpu if affinity_prefix(cpu) else None,
        "binary_bytes": {
            name: binary.stat().st_size for name, binary in binaries.items()
        },
        "validation": validation,
        "cases": benchmark(binaries, selected, args.samples, cpu),
    }
    output = args.output.expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))
    print(f"wrote {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (BenchmarkFailure, subprocess.CalledProcessError) as error:
        print(f"benchmark failed: {error}", file=os.sys.stderr)
        raise SystemExit(1) from error
