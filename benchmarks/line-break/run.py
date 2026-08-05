#!/usr/bin/env python3
"""Build and benchmark allocation-free Unicode line-break opportunities."""

from __future__ import annotations

import argparse
import json
import os
import statistics
import subprocess
import time
from pathlib import Path
from typing import Sequence


ROOT = Path(__file__).resolve().parents[2]
BENCH = Path(__file__).resolve().parent
BUILD = ROOT / ".roc-unicode-tmp" / "benchmarks" / "line-break"
DEFAULT_OUTPUT = BUILD / "results.json"
DEFAULT_TARGET_SECONDS = 0.40
DEFAULT_SAMPLES = 9
MAX_REPEATS = 99_999_999


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


def hard_line_break_scalars() -> set[int]:
    path = ROOT / "vendor/unicode/17.0.0/LineBreak.txt"
    hard: set[int] = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        scalar_range, property_name = (part.strip() for part in body.split(";"))
        if property_name not in {"BK", "CR", "LF", "NL"}:
            continue
        bounds = scalar_range.split("..")
        start = int(bounds[0], 16)
        end = int(bounds[-1], 16)
        hard.update(range(start, end + 1))
    return hard


def conformance_corpus() -> tuple[bytes, tuple[int, int, int], int]:
    path = ROOT / "vendor/unicode/17.0.0/LineBreakTest.txt"
    hard = hard_line_break_scalars()
    corpus = bytearray()
    offsets: list[int] = []
    cases = 0
    previous_last: int | None = None
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        tokens = body.split()
        code_points = [int(tokens[index], 16) for index in range(1, len(tokens), 2)]
        markers = tokens[0::2]
        if len(markers) != len(code_points) + 1:
            raise BenchmarkFailure("malformed LineBreakTest row")

        # A BK before each case guarantees a mandatory boundary at the case
        # start. LB6 prohibits the boundary before it except when the previous
        # case itself ends in a hard break, which earlier LB4/LB5 make
        # mandatory.
        if previous_last in hard:
            offsets.append(len(corpus))
        corpus.extend(b"\x0b")
        offsets.append(len(corpus))
        for index, code_point in enumerate(code_points):
            corpus.extend(chr(code_point).encode("utf-8"))
            if index + 1 < len(code_points) and markers[index + 1] == "÷":
                offsets.append(len(corpus))
        previous_last = code_points[-1]
        cases += 1

    offsets.append(len(corpus))
    signature = (
        len(offsets),
        sum(offsets),
        sum(index * offset for index, offset in enumerate(offsets, start=1)),
    )
    return bytes(corpus), signature, cases


def invoke(binary: Path, source: bytes, repeats: int) -> tuple[str, float]:
    payload = f"{repeats:08d}\n".encode("ascii") + source
    started = time.perf_counter()
    completed = subprocess.run(
        [str(binary)],
        cwd=ROOT,
        input=payload,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    elapsed = time.perf_counter() - started
    if completed.returncode != 0:
        raise BenchmarkFailure(
            f"benchmark exited {completed.returncode}: "
            f"{completed.stderr.decode('utf-8', errors='replace').strip()}"
        )
    return completed.stdout.decode("utf-8").strip(), elapsed


def calibrate(binary: Path, source: bytes, target_seconds: float) -> int:
    repeats = 1
    while True:
        _, elapsed = invoke(binary, source, repeats)
        if elapsed >= target_seconds or repeats >= MAX_REPEATS:
            return repeats
        scale = max(2, min(10, int(target_seconds / max(elapsed, 0.000001))))
        repeats = min(MAX_REPEATS, repeats * scale)


def benchmark(
    binary: Path,
    source: bytes,
    samples: int,
    target_seconds: float,
) -> dict[str, object]:
    repeats = calibrate(binary, source, target_seconds)
    rates: list[float] = []
    expected_count: str | None = None
    for _ in range(samples):
        output, elapsed = invoke(binary, source, repeats)
        if expected_count is None:
            expected_count = output
        elif output != expected_count:
            raise BenchmarkFailure("timed scans produced inconsistent opportunity counts")
        rates.append(len(source) * repeats / elapsed / 1_000_000)
    median = statistics.median(rates)
    mad = statistics.median(abs(value - median) for value in rates)
    total_opportunities = int(expected_count or "0")
    if total_opportunities % repeats != 0:
        raise BenchmarkFailure("timed opportunity count is not repeat-stable")
    return {
        "bytes": len(source),
        "repeats": repeats,
        "samples": samples,
        "median_mb_s": round(median, 3),
        "mad_mb_s": round(mad, 3),
        "sample_mb_s": [round(value, 3) for value in rates],
        "opportunities_per_scan": total_opportunities // repeats,
    }


def compare_baseline(path: Path, current: dict[str, object]) -> float:
    try:
        baseline = json.loads(path.read_text(encoding="utf-8"))
        previous = float(baseline["benchmark"]["median_mb_s"])
        now = float(current["median_mb_s"])
    except (OSError, ValueError, KeyError, TypeError, json.JSONDecodeError) as err:
        raise BenchmarkFailure(f"invalid baseline {path}: {err}") from err
    return (now / previous - 1.0) * 100.0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default=os.environ.get("ZIG", "zig"))
    parser.add_argument("--samples", type=int, default=DEFAULT_SAMPLES)
    parser.add_argument("--target-seconds", type=float, default=DEFAULT_TARGET_SECONDS)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--baseline", type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.samples < 1 or args.target_seconds <= 0:
        raise BenchmarkFailure("samples and target-seconds must be positive")

    BUILD.mkdir(parents=True, exist_ok=True)
    binary = BUILD / "roc-line-break"
    command([
        args.zig,
        "build",
        "--build-file",
        "tests/platform/build.zig",
        "native",
        "-Doptimize=ReleaseFast",
    ])
    command([
        args.roc,
        "build",
        BENCH / "roc" / "main.roc",
        "--opt=speed",
        f"--output={binary}",
        "--no-cache",
    ])

    source, signature, case_count = conformance_corpus()
    got, _ = invoke(binary, source, 0)
    expected = " ".join(str(value) for value in signature)
    if got != expected:
        raise BenchmarkFailure(f"semantic signature mismatch: expected {expected}, got {got}")

    result = benchmark(binary, source, args.samples, args.target_seconds)
    document: dict[str, object] = {
        "schema_version": 1,
        "unicode_version": "17.0.0",
        "uax14_revision": 55,
        "roc_version": capture([args.roc, "version"]),
        "corpus": {
            "source": "LineBreakTest.txt with BK case separators",
            "cases": case_count,
            "signature": list(signature),
        },
        "benchmark": result,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")

    print(
        f"line-break: {result['median_mb_s']:.3f} MB/s "
        f"(MAD {result['mad_mb_s']:.3f}, {result['samples']} samples)"
    )
    if args.baseline is not None:
        delta = compare_baseline(args.baseline, result)
        print(f"same-machine baseline delta: {delta:+.2f}%")
    print(f"wrote {args.output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (BenchmarkFailure, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=os.sys.stderr)
        raise SystemExit(1)
