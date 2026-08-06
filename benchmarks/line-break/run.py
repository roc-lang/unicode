#!/usr/bin/env python3
"""Build and benchmark allocation-free Unicode line-break opportunities."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import shutil
import statistics
import subprocess
import sys
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


def expected_timed_count(output: str, *, per_scan: int, repeats: int) -> None:
    expected = per_scan * repeats
    if output != str(expected):
        raise BenchmarkFailure(
            f"timed opportunity count mismatch: expected {expected}, got {output!r}"
        )


def calibrate(
    binary: Path,
    source: bytes,
    target_seconds: float,
    expected_per_scan: int,
) -> int:
    repeats = 1
    while True:
        output, elapsed = invoke(binary, source, repeats)
        expected_timed_count(output, per_scan=expected_per_scan, repeats=repeats)
        if elapsed >= target_seconds or repeats >= MAX_REPEATS:
            return repeats
        scale = max(2, min(10, int(target_seconds / max(elapsed, 0.000001))))
        repeats = min(MAX_REPEATS, repeats * scale)


def benchmark(
    binary: Path,
    source: bytes,
    samples: int,
    target_seconds: float,
    expected_per_scan: int,
) -> dict[str, object]:
    repeats = calibrate(binary, source, target_seconds, expected_per_scan)
    rates: list[float] = []
    for _ in range(samples):
        output, elapsed = invoke(binary, source, repeats)
        expected_timed_count(output, per_scan=expected_per_scan, repeats=repeats)
        rates.append(len(source) * repeats / elapsed / 1_000_000)
    median = statistics.median(rates)
    mad = statistics.median(abs(value - median) for value in rates)
    return {
        "bytes": len(source),
        "repeats": repeats,
        "samples": samples,
        "median_mb_s": round(median, 3),
        "mad_mb_s": round(mad, 3),
        "sample_mb_s": [round(value, 3) for value in rates],
        "opportunities_per_scan": expected_per_scan,
    }


def compare_baseline(path: Path, current: dict[str, object]) -> float:
    try:
        baseline = json.loads(path.read_text(encoding="utf-8"))
        previous_result = baseline.get("benchmarks", {}).get(
            "unicode_conformance", baseline.get("benchmark", {})
        )
        previous = float(previous_result["median_mb_s"])
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
    parser.add_argument(
        "--cpu",
        type=int,
        help="pin the runner and inherited benchmark processes to one CPU",
    )
    return parser.parse_args()


def sha256_path(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def executable_provenance(command_name: str) -> dict[str, str]:
    found = shutil.which(command_name)
    if found is None:
        raise BenchmarkFailure(f"executable not found: {command_name}")
    path = Path(found).resolve()
    return {"path": str(path), "sha256": sha256_path(path)}


def cpu_model() -> str:
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.exists():
        for line in cpuinfo.read_text(encoding="utf-8", errors="replace").splitlines():
            if line.startswith("model name"):
                return line.split(":", 1)[-1].strip()
    return platform.processor()


def provenance(args: argparse.Namespace) -> dict[str, object]:
    status = capture(["git", "status", "--porcelain=v1"])
    affinity = sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else None
    manifest_path = ROOT / "vendor/unicode/manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    relevant_sources: dict[str, object] = {}
    for key in ("line_break", "derived_line_break", "line_break_test"):
        entry = manifest["sources"][key]
        path = ROOT / entry["path"]
        relevant_sources[key] = {
            "path": entry["path"],
            "manifest_sha256": entry["sha256"],
            "actual_sha256": sha256_path(path),
        }
    benchmark_sources = {}
    for path in (Path(__file__), BENCH / "roc/main.roc", ROOT / "tests/platform/build.zig"):
        benchmark_sources[str(path.relative_to(ROOT))] = sha256_path(path)
    return {
        "git": {
            "commit": capture(["git", "rev-parse", "HEAD"]),
            "dirty": bool(status),
        },
        "platform": {
            "system": platform.system(),
            "release": platform.release(),
            "machine": platform.machine(),
            "cpu_model": cpu_model(),
            "logical_cpu_count": os.cpu_count(),
            "affinity": affinity,
        },
        "tools": {
            "roc": {
                **executable_provenance(args.roc),
                "version": capture([args.roc, "version"]),
            },
            "zig": {
                **executable_provenance(args.zig),
                "version": capture([args.zig, "version"]),
            },
            "python": {
                "path": str(Path(sys.executable).resolve()),
                "version": platform.python_version(),
                "sha256": sha256_path(Path(sys.executable).resolve()),
            },
        },
        "sources": {
            "manifest": {
                "path": str(manifest_path.relative_to(ROOT)),
                "sha256": sha256_path(manifest_path),
            },
            "unicode": relevant_sources,
            "benchmark": benchmark_sources,
        },
    }


def main() -> int:
    args = parse_args()
    if args.samples < 1 or args.target_seconds <= 0:
        raise BenchmarkFailure("samples and target-seconds must be positive")
    pinned_roc = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    actual_roc = capture([args.roc, "version"])
    if pinned_roc not in actual_roc:
        raise BenchmarkFailure(
            f"benchmark requires {pinned_roc}, got {actual_roc!r}"
        )
    command([sys.executable, "scripts/unicode_data.py", "validate"])

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

    if args.cpu is not None:
        if not hasattr(os, "sched_setaffinity"):
            raise BenchmarkFailure("--cpu is not supported on this platform")
        available = os.sched_getaffinity(0)
        if args.cpu not in available:
            raise BenchmarkFailure(
                f"CPU {args.cpu} is outside current affinity {sorted(available)}"
            )
        # Pin only after build: the measured child processes inherit this
        # affinity, while build tools retain their normal parallelism.
        os.sched_setaffinity(0, {args.cpu})

    source, signature, case_count = conformance_corpus()
    ascii_source = b"a" * (1024 * 1024)
    corpora = {
        "unicode_conformance": {
            "bytes": source,
            "signature": signature,
            "description": "LineBreakTest.txt with BK case separators",
            "cases": case_count,
        },
        "ascii_letters": {
            "bytes": ascii_source,
            "signature": (1, len(ascii_source), len(ascii_source)),
            "description": "1 MiB lowercase ASCII letter run",
        },
    }
    benchmarks: dict[str, object] = {}
    corpus_document: dict[str, object] = {}
    for name, corpus in corpora.items():
        corpus_bytes = corpus["bytes"]
        corpus_signature = corpus["signature"]
        got, _ = invoke(binary, corpus_bytes, 0)
        expected = " ".join(str(value) for value in corpus_signature)
        if got != expected:
            raise BenchmarkFailure(
                f"{name} semantic signature mismatch: expected {expected}, got {got}"
            )
        benchmarks[name] = benchmark(
            binary,
            corpus_bytes,
            args.samples,
            args.target_seconds,
            corpus_signature[0],
        )
        corpus_document[name] = {
            key: value for key, value in corpus.items() if key != "bytes"
        }

    document: dict[str, object] = {
        "schema_version": 2,
        "unicode_version": "17.0.0",
        "uax14_revision": 55,
        "provenance": provenance(args),
        "corpora": corpus_document,
        "benchmarks": benchmarks,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")

    for name, result in benchmarks.items():
        print(
            f"line-break/{name}: {result['median_mb_s']:.3f} MB/s "
            f"(MAD {result['mad_mb_s']:.3f}, {result['samples']} samples)"
        )
    if args.baseline is not None:
        delta = compare_baseline(args.baseline, benchmarks["unicode_conformance"])
        print(f"same-machine Unicode-conformance baseline delta: {delta:+.2f}%")
    print(f"wrote {args.output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (BenchmarkFailure, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=os.sys.stderr)
        raise SystemExit(1)
