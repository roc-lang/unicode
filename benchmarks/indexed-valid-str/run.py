#!/usr/bin/env python3
"""Measure the valid-Str pull cursor and verify scalar coordinates."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import statistics
import subprocess
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
BENCH = Path(__file__).resolve().parent
BUILD = ROOT / ".roc-unicode-tmp" / "benchmarks" / "indexed-valid-str"
DEFAULT_OUTPUT = BUILD / "results.json"


def repeated(unit: str, target_bytes: int) -> bytes:
    encoded = unit.encode()
    return encoded * max(1, target_bytes // len(encoded))


def semantic_cases() -> dict[str, bytes]:
    return {
        "empty": b"",
        "one": b"A",
        "inline_8": b"a" * 8,
        "inline_23": b"a" * 23,
        "heap_24": b"b" * 24,
        "heap_25": b"c" * 25,
        "heap_64": b"d" * 64,
        "bmp": "AΩन日".encode(),
        "supplementary": "A𐐀𓀀𝄞".encode(),
        "mixed": "A\u0301 العربية नमस्ते 日本語 𐐀 👩🏽‍💻".encode(),
    }


def performance_cases(target_bytes: int) -> dict[str, bytes]:
    return {
        "ascii": repeated("The quick brown fox jumps over 13 lazy dogs. ", target_bytes),
        "combining": repeated("a\u0301e\u0327o\u0308 ", target_bytes),
        "multiscript": repeated("العربية नमस्ते 日本語 한국어 Ελληνικά ", target_bytes),
        "supplementary": repeated("𐐀𓀀𞤀𠀀𝄞 ", target_bytes),
        "mixed": repeated("Latin A\u0301 العربية नमस्ते 日本語 𐐀 👩🏽‍💻 ", target_bytes),
    }


def command(args: list[str], cwd: Path = ROOT) -> None:
    print("+", " ".join(args), flush=True)
    subprocess.run(args, cwd=cwd, check=True)


def build(roc: str, zig: str) -> dict[str, Path]:
    BUILD.mkdir(parents=True, exist_ok=True)
    command([zig, "build", "native", "-Doptimize=ReleaseFast"], ROOT / "tests/platform")
    binaries: dict[str, Path] = {}
    for name in ("full", "early", "grapheme", "slice"):
        source = BENCH / f"{name}.roc"
        binary = BUILD / name
        command([roc, "check", str(source), "--no-cache"])
        command([roc, "build", str(source), "--opt=speed", f"--output={binary}"])
        binaries[name] = binary
    return binaries


def affinity(cpu: int | None) -> list[str]:
    taskset = shutil.which("taskset")
    return [] if cpu is None or taskset is None else [taskset, "--cpu-list", str(cpu)]


def run_once(binary: Path, source: bytes, cpu: int | None) -> tuple[list[int], float]:
    start = time.perf_counter_ns()
    completed = subprocess.run(
        [*affinity(cpu), str(binary)],
        cwd=ROOT,
        input=source,
        stdout=subprocess.PIPE,
        check=True,
    )
    elapsed = (time.perf_counter_ns() - start) / 1_000_000_000
    values = [int(value) for value in completed.stdout.decode().strip().split("\t")]
    if len(values) != 7:
        raise RuntimeError(f"unexpected probe output: {completed.stdout!r}")
    return values, elapsed


def expected(source: bytes, limit: int) -> list[int]:
    text = source.decode()
    selected = text[:limit]
    byte_offset = 0
    scalar_sum = 0
    indexed_scalar_sum = 0
    byte_start_sum = 0
    byte_end_sum = 0
    scalar_index_sum = 0
    for index, scalar in enumerate(selected):
        encoded = scalar.encode()
        scalar_value = ord(scalar)
        scalar_sum += scalar_value
        indexed_scalar_sum += scalar_value * (index + 1)
        byte_start_sum += byte_offset
        byte_offset += len(encoded)
        byte_end_sum += byte_offset
        scalar_index_sum += index
    return [
        len(selected),
        scalar_sum,
        indexed_scalar_sum,
        byte_start_sum,
        byte_end_sum,
        scalar_index_sum,
    ]


def median_mad(values: list[float]) -> tuple[float, float]:
    median = statistics.median(values)
    return median, statistics.median(abs(value - median) for value in values)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default="zig")
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument("--target-bytes", type=int, default=1024 * 1024)
    parser.add_argument("--cpu", type=int, default=0)
    parser.add_argument("--no-affinity", action="store_true")
    parser.add_argument("--no-build", action="store_true")
    parser.add_argument("--baseline", type=Path)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()

    binaries = (
        {name: BUILD / name for name in ("full", "early", "grapheme", "slice")}
        if args.no_build
        else build(args.roc, args.zig)
    )
    cpu = None if args.no_affinity else args.cpu
    semantics: dict[str, object] = {}
    for name, source in semantic_cases().items():
        full, _ = run_once(binaries["full"], source, cpu)
        early, _ = run_once(binaries["early"], source, cpu)
        grapheme, _ = run_once(binaries["grapheme"], source, cpu)
        sliced, _ = run_once(binaries["slice"], b"x" + source, cpu)
        if full[:6] != expected(source, 2**63):
            raise RuntimeError(f"full signature mismatch for {name}: {full}")
        if early[:6] != expected(source, 3):
            raise RuntimeError(f"early signature mismatch for {name}: {early}")
        if sliced[:6] != expected(source, 2**63):
            raise RuntimeError(f"seamless-slice signature mismatch for {name}: {sliced}")
        if (
            grapheme[0] != grapheme[1]
            or grapheme[2] != grapheme[3]
            or grapheme[4] != grapheme[5]
        ):
            raise RuntimeError(f"grapheme iterator/list mismatch for {name}: {grapheme}")
        if len(source) >= 64 and (
            full[6] != 0
            or early[6] != 0
            or sliced[6] != 0
            or grapheme[6] != 0
        ):
            raise RuntimeError(f"heap-backed traversal allocated for {name}")
        semantics[name] = {
            "bytes": len(source),
            "full": full,
            "early": early,
            "slice": sliced,
            "grapheme": grapheme,
        }

    performance: dict[str, object] = {}
    for name, source in performance_cases(args.target_bytes).items():
        samples: list[float] = []
        signature: list[int] | None = None
        for _ in range(args.samples):
            observed, elapsed = run_once(binaries["full"], source, cpu)
            if observed[:6] != expected(source, 2**63):
                raise RuntimeError(f"performance signature mismatch for {name}")
            if observed[6] != 0:
                raise RuntimeError(f"performance traversal allocated for {name}")
            if signature is not None and observed != signature:
                raise RuntimeError(f"nondeterministic signature for {name}")
            signature = observed
            samples.append(elapsed)
        median, mad = median_mad(samples)
        performance[name] = {
            "bytes": len(source),
            "signature": signature,
            "median_seconds": median,
            "mad_seconds": mad,
            "decimal_mb_per_second": len(source) / median / 1_000_000,
            "samples_seconds": samples,
        }

    report: dict[str, object] = {
        "schema": 1,
        "roc_version": subprocess.run(
            [args.roc, "version"], text=True, stdout=subprocess.PIPE, check=True
        ).stdout.strip(),
        "samples": args.samples,
        "cpu_affinity": cpu if affinity(cpu) else None,
        "binary_bytes": {name: path.stat().st_size for name, path in binaries.items()},
        "semantics": semantics,
        "performance": performance,
    }
    if args.baseline:
        baseline = json.loads(args.baseline.read_text())
        for name, result in semantics.items():
            if baseline["semantics"][name]["full"][:6] != result["full"][:6]:
                raise RuntimeError(f"baseline full signature mismatch for {name}")
            if baseline["semantics"][name]["early"][:6] != result["early"][:6]:
                raise RuntimeError(f"baseline early signature mismatch for {name}")
            if baseline["semantics"][name]["slice"][:6] != result["slice"][:6]:
                raise RuntimeError(f"baseline seamless-slice signature mismatch for {name}")
            if baseline["semantics"][name]["grapheme"][:6] != result["grapheme"][:6]:
                raise RuntimeError(f"baseline grapheme signature mismatch for {name}")
        report["baseline_ratios"] = {
            name: baseline["performance"][name]["median_seconds"]
            / result["median_seconds"]
            for name, result in performance.items()
        }
    output = args.output.expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    print(f"wrote {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
