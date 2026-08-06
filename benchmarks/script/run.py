#!/usr/bin/env python3
"""Validate and benchmark shaping-oriented Script itemization."""

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
BUILD = ROOT / ".roc-unicode-tmp" / "benchmarks" / "script"
DEFAULT_OUTPUT = BUILD / "results.json"
DEFAULT_TARGET_BYTES = 128 * 1024
DEFAULT_TARGET_SECONDS = 0.40
DEFAULT_SAMPLES = 9
MAX_REPEATS = 99_999_999
U64_MASK = (1 << 64) - 1


class BenchmarkFailure(RuntimeError):
    pass


def command(args: Sequence[object], *, cwd: Path = ROOT) -> None:
    printable = [str(arg) for arg in args]
    print("+", " ".join(printable), flush=True)
    subprocess.run(printable, cwd=cwd, check=True)


def capture(args: Sequence[object], *, cwd: Path = ROOT) -> str:
    return subprocess.run(
        [str(arg) for arg in args],
        cwd=cwd,
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()


def repeat_to_size(unit: str, target_bytes: int) -> bytes:
    encoded = unit.encode("utf-8")
    return encoded * max(1, target_bytes // len(encoded))


def corpora(target_bytes: int) -> dict[str, bytes]:
    return {
        "ascii": repeat_to_size(
            "The quick brown fox jumps over 13 lazy dogs. ", target_bytes
        ),
        "combining": repeat_to_size(
            "A\u0301 e\u0327\u0301 n\u0303 Κα\u0301 عربي\u0651 दे\u093c ",
            target_bytes,
        ),
        "multi_scx": repeat_to_size(
            "あーい アーイ ひらがな・カタカナ ا،ب १।२ ", target_bytes
        ),
        "alternating": repeat_to_size(
            "aαaЖaאaشaकaあa한 ", target_bytes
        ),
        "multilingual": repeat_to_size(
            "Latin A\u0301 العربية नमस्ते 日本語 あーい アーイ αβγ "
            "𐐀𓀀 👩🏽‍💻  ",
            target_bytes,
        ),
        "long_neutral": repeat_to_size(
            "a" + (" " * 512) + "α" + ("." * 512) + "a", target_bytes
        ),
    }


def build(roc: str, zig: str) -> dict[str, Path]:
    BUILD.mkdir(parents=True, exist_ok=True)
    command([
        zig,
        "build",
        "--build-file",
        "tests/platform/build.zig",
        "native",
        "-Doptimize=ReleaseFast",
    ])
    binaries: dict[str, Path] = {}
    for name in ("performance", "semantic", "allocation", "parity"):
        source = BENCH / f"{name}.roc"
        binary = BUILD / name
        command([roc, "check", source, "--no-cache"])
        command([
            roc,
            "build",
            source,
            "--opt=speed",
            f"--output={binary}",
            "--no-cache",
            "-j1",
        ])
        binaries[name] = binary
    return binaries


def run_output(binary: Path, source: bytes, *, timeout: float = 15.0) -> str:
    completed = subprocess.run(
        [str(binary)],
        cwd=ROOT,
        input=source,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
        timeout=timeout,
    )
    if completed.returncode != 0:
        raise BenchmarkFailure(
            f"{binary.name} exited {completed.returncode}: "
            f"{completed.stderr.decode('utf-8', errors='replace').strip()}"
        )
    return completed.stdout.decode("utf-8").strip()


def invoke(binary: Path, source: bytes, repeats: int) -> tuple[str, float]:
    payload = f"{repeats:08d}\n".encode("ascii") + source
    started = time.perf_counter()
    output = run_output(binary, payload, timeout=120.0)
    return output, time.perf_counter() - started


def parse_signature(output: str) -> tuple[int, int, int, int, int, int]:
    fields = output.split("\t")
    if len(fields) != 6:
        raise BenchmarkFailure(f"malformed semantic signature: {output!r}")
    try:
        signature = tuple(int(field) for field in fields)
    except ValueError as error:
        raise BenchmarkFailure(f"non-numeric semantic signature: {output!r}") from error
    return signature  # type: ignore[return-value]


def expected_timed_checksum(output: str, *, per_scan: int, repeats: int) -> None:
    expected = (per_scan * repeats) & U64_MASK
    if output != str(expected):
        raise BenchmarkFailure(
            f"timed checksum mismatch: expected {expected}, got {output!r}"
        )


def validate_semantics(binaries: dict[str, Path], target_bytes: int) -> dict[str, object]:
    expected_semantic = (
        "aliases=true;recursive_is_rejected=true\n"
        "right=0-1:0-1:Latn,1-2:1-2:Zyyy,2-5:2-3:Zzzz\n"
        "right_cursor=0-1:0-1:Latn,1-2:1-2:Zyyy,2-5:2-3:Zzzz\n"
        "left=0-3:0-1:Zzzz,3-4:1-2:Zyyy,4-5:2-3:Latn\n"
        "left_cursor=0-3:0-1:Zzzz,3-4:1-2:Zyyy,4-5:2-3:Latn\n"
        "parity=true"
    )
    semantic = run_output(binaries["semantic"], b"Greek")
    if semantic != expected_semantic:
        raise BenchmarkFailure(f"Script semantic probe drifted:\n{semantic}")

    alias = run_output(binaries["allocation"], b"AisGreek")
    if alias != "allocations=0;checksum=4":
        raise BenchmarkFailure(f"dynamic Script alias lookup regressed: {alias!r}")

    allocation_lengths = (23, 24, 31, 32, 64, 256)
    allocations: dict[str, int] = {}
    for length in allocation_lengths:
        output = run_output(binaries["allocation"], b"I" + (b"a" * length))
        fields = dict(field.split("=", 1) for field in output.split(";"))
        count = int(fields.get("allocations", "-1"))
        if int(fields.get("checksum", "0")) == 0 or count not in (0, 1):
            raise BenchmarkFailure(
                f"itemization allocation/progress probe failed at {length}: {output!r}"
            )
        allocations[str(length)] = count
    if allocations["64"] != 0 or allocations["256"] != 0:
        raise BenchmarkFailure(
            f"heap-backed itemization must remain allocation-free: {allocations!r}"
        )

    progress: dict[str, str] = {}
    for length in (23, 24, 31, 32, target_bytes):
        source = (b"a b." * ((length + 3) // 4))[:length]
        output, _ = invoke(binaries["performance"], source, 0)
        signature = parse_signature(output)
        if signature[0] != 1 or signature[1] != length or signature[3] != length:
            raise BenchmarkFailure(
                f"indexed progress invariant failed at {length}: {signature!r}"
            )
        progress[str(length)] = output

    corpus_parity: dict[str, str] = {}
    for name, source in corpora(target_bytes).items():
        output = run_output(binaries["parity"], source)
        lines = output.splitlines()
        if (
            len(lines) != 3
            or not lines[0].startswith("complete=")
            or not lines[1].startswith("cursor=")
            or lines[2] != "parity=true"
            or lines[0][len("complete=") :] != lines[1][len("cursor=") :]
        ):
            raise BenchmarkFailure(
                f"complete/cursor parity failed for {name}: {output!r}"
            )
        parse_signature(lines[0][len("complete=") :])
        corpus_parity[name] = lines[0][len("complete=") :]

    return {
        "semantic_probe": semantic,
        "alias_allocation": alias,
        "itemization_allocations": allocations,
        "progress_signatures": progress,
        "corpus_parity": corpus_parity,
    }


def calibrate(
    binary: Path,
    source: bytes,
    target_seconds: float,
    per_scan_checksum: int,
) -> int:
    repeats = 1
    while True:
        output, elapsed = invoke(binary, source, repeats)
        expected_timed_checksum(output, per_scan=per_scan_checksum, repeats=repeats)
        if elapsed >= target_seconds or repeats >= MAX_REPEATS:
            return repeats
        scale = max(2, min(10, int(target_seconds / max(elapsed, 0.000001))))
        repeats = min(MAX_REPEATS, repeats * scale)


def benchmark_case(
    binary: Path,
    source: bytes,
    samples: int,
    target_seconds: float,
) -> dict[str, object]:
    signature_output, _ = invoke(binary, source, 0)
    signature = parse_signature(signature_output)
    repeats = calibrate(binary, source, target_seconds, signature[-1])
    rates: list[float] = []
    for _ in range(samples):
        output, elapsed = invoke(binary, source, repeats)
        expected_timed_checksum(output, per_scan=signature[-1], repeats=repeats)
        rates.append(len(source) * repeats / elapsed / 1_000_000)
    median = statistics.median(rates)
    return {
        "bytes": len(source),
        "repeats": repeats,
        "samples": samples,
        "signature": signature_output,
        "runs_per_scan": signature[0],
        "median_mb_s": round(median, 6),
        "mad_mb_s": round(
            statistics.median(abs(value - median) for value in rates), 6
        ),
        "sample_mb_s": [round(value, 6) for value in rates],
    }


def sha256_path(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def executable_provenance(command_name: str) -> dict[str, str]:
    found = shutil.which(command_name)
    path = Path(found if found is not None else command_name).resolve()
    if not path.is_file():
        raise BenchmarkFailure(f"executable not found: {command_name}")
    return {"path": str(path), "sha256": sha256_path(path)}


def cpu_model() -> str:
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.exists():
        for line in cpuinfo.read_text(encoding="utf-8", errors="replace").splitlines():
            if line.startswith("model name"):
                return line.split(":", 1)[-1].strip()
    return platform.processor()


def provenance(args: argparse.Namespace, binaries: dict[str, Path]) -> dict[str, object]:
    manifest_path = ROOT / "vendor/unicode/manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    unicode_sources: dict[str, object] = {}
    for key in ("scripts", "script_extensions", "property_value_aliases"):
        entry = manifest["sources"][key]
        path = ROOT / entry["path"]
        unicode_sources[key] = {
            "path": entry["path"],
            "manifest_sha256": entry["sha256"],
            "actual_sha256": sha256_path(path),
        }
    benchmark_sources = {
        str(path.relative_to(ROOT)): sha256_path(path)
        for path in (
            Path(__file__),
            BENCH / "performance.roc",
            BENCH / "semantic.roc",
            BENCH / "allocation.roc",
            BENCH / "parity.roc",
            ROOT / "tests/platform/build.zig",
        )
    }
    affinity = sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else None
    return {
        "git": {
            "commit": capture(["git", "rev-parse", "HEAD"]),
            "dirty": bool(capture(["git", "status", "--porcelain=v1"])),
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
            "unicode": unicode_sources,
            "benchmark": benchmark_sources,
        },
        "binaries": {
            name: {"bytes": path.stat().st_size, "sha256": sha256_path(path)}
            for name, path in binaries.items()
        },
    }


def compare_baseline(path: Path, benchmarks: dict[str, object]) -> dict[str, float]:
    try:
        baseline = json.loads(path.read_text(encoding="utf-8"))["benchmarks"]
        deltas: dict[str, float] = {}
        for name, current in benchmarks.items():
            previous = baseline[name]
            if previous["signature"] != current["signature"]:
                raise BenchmarkFailure(f"baseline semantic signature differs for {name}")
            deltas[name] = (
                float(current["median_mb_s"]) / float(previous["median_mb_s"]) - 1.0
            ) * 100.0
        return deltas
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
        if isinstance(error, BenchmarkFailure):
            raise
        raise BenchmarkFailure(f"invalid baseline {path}: {error}") from error


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--zig", default=os.environ.get("ZIG", "zig"))
    parser.add_argument("--samples", type=int, default=DEFAULT_SAMPLES)
    parser.add_argument("--target-bytes", type=int, default=DEFAULT_TARGET_BYTES)
    parser.add_argument("--target-seconds", type=float, default=DEFAULT_TARGET_SECONDS)
    parser.add_argument("--cpu", type=int)
    parser.add_argument("--case", action="append", choices=tuple(corpora(1)))
    parser.add_argument("--no-build", action="store_true")
    parser.add_argument("--validate-only", action="store_true")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--baseline", type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.samples < 1 or args.target_bytes < 32 or args.target_seconds <= 0:
        raise BenchmarkFailure("samples/target-bytes/target-seconds are out of range")
    pinned_roc = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    actual_roc = capture([args.roc, "version"])
    if pinned_roc not in actual_roc:
        raise BenchmarkFailure(
            f"benchmark requires {pinned_roc}, got {actual_roc!r}"
        )

    command([sys.executable, "scripts/unicode_data.py", "validate"])
    binaries = (
        {
            name: BUILD / name
            for name in ("performance", "semantic", "allocation", "parity")
        }
        if args.no_build
        else build(args.roc, args.zig)
    )
    for binary in binaries.values():
        if not binary.is_file():
            raise BenchmarkFailure(f"missing benchmark binary: {binary}")

    validation = validate_semantics(binaries, args.target_bytes)
    if args.validate_only:
        print(json.dumps({"validation": validation}, indent=2))
        return 0

    if args.cpu is not None:
        if not hasattr(os, "sched_setaffinity"):
            raise BenchmarkFailure("--cpu is not supported on this platform")
        available = os.sched_getaffinity(0)
        if args.cpu not in available:
            raise BenchmarkFailure(
                f"CPU {args.cpu} is outside current affinity {sorted(available)}"
            )
        os.sched_setaffinity(0, {args.cpu})

    selected = corpora(args.target_bytes)
    if args.case:
        selected = {name: selected[name] for name in args.case}
    benchmarks = {
        name: benchmark_case(
            binaries["performance"], source, args.samples, args.target_seconds
        )
        for name, source in selected.items()
    }
    document: dict[str, object] = {
        "schema_version": 2,
        "unicode_version": "17.0.0",
        "uax24_revision": 39,
        "policy": "ConservativeScxV1",
        "provenance": provenance(args, binaries),
        "validation": validation,
        "benchmarks": benchmarks,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")

    for name, result in benchmarks.items():
        print(
            f"script/{name}: {result['median_mb_s']:.3f} MB/s "
            f"(MAD {result['mad_mb_s']:.3f}, {result['samples']} samples, "
            f"{result['runs_per_scan']} runs)"
        )
    if args.baseline is not None:
        for name, delta in compare_baseline(args.baseline, benchmarks).items():
            print(f"same-machine {name} baseline delta: {delta:+.2f}%")
    print(f"wrote {args.output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (BenchmarkFailure, subprocess.CalledProcessError, subprocess.TimeoutExpired) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1) from error
