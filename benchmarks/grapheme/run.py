#!/usr/bin/env python3
"""Build and benchmark equivalent allocation-free grapheme counts."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import platform
import shutil
import statistics
import subprocess
import sys
import tarfile
import time
import urllib.request
from pathlib import Path
from typing import Sequence


ROOT = Path(__file__).resolve().parents[2]
BENCH = Path(__file__).resolve().parent
BUILD = ROOT / ".roc-unicode-tmp" / "benchmarks" / "grapheme"
DEFAULT_OUTPUT = BUILD / "results.json"
DEFAULT_TARGET_BYTES = 1024 * 1024
DEFAULT_TARGET_SECONDS = 0.40
DEFAULT_SAMPLES = 9
MAX_REPEATS = 99_999_999
LIBGRAPHEME_VERSION = "3.0.0"
LIBGRAPHEME_URL = (
    "https://dl.suckless.org/libgrapheme/"
    f"libgrapheme-{LIBGRAPHEME_VERSION}.tar.gz"
)
LIBGRAPHEME_SHA256 = "32585af73dda62fbcc0fed14f199aa1bc988ad01dad0bfbd06cf175d9cf3d68c"
CASE_NAMES = (
    "ascii",
    "latin_combining",
    "multilingual",
    "emoji_sequences",
    "unicode17_conformance",
)


class BenchmarkFailure(RuntimeError):
    pass


def command(
    args: Sequence[str],
    *,
    cwd: Path = ROOT,
) -> None:
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


def boundary_signature(ends: Sequence[int]) -> tuple[int, int, int]:
    return (
        len(ends),
        sum(ends),
        sum(index * end for index, end in enumerate(ends, start=1)),
    )


def conformance_corpus() -> tuple[bytes, tuple[int, int, int]]:
    path = ROOT / "vendor/unicode/17.0.0/GraphemeBreakTest.txt"
    corpus = bytearray()
    ends: list[int] = []
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        tokens = body.split()
        # A control separator guarantees a boundary between adjacent test cases.
        corpus.extend(b"\0")
        ends.append(len(corpus))
        for index in range(1, len(tokens), 2):
            corpus.extend(chr(int(tokens[index], 16)).encode("utf-8"))
            if tokens[index + 1] == "÷":
                ends.append(len(corpus))
    return bytes(corpus), boundary_signature(ends)


def corpora(
    target_bytes: int,
) -> tuple[dict[str, bytes], dict[str, tuple[int, int, int]]]:
    conformance, expected = conformance_corpus()
    cases = {
        "ascii": repeated("The quick brown fox jumps over 13 lazy dogs. ", target_bytes),
        "latin_combining": repeated(
            "Cafe\u0301 nai\u0308ve coo\u0308perate re\u0301sume\u0301 ", target_bytes
        ),
        "multilingual": repeated(
            "Καλημέρα κόσμε Привет мир "
            "مرحبا بالعالم नमस्ते दुनिया "
            "日本語の文章 한국어 문장 "
            "สวัสดีชาวโลก ",
            target_bytes,
        ),
        "emoji_sequences": repeated(
            "👨‍👩‍👧‍👦 👩🏽‍💻 🏳️‍🌈 🇦🇺 "
            "1️⃣ 👋🏿 ❤️‍🔥 🧑‍🚀 ",
            target_bytes,
        ),
        "unicode17_conformance": conformance,
    }
    return cases, {"unicode17_conformance": expected}


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def libgrapheme_source() -> Path:
    archive = BUILD / f"libgrapheme-{LIBGRAPHEME_VERSION}.tar.gz"
    source = BUILD / f"libgrapheme-{LIBGRAPHEME_VERSION}"
    if source.is_dir():
        return source

    if not archive.is_file():
        print(f"+ download {LIBGRAPHEME_URL}", flush=True)
        partial = archive.with_suffix(archive.suffix + ".partial")
        try:
            with urllib.request.urlopen(LIBGRAPHEME_URL) as response:
                with partial.open("wb") as output:
                    shutil.copyfileobj(response, output)
            partial.replace(archive)
        finally:
            partial.unlink(missing_ok=True)
    actual_sha256 = file_sha256(archive)
    if actual_sha256 != LIBGRAPHEME_SHA256:
        raise BenchmarkFailure(
            f"{archive} has SHA-256 {actual_sha256}, expected {LIBGRAPHEME_SHA256}"
        )

    with tarfile.open(archive, "r:gz") as package:
        for member in package.getmembers():
            member_path = Path(member.name)
            if member_path.is_absolute() or ".." in member_path.parts:
                raise BenchmarkFailure(f"unsafe path in {archive}: {member.name}")
            if member.issym() or member.islnk():
                raise BenchmarkFailure(f"refusing link in {archive}: {member.name}")
        package.extractall(BUILD)
    if not source.is_dir():
        raise BenchmarkFailure(f"{archive} did not contain {source.name}")
    return source


def build(roc: str, go: str, cargo: str, zig: str, cc: str, make: str) -> dict[str, Path]:
    BUILD.mkdir(parents=True, exist_ok=True)
    command([zig, "build", "native", "-Doptimize=ReleaseFast"], cwd=ROOT / "tests/platform")
    roc_source = BENCH / "roc/main.roc"
    roc_binary = BUILD / "roc"
    command([roc, "check", str(roc_source)])
    command([roc, "build", str(roc_source), "--opt=speed", f"--output={roc_binary}"])

    rust_target = BUILD / "rust-target"
    command(
        [
            cargo,
            "build",
            "--release",
            "--locked",
            "--manifest-path",
            str(BENCH / "rust/Cargo.toml"),
            "--target-dir",
            str(rust_target),
        ]
    )

    go_binary = BUILD / "go"
    command(
        [go, "build", "-trimpath", "-ldflags=-s -w", "-o", str(go_binary), "."],
        cwd=BENCH / "go",
    )

    libgrapheme = libgrapheme_source()
    command([make, "gen/character.h"], cwd=libgrapheme)
    c_binary = BUILD / "c-libgrapheme"
    command(
        [
            cc,
            "-std=c99",
            "-O3",
            "-DNDEBUG",
            "-flto",
            "-I",
            str(libgrapheme),
            str(BENCH / "c/main.c"),
            str(libgrapheme / "src/character.c"),
            str(libgrapheme / "src/utf8.c"),
            str(libgrapheme / "src/util.c"),
            "-o",
            str(c_binary),
        ]
    )
    return {
        "roc": roc_binary,
        "rust_unicode_segmentation": rust_target / "release/rust-unicode-segmentation",
        "rust_icu4x": rust_target / "release/rust-icu4x",
        "go_clipperhouse": go_binary,
        "c_libgrapheme": c_binary,
    }


def choose_cpu(requested: int | None, no_affinity: bool) -> int | None:
    if no_affinity:
        return None
    if shutil.which("taskset") is None or not hasattr(os, "sched_getaffinity"):
        print("CPU affinity unavailable; continuing without pinning", file=sys.stderr)
        return None
    available = os.sched_getaffinity(0)
    cpu = min(available) if requested is None else requested
    if cpu not in available:
        raise BenchmarkFailure(f"CPU {cpu} is unavailable; allowed CPUs are {sorted(available)}")
    return cpu


def invoke(binary: Path, corpus: bytes, repeats: int, cpu: int | None) -> tuple[float, int]:
    if not 1 <= repeats <= MAX_REPEATS:
        raise BenchmarkFailure(f"repeat count {repeats} is outside 1..{MAX_REPEATS}")
    payload = f"{repeats:08d}\n".encode("ascii") + corpus
    args = [str(binary)]
    if cpu is not None:
        args = ["taskset", "-c", str(cpu), *args]
    started = time.perf_counter_ns()
    result = subprocess.run(
        args,
        input=payload,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )
    elapsed = (time.perf_counter_ns() - started) / 1e9
    try:
        checksum = int(result.stdout.strip())
    except ValueError as error:
        raise BenchmarkFailure(
            f"{binary} returned {result.stdout!r}; "
            f"stderr={result.stderr.decode(errors='replace')!r}"
        ) from error
    return elapsed, checksum


def verify_boundaries(
    binary: Path, corpus: bytes, cpu: int | None
) -> tuple[int, int, int]:
    payload = b"00000000\n" + corpus
    args = [str(binary)]
    if cpu is not None:
        args = ["taskset", "-c", str(cpu), *args]
    result = subprocess.run(
        args,
        input=payload,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )
    try:
        count, sum_ends, weighted_ends = (
            int(part) for part in result.stdout.decode("ascii").split()
        )
    except (UnicodeDecodeError, ValueError) as error:
        raise BenchmarkFailure(
            f"{binary} returned malformed boundary signature {result.stdout!r}; "
            f"stderr={result.stderr.decode(errors='replace')!r}"
        ) from error
    return count, sum_ends, weighted_ends


def checked_invoke(
    binary: Path,
    corpus: bytes,
    repeats: int,
    cpu: int | None,
    one_pass_checksum: int,
) -> float:
    elapsed, got = invoke(binary, corpus, repeats, cpu)
    expected = one_pass_checksum * repeats
    if got != expected:
        raise BenchmarkFailure(f"{binary}: checksum {got}, expected {expected}")
    return elapsed


def calibrate(
    binary: Path,
    corpus: bytes,
    cpu: int | None,
    target_seconds: float,
) -> tuple[int, int]:
    elapsed, checksum = invoke(binary, corpus, 1, cpu)
    repeats = max(1, min(MAX_REPEATS, math.ceil(target_seconds / max(elapsed, 1e-6))))
    for _ in range(3):
        elapsed = checked_invoke(binary, corpus, repeats, cpu, checksum)
        if elapsed >= target_seconds * 0.8:
            break
        repeats = max(
            repeats + 1,
            min(MAX_REPEATS, math.ceil(repeats * target_seconds / max(elapsed, 1e-6))),
        )
    return repeats, checksum


def benchmark(
    binaries: dict[str, Path],
    cases: dict[str, bytes],
    expected: dict[str, tuple[int, int, int]],
    *,
    cpu: int | None,
    samples_count: int,
    target_seconds: float,
) -> dict[str, dict]:
    results: dict[str, dict] = {}
    label_width = max(len(label) for label in binaries)
    for case_name, corpus in cases.items():
        print(f"\n[{case_name}] {len(corpus):,} bytes", flush=True)
        case_results: dict[str, dict] = {}
        one_pass_counts: dict[str, int] = {}
        signatures: dict[str, tuple[int, int, int]] = {}
        for language, binary in binaries.items():
            signatures[language] = verify_boundaries(binary, corpus, cpu)
            repeats, checksum = calibrate(binary, corpus, cpu, target_seconds)
            one_pass_counts[language] = checksum
            checked_invoke(binary, corpus, repeats, cpu, checksum)
            samples = []
            for _ in range(samples_count):
                elapsed = checked_invoke(binary, corpus, repeats, cpu, checksum)
                samples.append((len(corpus) * repeats / 1_000_000) / elapsed)
            median = statistics.median(samples)
            mad = statistics.median(abs(value - median) for value in samples)
            case_results[language] = {
                "clusters": checksum,
                "repeats": repeats,
                "mb_per_second_median": median,
                "mb_per_second_mad": mad,
                "samples_mb_per_second": samples,
            }
            print(
                f"  {language:{label_width}s} {median:9.1f} MB/s  "
                f"MAD {mad:5.1f}  n={samples_count}",
                flush=True,
            )

        if len(set(one_pass_counts.values())) != 1:
            raise BenchmarkFailure(
                f"{case_name}: implementations disagree on cluster counts: {one_pass_counts}"
            )
        if len(set(signatures.values())) != 1:
            raise BenchmarkFailure(
                f"{case_name}: implementations disagree on boundary positions: {signatures}"
            )
        if case_name in expected and next(iter(signatures.values())) != expected[case_name]:
            raise BenchmarkFailure(
                f"{case_name}: Unicode 17 expected boundary signature "
                f"{expected[case_name]}, got {signatures}"
            )
        results[case_name] = {"bytes": len(corpus), "implementations": case_results}
    return results


def cpu_model() -> str:
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.is_file():
        for line in cpuinfo.read_text(encoding="utf-8", errors="replace").splitlines():
            if line.startswith("model name"):
                return line.split(":", 1)[1].strip()
    return platform.processor() or "unknown"


def read_baseline(path: Path) -> dict:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise BenchmarkFailure(f"unable to read baseline {path}: {error}") from error
    if not isinstance(value, dict) or not isinstance(value.get("benchmark"), dict):
        raise BenchmarkFailure(f"baseline {path} has no benchmark object")
    return value


def case_implementations(case: dict) -> dict[str, dict]:
    implementations = case.get("implementations")
    if not isinstance(implementations, dict):
        raise BenchmarkFailure("malformed benchmark case")
    return implementations


def print_comparison(current: dict, baseline: dict, baseline_path: Path) -> None:
    baseline_cases = baseline["benchmark"].get("cases")
    current_cases = current["benchmark"].get("cases")
    if not isinstance(baseline_cases, dict) or not isinstance(current_cases, dict):
        raise BenchmarkFailure("current result or baseline has no benchmark cases")

    baseline_configuration = baseline.get("configuration")
    current_configuration = current.get("configuration")
    if (
        isinstance(baseline_configuration, dict)
        and isinstance(current_configuration, dict)
        and baseline_configuration.get("target_corpus_bytes")
        != current_configuration.get("target_corpus_bytes")
    ):
        print("WARNING: baseline and current target corpus sizes differ", file=sys.stderr)

    print(f"\nComparison with {baseline_path}:")
    print(f"{'case':28s} {'implementation':27s} {'baseline':>11s} {'current':>11s} {'delta':>9s}")
    for case_name, current_case in current_cases.items():
        baseline_case = baseline_cases.get(case_name)
        if not isinstance(baseline_case, dict):
            continue
        if (
            "bytes" in baseline_case
            and "bytes" in current_case
            and baseline_case["bytes"] != current_case["bytes"]
        ):
            print(f"WARNING: {case_name} corpus sizes differ", file=sys.stderr)
        current_impls = case_implementations(current_case)
        baseline_impls = case_implementations(baseline_case)
        for language, current_impl in current_impls.items():
            baseline_impl = baseline_impls.get(language)
            if not isinstance(baseline_impl, dict):
                continue
            before = float(baseline_impl["mb_per_second_median"])
            after = float(current_impl["mb_per_second_median"])
            delta = (after / before - 1.0) * 100.0
            print(
                f"{case_name:28s} {language:27s} "
                f"{before:9.1f} MB/s {after:9.1f} MB/s {delta:+8.1f}%"
            )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    parser.add_argument("--go", default="go")
    parser.add_argument("--cargo", default="cargo")
    parser.add_argument("--zig", default="zig")
    parser.add_argument("--cc", default="cc")
    parser.add_argument("--make", default="make")
    parser.add_argument("--skip-build", action="store_true")
    parser.add_argument("--samples", type=int, default=DEFAULT_SAMPLES)
    parser.add_argument("--target-seconds", type=float, default=DEFAULT_TARGET_SECONDS)
    parser.add_argument("--corpus-bytes", type=int, default=DEFAULT_TARGET_BYTES)
    parser.add_argument("--case", action="append", choices=CASE_NAMES, dest="cases")
    parser.add_argument("--cpu", type=int)
    parser.add_argument("--no-affinity", action="store_true")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--baseline", type=Path)
    args = parser.parse_args()
    if args.samples < 1:
        parser.error("--samples must be positive")
    if args.target_seconds <= 0:
        parser.error("--target-seconds must be positive")
    if args.corpus_bytes < 1:
        parser.error("--corpus-bytes must be positive")
    if args.cpu is not None and args.no_affinity:
        parser.error("--cpu and --no-affinity cannot be combined")
    return args


def main() -> None:
    args = parse_args()
    args.roc = resolve_tool(args.roc)
    args.go = resolve_tool(args.go)
    args.cargo = resolve_tool(args.cargo)
    args.zig = resolve_tool(args.zig)
    args.cc = resolve_tool(args.cc)
    args.make = resolve_tool(args.make)
    pinned_roc = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    actual_roc = capture([args.roc, "version"])
    if pinned_roc not in actual_roc:
        raise BenchmarkFailure(
            f"benchmark requires {pinned_roc}, got {actual_roc!r}"
        )
    binaries = (
        {
            "roc": BUILD / "roc",
            "rust_unicode_segmentation": BUILD
            / "rust-target/release/rust-unicode-segmentation",
            "rust_icu4x": BUILD / "rust-target/release/rust-icu4x",
            "go_clipperhouse": BUILD / "go",
            "c_libgrapheme": BUILD / "c-libgrapheme",
        }
        if args.skip_build
        else build(args.roc, args.go, args.cargo, args.zig, args.cc, args.make)
    )
    missing = [str(path) for path in binaries.values() if not path.is_file()]
    if missing:
        raise BenchmarkFailure(f"benchmark binaries are missing: {missing}")

    all_cases, all_expected = corpora(args.corpus_bytes)
    selected_names = args.cases or list(CASE_NAMES)
    cases = {name: all_cases[name] for name in selected_names}
    expected = {name: all_expected[name] for name in selected_names if name in all_expected}
    cpu = choose_cpu(args.cpu, args.no_affinity)
    status = capture(["git", "status", "--porcelain"])
    report = {
        "schema_version": 1,
        "system": {
            "platform": platform.platform(),
            "cpu_model": cpu_model(),
            "cpu_affinity": cpu,
        },
        "source": {
            "commit": capture(["git", "rev-parse", "HEAD"]),
            "dirty": bool(status),
        },
        "tools": {
            "roc": actual_roc,
            "rustc": capture(["rustc", "--version"]),
            "cargo": capture([args.cargo, "--version"]),
            "go": capture([args.go, "version"]),
            "zig": capture([args.zig, "version"]),
            "cc": capture([args.cc, "--version"]),
        },
        "libraries": {
            "unicode_segmentation": "1.13.3 (Unicode 17)",
            "icu_segmenter": "2.2.0 (Unicode 17)",
            "clipperhouse_uax29": "2.7.0 (Unicode 17)",
            "libgrapheme": "3.0.0 (Unicode 17)",
        },
        "configuration": {
            "samples": args.samples,
            "target_seconds": args.target_seconds,
            "target_corpus_bytes": args.corpus_bytes,
        },
        "benchmark": {
            "unit": "decimal MB/s",
            "cases": benchmark(
                binaries,
                cases,
                expected,
                cpu=cpu,
                samples_count=args.samples,
                target_seconds=args.target_seconds,
            ),
        },
    }

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"\nwrote {args.output}")
    if args.baseline is not None:
        print_comparison(report, read_baseline(args.baseline), args.baseline)


if __name__ == "__main__":
    try:
        main()
    except (BenchmarkFailure, OSError, subprocess.CalledProcessError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        raise SystemExit(1) from error
