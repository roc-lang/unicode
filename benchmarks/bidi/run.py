#!/usr/bin/env python3
"""Measure retained UAX #9 paragraph analysis and per-line reordering."""

from __future__ import annotations

import argparse
import json
import subprocess
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
BENCH = Path(__file__).resolve().parent
BUILD = ROOT / ".roc-unicode-tmp" / "benchmarks" / "bidi"


def corpus(target_bytes: int) -> dict[str, str]:
    def repeated(unit: str) -> str:
        return unit * max(1, target_bytes // len(unit.encode("utf-8")))

    return {
        "ltr": repeated("The quick brown fox has 123 words. "),
        "mixed": repeated("abc אבג 123 العربية "),
        "neutrals-nsm": repeated("א(̀  ...  )ب "),
        "nested-isolates": repeated("a \u2068אב \u2066(12)\u2069\u2069 ب "),
        "overflow-controls": repeated(("\u202b" * 126) + "a" + ("\u202c" * 126)),
        "repeated-fsi": repeated("a \u2068אב \u2068(12)\u2069\u2069 ب "),
        "brackets": repeated("א([<abc>])ب "),
        "brackets-63": repeated(("(" * 63) + "a" + (")" * 63)),
        "brackets-64": repeated(("(" * 64) + "a" + (")" * 64)),
        "many-runs": repeated("aא1ب(a)2 "),
    }


def command(args: list[str], *, cwd: Path = ROOT) -> None:
    print("+", " ".join(args), flush=True)
    subprocess.run(args, cwd=cwd, check=True)


def verify_pinned_roc(roc: str) -> None:
    """Reject a benchmark run whose compiler is not the repository pin."""
    pin = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    if not pin:
        raise RuntimeError(".roc-version is empty")
    completed = subprocess.run([roc, "version"], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=True)
    observed = completed.stdout.strip()
    revision = pin.rsplit("-", 1)[-1]
    if pin not in observed and revision not in observed:
        raise RuntimeError(f"Roc compiler differs from .roc-version ({pin}): {observed}")


def build(roc: str, zig: str) -> Path:
    BUILD.mkdir(parents=True, exist_ok=True)
    command([zig, "build", "--build-file", "tests/platform/build.zig", "native", "-Doptimize=ReleaseFast"])
    source = BENCH / "main.roc"
    binary = BUILD / "bidi"
    command([roc, "check", str(source), "--no-cache"])
    command([roc, "build", str(source), "--opt=speed", f"--output={binary}", "--no-cache"])
    return binary


def measure(binary: Path, text: str, repeats: int, line_reorders: int) -> dict[str, float | int]:
    payload = f"{repeats}\t{line_reorders}\n{text}".encode("utf-8")
    started = time.perf_counter()
    completed = subprocess.run([str(binary)], input=payload, stdout=subprocess.PIPE, check=True)
    elapsed = time.perf_counter() - started
    checksum_text, byte_text = completed.stdout.decode("utf-8").strip().split("\t")
    checksum, byte_count = int(checksum_text), int(byte_text)
    if checksum <= 0 or byte_count != len(text.encode("utf-8")):
        raise RuntimeError(f"benchmark output drifted: {completed.stdout!r}")
    return {"bytes": byte_count, "checksum": checksum, "seconds": elapsed}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default="roc")
    parser.add_argument("--zig", default="zig")
    parser.add_argument("--sizes", default="4096,16384,65536", help="comma-separated corpus byte targets")
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--line-reorders", type=int, default=64, help="logical line partitions reordered per retained analysis")
    parser.add_argument("--output", type=Path, default=BUILD / "results.json")
    args = parser.parse_args()
    try:
        sizes = tuple(int(value) for value in args.sizes.split(","))
    except ValueError as error:
        parser.error(f"--sizes must be comma-separated positive integers: {error}")
    if not sizes or any(size < 1 for size in sizes) or args.repeats < 1 or args.line_reorders < 1:
        parser.error("--sizes, --repeats, and --line-reorders must be positive")
    verify_pinned_roc(args.roc)
    binary = build(args.roc, args.zig)
    results = {
        str(size): {
            name: measure(binary, text, args.repeats, args.line_reorders)
            for name, text in corpus(size).items()
        }
        for size in sizes
    }
    scaling = {}
    for smaller, larger in zip(sizes, sizes[1:]):
        scaling[f"{smaller}->{larger}"] = {
            name: round(results[str(larger)][name]["seconds"] / max(results[str(smaller)][name]["seconds"], 0.000001), 3)
            for name in corpus(smaller)
        }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(
            {
                "sizes": sizes,
                "repeats": args.repeats,
                "line_reorders": args.line_reorders,
                "cases": results,
                "scaling_seconds_ratio": scaling,
            },
            indent=2,
        )
        + "\n"
    )
    print(args.output)


if __name__ == "__main__":
    main()
