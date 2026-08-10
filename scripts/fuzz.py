#!/usr/bin/env python3
"""Build and operate roc-unicode's coverage-guided fuzz targets."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import re
import shutil
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
PERSISTENT_CORPUS_ROOT = WORK_ROOT / "corpus"
SEED_PATH = FUZZ_ROOT / "seeds.json"
NAME_PATTERN = re.compile(r"[a-z0-9]+(?:-[a-z0-9]+)*\Z")


class FuzzFailure(RuntimeError):
    pass


@dataclass(frozen=True)
class Target:
    name: str
    source: Path
    max_input_size: int

    @property
    def binary(self) -> Path:
        suffix = ".exe" if os.name == "nt" else ""
        return BIN_ROOT / f"unicode-{self.name}{suffix}"


TARGETS = {
    "utf8": Target("utf8", FUZZ_ROOT / "utf8.roc", 512),
    "grapheme": Target("grapheme", FUZZ_ROOT / "grapheme.roc", 384),
}


def command(
    args: Sequence[str | Path],
    *,
    cwd: Path = ROOT,
    capture: bool = False,
) -> subprocess.CompletedProcess[str]:
    values = [str(value) for value in args]
    print("+", " ".join(values), flush=True)
    completed = subprocess.run(
        values,
        cwd=cwd,
        text=True,
        encoding="utf-8",
        errors="replace",
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
        check=False,
    )
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
            "roc-fuzz 0.2.1 supports Linux x86-64 and Apple Silicon macOS; "
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
    if not isinstance(data, dict) or set(data) != {"schema_version", "utf8", "grapheme"}:
        raise FuzzFailure("fuzz seed manifest fields must be schema_version, utf8, grapheme")
    if data["schema_version"] != 1:
        raise FuzzFailure("unsupported fuzz seed manifest schema")

    result: dict[str, list[tuple[str, bytes]]] = {}
    for target_name in TARGETS:
        entries = data[target_name]
        if not isinstance(entries, list):
            raise FuzzFailure(f"{target_name} seeds must be a list")
        parsed = [
            validate_seed_entry(target_name, value) for value in entries
        ]
        names = [name for name, _payload in parsed]
        if len(names) != len(set(names)):
            raise FuzzFailure(f"{target_name} seed names must be unique")
        if target_name == "grapheme":
            for name, payload in parsed:
                try:
                    payload.decode("utf-8")
                except UnicodeDecodeError as error:
                    raise FuzzFailure(f"grapheme/{name} must be valid UTF-8") from error
        result[target_name] = parsed
    return result


def scalar_entropy(code_points: Sequence[int]) -> bytes:
    result = bytearray()
    for code_point in code_points:
        if not (0 <= code_point <= 0x10FFFF) or 0xD800 <= code_point <= 0xDFFF:
            raise FuzzFailure(f"U+{code_point:04X} is not a Unicode scalar")
        rank = code_point if code_point <= 0xD7FF else code_point - 0x800
        result.extend(rank.to_bytes(3, "little"))
    return bytes(result)


def target_seed_payloads(target: Target) -> list[tuple[str, bytes]]:
    manifest = load_seeds()
    if target.name == "utf8":
        return manifest["utf8"]

    seeds = [
        (name, scalar_entropy([ord(character) for character in payload.decode("utf-8")]))
        for name, payload in manifest["grapheme"]
    ]
    unicode_manifest = unicode_data.load_manifest()
    for case in unicode_data.parse_grapheme_tests(unicode_manifest):
        seeds.append((f"unicode17-{case.line}", scalar_entropy(case.code_points)))
    return seeds


def populate_corpus(target: Target, destination: Path, *, clean: bool) -> int:
    if clean and destination.exists():
        shutil.rmtree(destination)
    destination.mkdir(parents=True, exist_ok=True)
    unique: dict[str, bytes] = {}
    for _name, payload in target_seed_payloads(target):
        digest = hashlib.sha256(payload).hexdigest()
        unique.setdefault(digest, payload)
    for digest, payload in unique.items():
        path = destination / f"seed-{digest[:20]}"
        if not path.exists():
            path.write_bytes(payload)
    print(f"Prepared {len(unique)} unique {target.name} seeds in {destination}")
    return len(unique)


def build_target(roc: str, target: Target) -> Path:
    verify_host()
    verify_pinned_roc(roc)
    BIN_ROOT.mkdir(parents=True, exist_ok=True)
    command([roc, "fmt", "--check", FUZZ_ROOT / "FuzzSupport.roc", target.source])
    command([roc, "check", target.source, "--no-cache"])
    command(
        [
            roc,
            "build",
            "--fuzz",
            target.source,
            f"--output={target.binary}",
            "--no-cache",
        ]
    )
    return target.binary


def smoke(roc: str, targets: Sequence[Target], runs: int) -> None:
    if runs < 1:
        raise FuzzFailure("smoke run count must be positive")
    WORK_ROOT.mkdir(parents=True, exist_ok=True)
    for target in targets:
        binary = build_target(roc, target)
        with tempfile.TemporaryDirectory(
            prefix=f"smoke-{target.name}-", dir=WORK_ROOT
        ) as temporary:
            corpus = Path(temporary) / "corpus"
            populate_corpus(target, corpus, clean=True)
            command(
                [
                    binary,
                    "run",
                    corpus,
                    f"--runs={runs}",
                    "--seed=50",
                    f"--max-input-size={target.max_input_size}",
                    "--timeout=5",
                    "--memory-limit=2048",
                    "--print-final-stats",
                ]
            )


def strip_separator(values: list[str]) -> list[str]:
    return values[1:] if values[:1] == ["--"] else values


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    subparsers = parser.add_subparsers(dest="operation", required=True)

    build_parser = subparsers.add_parser("build")
    build_parser.add_argument("target", choices=("all", *TARGETS), default="all", nargs="?")

    smoke_parser = subparsers.add_parser("smoke")
    smoke_parser.add_argument("target", choices=("all", *TARGETS), default="all", nargs="?")
    smoke_parser.add_argument("--runs", type=int, default=2000)

    campaign_parser = subparsers.add_parser("campaign")
    campaign_parser.add_argument("target", choices=tuple(TARGETS))
    campaign_parser.add_argument("runner_args", nargs=argparse.REMAINDER)

    reproduce_parser = subparsers.add_parser("reproduce")
    reproduce_parser.add_argument("target", choices=tuple(TARGETS))
    reproduce_parser.add_argument("artifact", type=Path)

    minimize_parser = subparsers.add_parser("minimize")
    minimize_parser.add_argument("target", choices=tuple(TARGETS))
    minimize_parser.add_argument("input", type=Path)
    minimize_parser.add_argument("output", type=Path)

    args = parser.parse_args(argv)
    try:
        if args.operation == "build":
            for target in selected_targets(args.target):
                build_target(args.roc, target)
        elif args.operation == "smoke":
            smoke(args.roc, selected_targets(args.target), args.runs)
        elif args.operation == "campaign":
            target = TARGETS[args.target]
            binary = build_target(args.roc, target)
            corpus = PERSISTENT_CORPUS_ROOT / target.name
            populate_corpus(target, corpus, clean=False)
            command([binary, "run", corpus, *strip_separator(args.runner_args)])
        elif args.operation == "reproduce":
            target = TARGETS[args.target]
            artifact = args.artifact.resolve()
            payload = artifact.read_bytes()
            print(f"raw-bytes={len(payload)} hex={payload.hex().upper()}")
            binary = build_target(args.roc, target)
            command([binary, "show", artifact])
            command([binary, "replay", artifact])
        elif args.operation == "minimize":
            target = TARGETS[args.target]
            binary = build_target(args.roc, target)
            command([binary, "minimize", args.input.resolve(), args.output.resolve()])
    except (FuzzFailure, OSError, subprocess.TimeoutExpired) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
