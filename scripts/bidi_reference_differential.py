#!/usr/bin/env python3
"""Differentially run deterministic scalar cases through Unicode Code9 and Roc."""

from __future__ import annotations

import argparse
import hashlib
import json
import random
import shutil
import subprocess
import tempfile
import urllib.request
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CODE9_ROOT = "https://www.unicode.org/Public/PROGRAMS/BidiReferenceC/17.0.0/"
CODE9_FILES = {
    "include/bidiref.h": "92af5400d2bcd6e5c905bd0d495327922d808a89411290b1464f4bb57eb69f1e",
    "source/bidiref.c": "4dc992dcbf4a9219319e8c567794d9c0956f65f4023fed8a3179af6cb8ea2666",
    "source/bidiref1.c": "9fb4f07be60e13006f4835f5934025d96db5ea2967a975db8b3b4ce198ccb8ae",
    "source/bidirefp.h": "74427a3d798b8caa62a04b40a48be990bcdba1c1bd0a25db0f97be86cd18a58c",
    "source/brinput.c": "ec627e197b663432f7a8e3042d6d358a2630519ef6a264ee77febe90b2b0afc0",
    "source/brrule.c": "e5b54978f2f7c2f21070810ef65d958848ae0ab002bec3f40f93da9a300f7a38",
    "source/brtable.c": "bc42a4355fba47eb0426bc9755a090e30deda1371cf3a026e789f404934b96eb",
    "source/brtest.c": "359d42e4ce17352547e77d5a354ff1d906aeaf1585940af920b0dffd188a433c",
    "source/brutils.c": "3ae40615e4e3adaa4445be272c98133bbe62489084721bb9c6a103e0ce1a8b04",
}
SCALARS = (
    0x61, 0x05D0, 0x0627, 0x30, 0x0660, 0x2B, 0x24, 0x2C, 0x0300,
    0x200C, 0x200D, 0x20, 0x21, 0x202A, 0x202B, 0x202C, 0x202D, 0x202E,
    0x2066, 0x2067, 0x2068, 0x2069, 0x200B, 0x28, 0x29, 0x3008, 0x3009,
    0x2201, 0xE000, 0xFDD0, 0x1E8C5, 0x1F600,
)
WRAPPER = r'''#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bidiref.h"
int main(int argc, char **argv) {
  char input[8192], levels[8192], order[8192], *field, *codes, *next;
  U_Int_32 text[BR_MAXINPUTLEN]; int mode, count, level, rc;
  TraceOff(TraceAll);
  if (argc != 2 || br_InitWithPath(UBA170, argv[1]) != BR_TESTOK) return 2;
  while (fgets(input, sizeof(input), stdin)) {
    field = strtok(input, "\t\n"); codes = strtok(NULL, "\t\n");
    if (!field || !codes) return 3; mode = atoi(field); count = 0; next = codes;
    while (*next) { char *end; if (count == BR_MAXINPUTLEN) return 4;
      text[count++] = strtoul(next, &end, 16); if (end == next) return 5;
      next = *end == ',' ? end + 1 : end; if (*end && *end != ',') return 6; }
    rc = br_QueryOneTestCase(text, count, mode, &level, levels, sizeof(levels), order, sizeof(order));
    if (rc != BR_TESTOK) return 7;
    printf("%d\t%s\t%s\n", level, levels, order);
  } return 0;
}'''


def verify_pinned_roc(roc: str) -> None:
    pin = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    output = subprocess.run([roc, "version"], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=True).stdout
    if pin not in output and pin.rsplit("-", 1)[-1] not in output:
        raise RuntimeError(f"Roc compiler differs from .roc-version ({pin}): {output.strip()}")


def download_verified(relative: str, digest: str, destination: Path) -> None:
    data = urllib.request.urlopen(CODE9_ROOT + relative, timeout=60).read()
    actual = hashlib.sha256(data).hexdigest()
    if actual != digest:
        raise RuntimeError(f"Code9 checksum mismatch for {relative}: expected {digest}, got {actual}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(data)


def build_code9(directory: Path) -> Path:
    for relative, digest in CODE9_FILES.items():
        download_verified(relative, digest, directory / relative)
    data = directory / "data"
    data.mkdir()
    manifest = json.loads((ROOT / "vendor" / "unicode" / "manifest.json").read_text(encoding="utf-8"))
    if manifest["releases"]["unicode"]["version"] != "17.0.0" or manifest["specifications"]["uax_9"]["revision"] != "51":
        raise RuntimeError("Code9 source pin requires Unicode 17.0.0 / UAX #9 revision 51")
    shutil.copyfile(ROOT / "vendor/unicode/17.0.0/UnicodeData.txt", data / "UnicodeData-17.0.0.txt")
    shutil.copyfile(ROOT / "vendor/unicode/17.0.0/BidiBrackets.txt", data / "BidiBrackets-17.0.0.txt")
    wrapper = directory / "wrapper.c"
    wrapper.write_text(WRAPPER, encoding="utf-8")
    binary = directory / "code9-query"
    # The two distributed command-line/demo translation units each define
    # `main`; the query wrapper replaces them while retaining the shared
    # table, rule, test, input, and utility implementation units.
    sources = [
        directory / name
        for name in CODE9_FILES
        if name.endswith(".c") and name not in {"source/bidiref.c", "source/bidiref1.c", "source/brinput.c"}
    ]
    subprocess.run(["cc", "-std=c99", "-O2", "-I", str(directory / "include"), "-I", str(directory / "source"), *map(str, sources), str(wrapper), "-o", str(binary)], check=True)
    return binary


def cases(seed: int, count: int) -> list[tuple[str, str, int]]:
    generator = random.Random(seed)
    result = []
    for index in range(count):
        codepoints = ",".join(f"{generator.choice(SCALARS):04X}" for _ in range(generator.randrange(1, 97)))
        result.append((f"code9:seed-{seed}:case-{index}", codepoints, generator.randrange(3)))
    return result


def code9_results(binary: Path, data_directory: Path, rows: list[tuple[str, str, int]]) -> list[tuple[int, str, str]]:
    payload = "".join(f"{mode}\t{codepoints}\n" for _, codepoints, mode in rows)
    completed = subprocess.run([str(binary), f"{data_directory}/"], input=payload, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if completed.returncode != 0:
        raise RuntimeError(f"Code9 query failed ({completed.returncode}): {completed.stderr.strip()}")
    output = []
    for line in completed.stdout.splitlines():
        # Code9's initialization may emit informational text despite all trace
        # flags being disabled; only the wrapper's exact three-field records
        # participate in the comparison.
        if line.count("\t") != 2:
            continue
        level, levels, order = line.split("\t")
        output.append((int(level), levels.replace(" ", ","), order.replace(" ", ",") or "-"))
    if len(output) != len(rows):
        raise RuntimeError(f"Code9 response count drift: expected {len(rows)}, got {len(output)}")
    return output


def run_candidate(candidate: Path, rows: list[tuple[str, str, int]], expected: list[tuple[int, str, str]]) -> None:
    # Code9's direction encoding is already BidiCharacterTest's LTR/RTL/Auto order.
    protocol_rows = [f"{case_id}\t{codepoints}\t{mode}\t{level}\t{levels}\t{order}" for (case_id, codepoints, mode), (level, levels, order) in zip(rows, expected, strict=True)]
    payload = "ROC_UNICODE_TEST_V1\tbidi-character-test\t{}\n{}\n".format(len(protocol_rows), "\n".join(protocol_rows))
    completed = subprocess.run([str(candidate)], cwd=ROOT, input=payload, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    expected_output = f"PASS\tbidi-character-test\t{len(protocol_rows)}"
    if completed.returncode != 0 or completed.stdout.strip() != expected_output:
        # Keep the scheduled failure actionable: rerun one row at a time and
        # preserve the seed-derived scalar sequence, base mode, Code9 result,
        # and Roc app's exact expected/actual diagnostic.
        for row, oracle in zip(rows, expected, strict=True):
            case_id, codepoints, mode = row
            one = f"{case_id}\t{codepoints}\t{mode}\t{oracle[0]}\t{oracle[1]}\t{oracle[2]}"
            one_payload = f"ROC_UNICODE_TEST_V1\tbidi-character-test\t1\n{one}\n"
            isolated = subprocess.run([str(candidate)], cwd=ROOT, input=one_payload, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            if isolated.returncode != 0 or isolated.stdout.strip() != "PASS\tbidi-character-test\t1":
                raise RuntimeError(
                    "Roc/Code9 differential mismatch: "
                    f"case={case_id}, mode={mode}, scalars={codepoints}, code9={oracle}, "
                    f"roc_stdout={isolated.stdout.strip()!r}, roc_stderr={isolated.stderr.strip()!r}"
                )
        raise RuntimeError(f"Roc/Code9 differential batch protocol failure: stdout={completed.stdout.strip()!r}, stderr={completed.stderr.strip()!r}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", default="roc")
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--seed", type=int, default=1511506142)
    parser.add_argument("--cases", type=int, default=256)
    args = parser.parse_args()
    if args.cases < 1:
        parser.error("--cases must be positive")
    try:
        verify_pinned_roc(args.roc)
        if not args.candidate.is_file():
            raise RuntimeError(f"candidate binary is missing: {args.candidate}")
        with tempfile.TemporaryDirectory(prefix="unicode-code9-reference-") as temporary:
            directory = Path(temporary)
            reference = build_code9(directory)
            generated = cases(args.seed, args.cases)
            run_candidate(args.candidate, generated, code9_results(reference, directory / "data", generated))
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=__import__("sys").stderr)
        return 1
    print(f"PASS Code9 Unicode 17.0.0 UAX #9 r51 differential: {args.cases} seeded cases")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
