#!/usr/bin/env python3
"""Generate MSVC import archives from the selected Zig's mingw-w64 definitions."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import shutil
import subprocess

ROOT = Path(__file__).resolve().parent
# std.Io.Threaded, std.heap and the default diagnostic handlers use these DLLs.
LIBRARIES = ("ntdll", "kernel32", "advapi32", "bcrypt", "ws2_32", "dbghelp")


def build(zig: str, target: str) -> None:
    environment = subprocess.check_output([zig, "env"], text=True)
    match = re.search(r'\.lib_dir\s*=\s*("(?:[^"\\]|\\.)*")', environment)
    if match is None:
        raise RuntimeError("zig env did not report lib_dir")
    mingw = Path(json.loads(match[1])) / "libc" / "mingw"
    output = ROOT / "targets" / target
    parts = output / "windows-imports"
    parts.mkdir(parents=True, exist_ok=True)
    architecture, machine, definitions = {
        "x64win": ("x86_64", "i386:x86-64", "lib64"),
        "arm64win": ("aarch64", "arm64", "libarm64"),
    }[target]
    for name in LIBRARIES:
        candidates = [mingw / directory / (name + suffix)
                      for directory in (definitions, "lib-common")
                      for suffix in (".def", ".def.in")]
        source = next(path for path in candidates if path.is_file())
        definition = parts / (name + ".def")
        subprocess.run([
            zig, "cc", "-E", "-P", "-x", "c", "-target", architecture + "-windows-msvc",
            "-Wno-extra-tokens", "-Wno-invalid-pp-token",
            "-I", str(mingw / "def-include"), "-I", str(mingw / "lib-common"),
            str(source), "-o", str(definition),
        ], check=True)
        # Win64 uses undecorated names, including definitions shared with x86.
        definition.write_text(re.sub(r"(?<=[A-Za-z0-9_])@\d+\b", "", definition.read_text()))
        subprocess.run([
            zig, "dlltool", "-m", machine, "-d", str(definition),
            "-l", str(parts / (name + ".lib")),
        ], check=True)
    # Preserve duplicate member names in COFF import libraries while flattening.
    archive = output / "windows_imports.lib"
    script = f'CREATE "{archive.as_posix()}"\n' + "".join(
        f'ADDLIB "{(parts / (name + ".lib")).as_posix()}"\n' for name in LIBRARIES
    ) + "SAVE\nEND\n"
    subprocess.run([zig, "ar", "--format=coff", "-M"], input=script, text=True, check=True)
    shutil.copyfile(mingw / "COPYING", parts / "LICENSE-mingw-w64")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--zig", required=True)
    parser.add_argument("--target", choices=("x64win", "arm64win"), required=True)
    arguments = parser.parse_args()
    build(arguments.zig, arguments.target)
