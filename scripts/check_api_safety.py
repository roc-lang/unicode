#!/usr/bin/env python3
"""Check public API audit coverage and review of production panic sites."""

import re
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PACKAGE = ROOT / "package"
INVENTORY = ROOT / "docs" / "api-safety.md"

MODULE = re.compile(r"^([A-Z][A-Za-z0-9_]*)\s+::\s")
ASSOCIATED = re.compile(r"^([A-Z][A-Za-z0-9_]*)\s+(?::=|::)\s")
BINDING = re.compile(r"^([a-z][a-z0-9_]*)\s*=(?![=>])")
DERIVED = re.compile(r"^([a-z][a-z0-9_]*)\s*:\s*_\s*$")
REVIEWED_SYNTAX = re.compile(
    r"\bcrash\b|(?<!\.)\.\.\.(?!\.)"
    r"|\b(?:plus_wrap|minus_wrap|times_wrap|with_capacity|reserve)\b|\b[A-Za-z0-9_]*unchecked[A-Za-z0-9_]*\b"
)


def without_non_code(text):
    """Blank comments and literals while preserving executable interpolation."""
    code = list(text)
    size = len(text)

    def blank(index):
        if code[index] != "\n":
            code[index] = " "

    def literal(start, delimiter):
        index = start
        for index in range(start, start + len(delimiter)):
            blank(index)
        index += 1
        while index < size:
            if text.startswith(delimiter, index):
                for tail in range(index, index + len(delimiter)):
                    blank(tail)
                return index + len(delimiter)
            if text[index] == "\\":
                blank(index)
                index += 1
                if index < size:
                    blank(index)
                    index += 1
            elif delimiter.startswith('"') and text.startswith("${", index):
                blank(index)
                blank(index + 1)
                index = expression(index + 2, interpolation=True)
            else:
                blank(index)
                index += 1
        return index

    def expression(start, *, interpolation):
        index = start
        depth = 0
        while index < size:
            char = text[index]
            if char == "#":
                while index < size and text[index] != "\n":
                    blank(index)
                    index += 1
            elif char in ('"', "'"):
                delimiter = char * 3 if text.startswith(char * 3, index) else char
                index = literal(index, delimiter)
            elif interpolation and char == "{":
                depth += 1
                index += 1
            elif interpolation and char == "}":
                if depth == 0:
                    blank(index)
                    return index + 1
                depth -= 1
                index += 1
            else:
                index += 1
        return index

    expression(0, interpolation=False)
    return "".join(code)


def exported_modules(text):
    match = re.search(r"\bpackage\s*\[([^]]*)\]", without_non_code(text))
    if not match:
        raise ValueError("cannot find the public package exposure list")
    return [name.strip() for name in match[1].split(",") if name.strip()]


def operations(module, text):
    # Roc scopes place associated items exactly one indentation level deeper
    # than their declaration. Track the associated record opening even when
    # its underlying type spans multiple lines.
    scope = [(-1, module)]
    pending = {}
    found = set()
    inside = False
    awaiting_scope = False
    scope_endings = ("}.{", "].{", ").{")
    for line in without_non_code(text).expandtabs(4).splitlines():
        stripped = line.lstrip()
        if not stripped:
            continue
        column = len(line) - len(stripped)
        if not inside:
            if awaiting_scope:
                if column > 0:
                    continue
                if stripped not in scope_endings:
                    break
                inside = True
                continue
            match = MODULE.match(stripped)
            if not match or match[1] != module or column != 0:
                continue
            inside = stripped.endswith(scope_endings)
            awaiting_scope = not inside
            continue
        if column == 0:
            break

        while len(scope) > 1 and column <= scope[-1][0]:
            scope.pop()
        parent_column = scope[-1][0]
        # All hand-authored public modules use four columns (or a tab).
        if column != max(parent_column, 0) + 4:
            continue

        associated = ASSOCIATED.match(stripped)
        if associated:
            pending[column] = associated[1]
        match = BINDING.match(stripped) or DERIVED.match(stripped)
        if match:
            found.add(".".join([name for _, name in scope] + [match[1]]))
        if stripped.endswith(scope_endings):
            if column not in pending:
                raise ValueError(f"unrecognized associated type opener in {module}: {stripped}")
            scope.append((column, pending.pop(column)))
    if not inside or not found:
        raise ValueError(f"cannot find public operations in {module}")
    return found


def inventory(text):
    covered = Counter()
    allowed = Counter()
    for line in text.splitlines():
        if line.startswith("| Invariant |"):
            match = re.fullmatch(r"\| Invariant \| ([^/]+)/`([^`]+)` \| ([^|]+) \|", line)
            if not match or not match[3].strip():
                raise ValueError(f"incomplete invariant review: {line}")
            allowed[(match[1], match[2].replace(r"\|", "|"))] += 1
            continue
        columns = [part.strip() for part in line.split("|")[1:-1]]
        if not columns:
            continue
        module, members, *contract = columns
        if members.startswith("`"):
            if len(contract) != 3 or any(not item for item in contract):
                raise ValueError(f"incomplete audit row for {module}: {members}")
            covered.update(f"{module}.{name}" for name in re.findall(r"`([a-zA-Z0-9_.]+)`", members))
    return covered, allowed


def panic_sites(package):
    sites = Counter()
    for path in sorted(package.glob("*.roc")):
        # Public packages have no test scripts in this directory. This also
        # scans generated views and helpers outside the public module list.
        text = path.read_text(encoding="utf-8")
        for code, original in zip(without_non_code(text).splitlines(), text.splitlines(), strict=True):
            if REVIEWED_SYNTAX.search(code):
                sites[(path.name, original.strip())] += 1
    return sites


def main():
    public = set()
    try:
        for module in exported_modules((PACKAGE / "main.roc").read_text(encoding="utf-8")):
            public.update(operations(module, (PACKAGE / f"{module}.roc").read_text(encoding="utf-8")))
        documented, allowed = inventory(INVENTORY.read_text(encoding="utf-8"))
        sites = panic_sites(PACKAGE)
    except (OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    errors = [
        *(f"unaudited public operation {name}" for name in sorted(public - documented.keys())),
        *(f"stale public operation {name}" for name in sorted(documented.keys() - public)),
        *(f"duplicate audit entry {name}" for name, count in documented.items() if count != 1),
        *(f"unreviewed invariant site {file}: {source}" for file, source in sorted(sites - allowed)),
        *(f"stale invariant site {file}: {source}" for file, source in sorted(allowed - sites)),
    ]
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print(f"Checked safety inventory for {len(public)} public operations and {sum(sites.values())} invariant sites")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
