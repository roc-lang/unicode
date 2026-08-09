"""Deterministic scalar-sequence delta debugging and CI regression capture."""

from __future__ import annotations

from collections.abc import Callable, Sequence
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FAILURE_ROOT = ROOT / ".roc-unicode-tmp" / "failures"


def minimize(values: Sequence[str], reproduces: Callable[[list[str]], bool]) -> list[str]:
    """ddmin deletion reduction; the caller defines the preserved failure."""
    current = list(values)
    granularity = 2
    while len(current) >= 2:
        chunk = max(1, (len(current) + granularity - 1) // granularity)
        for start in range(0, len(current), chunk):
            candidate = current[:start] + current[start + chunk :]
            if candidate and reproduces(candidate):
                current = candidate
                granularity = max(2, granularity - 1)
                break
        else:
            if granularity >= len(current):
                break
            granularity = min(len(current), granularity * 2)
    return current


def capture(name: str, contents: str) -> Path:
    FAILURE_ROOT.mkdir(parents=True, exist_ok=True)
    path = FAILURE_ROOT / name
    path.write_text(contents, encoding="utf-8")
    return path
