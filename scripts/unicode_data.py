#!/usr/bin/env python3
"""Validate pinned Unicode data and generate Roc lookup modules.

This deliberately uses only the Python standard library so normal CI can run
offline after checkout. The manifest is the source of truth for provenance,
versions, hashes, and expected record counts.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


ROOT = Path(__file__).resolve().parents[1]
UNICODE_VENDOR_ROOT = ROOT / "vendor" / "unicode"
MANIFEST_PATH = UNICODE_VENDOR_ROOT / "manifest.json"
MAX_CODE_POINT = 0x10FFFF
PAGE_BITS = 8
PAGE_SIZE = 1 << PAGE_BITS
RANGE_RE = re.compile(
    r"^(?P<start>[0-9A-F]{4,6})(?:\.\.(?P<end>[0-9A-F]{4,6}))?"
    r"\s*;\s*(?P<property>[A-Za-z_]+)(?:\s*#.*)?$"
)
INCB_RE = re.compile(
    r"^(?P<start>[0-9A-F]{4,6})(?:\.\.(?P<end>[0-9A-F]{4,6}))?"
    r"\s*;\s*InCB\s*;\s*(?P<property>[A-Za-z_]+)(?:\s*#.*)?$"
)
HEX_RE = re.compile(r"^[0-9A-F]{4,6}$")

GCB_PROPERTIES = (
    "CR",
    "LF",
    "Control",
    "Extend",
    "ZWJ",
    "Regional_Indicator",
    "Prepend",
    "SpacingMark",
    "L",
    "V",
    "T",
    "LV",
    "LVT",
)
EAW_PROPERTIES = ("A", "F", "H", "N", "Na", "W")
EMOJI_PROPERTIES = (
    "Emoji",
    "Emoji_Presentation",
    "Emoji_Modifier",
    "Emoji_Modifier_Base",
    "Emoji_Component",
    "Extended_Pictographic",
)
INCB_PROPERTIES = ("Consonant", "Extend", "Linker")


class DataError(ValueError):
    """Pinned data is malformed or has drifted from its manifest."""


@dataclass(frozen=True, order=True)
class RangeRecord:
    start: int
    end: int
    property: str
    line: int


@dataclass(frozen=True)
class GraphemeCase:
    case_id: str
    line: int
    code_points: tuple[int, ...]
    break_offsets: tuple[int, ...]
    rules: frozenset[str]

def _require_dict(value: object, context: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise DataError(f"{context} must be a JSON object")
    return value


def load_manifest(path: Path = MANIFEST_PATH) -> dict[str, object]:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as err:
        raise DataError(f"unable to read {path}: {err}") from err
    manifest = _require_dict(raw, "manifest")
    if manifest.get("schema_version") != 1:
        raise DataError("manifest schema_version must be 1")
    for key in ("unicode_version", "emoji_version", "license", "standards", "files"):
        if not isinstance(manifest.get(key), str if key not in ("standards", "files") else dict):
            raise DataError(f"manifest field {key!r} has the wrong type")
    standards = _require_dict(manifest["standards"], "manifest.standards")
    expected_standards = {
        "uax_11_revision",
        "uax_29_revision",
        "uax_44_revision",
        "uts_51_revision",
    }
    if set(standards) != expected_standards or not all(
        isinstance(value, str) for value in standards.values()
    ):
        raise DataError("manifest.standards has the wrong fields or value types")
    expected_files = {
        "derived_core_properties",
        "east_asian_width",
        "grapheme_break_property",
        "grapheme_break_test",
        "emoji_data",
    }
    files = _require_dict(manifest["files"], "manifest.files")
    if set(files) != expected_files:
        raise DataError(
            f"manifest.files must be exactly {sorted(expected_files)}, got {sorted(files)}"
        )
    unicode_version = str(manifest["unicode_version"])
    emoji_version = str(manifest["emoji_version"])
    for name, raw_item in files.items():
        item = _require_dict(raw_item, f"manifest.files.{name}")
        path_value = item.get("path")
        header_value = item.get("header")
        if not isinstance(path_value, str) or Path(path_value).parts[:1] != (unicode_version,):
            raise DataError(f"manifest.files.{name}.path does not match unicode_version")
        expected_header_version = emoji_version if name == "emoji_data" else unicode_version
        if not isinstance(header_value, str) or expected_header_version not in header_value:
            raise DataError(f"manifest.files.{name}.header does not match its pinned version")
    return manifest


def _entry(manifest: dict[str, object], name: str) -> dict[str, object]:
    files = _require_dict(manifest["files"], "manifest.files")
    item = _require_dict(files[name], f"manifest.files.{name}")
    for field, expected_type in (
        ("path", str),
        ("url", str),
        ("sha256", str),
        ("header", str),
        ("records", int),
        ("role", str),
    ):
        if not isinstance(item.get(field), expected_type):
            raise DataError(f"manifest.files.{name}.{field} has the wrong type")
    if not str(item["url"]).startswith("https://www.unicode.org/"):
        raise DataError(f"manifest.files.{name}.url is not an official Unicode URL")
    if not re.fullmatch(r"[0-9a-f]{64}", str(item["sha256"])):
        raise DataError(f"manifest.files.{name}.sha256 is not a SHA-256 digest")
    return item


def data_path(manifest: dict[str, object], name: str) -> Path:
    relative = Path(str(_entry(manifest, name)["path"]))
    if relative.is_absolute() or ".." in relative.parts:
        raise DataError(f"manifest path for {name} must stay below vendor/unicode/")
    return UNICODE_VENDOR_ROOT / relative


def verify_source(manifest: dict[str, object], name: str) -> str:
    item = _entry(manifest, name)
    path = data_path(manifest, name)
    try:
        content = path.read_bytes()
    except OSError as err:
        raise DataError(f"unable to read {path}: {err}") from err
    digest = hashlib.sha256(content).hexdigest()
    if digest != item["sha256"]:
        raise DataError(
            f"SHA-256 mismatch for {path}: expected {item['sha256']}, got {digest}"
        )
    try:
        text = content.decode("utf-8")
    except UnicodeDecodeError as err:
        raise DataError(f"{path} is not UTF-8: {err}") from err
    if str(item["header"]) not in text:
        raise DataError(f"missing header marker {item['header']!r} in {path}")
    records = sum(
        1 for line in text.splitlines() if line.strip() and not line.lstrip().startswith("#")
    )
    if records != item["records"]:
        raise DataError(
            f"record-count drift for {path}: expected {item['records']}, got {records}"
        )
    return text


def parse_ranges(
    text: str,
    *,
    source: str,
    allowed_properties: Iterable[str],
    default_marker: str,
    overlaps_by_property: bool = False,
) -> list[RangeRecord]:
    if default_marker not in text:
        raise DataError(f"{source}: missing required default marker {default_marker!r}")
    allowed = frozenset(allowed_properties)
    records: list[RangeRecord] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        match = RANGE_RE.fullmatch(line)
        if match is None:
            raise DataError(f"{source}:{line_number}: malformed data line: {raw_line!r}")
        prop = match.group("property")
        if prop not in allowed:
            raise DataError(f"{source}:{line_number}: unknown property {prop!r}")
        start = int(match.group("start"), 16)
        end = int(match.group("end") or match.group("start"), 16)
        if start > MAX_CODE_POINT or end > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: code point exceeds U+10FFFF")
        if start > end:
            raise DataError(f"{source}:{line_number}: reversed range")
        records.append(RangeRecord(start, end, prop, line_number))
    if not records:
        raise DataError(f"{source}: no data records")

    groups: dict[str, list[RangeRecord]]
    if overlaps_by_property:
        groups = {prop: [] for prop in allowed}
        for record in records:
            groups[record.property].append(record)
    else:
        groups = {"all properties": records}
    for group, ranges in groups.items():
        ordered = sorted(ranges, key=lambda item: (item.start, item.end))
        for previous, current in zip(ordered, ordered[1:]):
            if current.start <= previous.end:
                raise DataError(
                    f"{source}:{current.line}: range overlaps line {previous.line} in {group}"
                )
    return records


def parse_incb(text: str, *, source: str) -> list[RangeRecord]:
    default_marker = "# @missing: 0000..10FFFF; InCB; None"
    if default_marker not in text:
        raise DataError(f"{source}: missing required default marker {default_marker!r}")

    records: list[RangeRecord] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        if "; InCB" not in line:
            continue
        match = INCB_RE.fullmatch(line)
        if match is None:
            raise DataError(f"{source}:{line_number}: malformed InCB line: {raw_line!r}")
        prop = match.group("property")
        if prop not in INCB_PROPERTIES:
            raise DataError(f"{source}:{line_number}: unknown InCB value {prop!r}")
        start = int(match.group("start"), 16)
        end = int(match.group("end") or match.group("start"), 16)
        if start > end or end > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: invalid InCB range")
        records.append(RangeRecord(start, end, prop, line_number))

    if not records:
        raise DataError(f"{source}: no InCB records")
    ordered = sorted(records, key=lambda item: (item.start, item.end))
    for previous, current in zip(ordered, ordered[1:]):
        if current.start <= previous.end:
            raise DataError(
                f"{source}:{current.line}: InCB range overlaps line {previous.line}"
            )
    return records


def load_property_data(
    manifest: dict[str, object],
) -> tuple[
    list[RangeRecord],
    list[RangeRecord],
    list[RangeRecord],
    list[RangeRecord],
]:
    gcb = parse_ranges(
        verify_source(manifest, "grapheme_break_property"),
        source=str(data_path(manifest, "grapheme_break_property")),
        allowed_properties=GCB_PROPERTIES,
        default_marker="# @missing: 0000..10FFFF; Other",
    )
    eaw = parse_ranges(
        verify_source(manifest, "east_asian_width"),
        source=str(data_path(manifest, "east_asian_width")),
        allowed_properties=EAW_PROPERTIES,
        default_marker="explicitly are given the value \"N\"",
    )
    emoji = parse_ranges(
        verify_source(manifest, "emoji_data"),
        source=str(data_path(manifest, "emoji_data")),
        allowed_properties=EMOJI_PROPERTIES,
        default_marker="# All omitted code points have Emoji=No",
        overlaps_by_property=True,
    )
    incb = parse_incb(
        verify_source(manifest, "derived_core_properties"),
        source=str(data_path(manifest, "derived_core_properties")),
    )
    return gcb, eaw, emoji, incb


def parse_grapheme_tests(
    manifest: dict[str, object], text: str | None = None
) -> list[GraphemeCase]:
    version = str(manifest["unicode_version"])
    source_name = "GraphemeBreakTest.txt"
    if text is None:
        text = verify_source(manifest, "grapheme_break_test")
    cases: list[GraphemeCase] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        stripped = raw_line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        fields = raw_line.split("#", 1)
        if len(fields) != 2:
            raise DataError(f"{source_name}:{line_number}: test is missing its rule comment")
        body, comment = fields
        tokens = body.split()
        if len(tokens) < 3 or len(tokens) % 2 == 0:
            raise DataError(f"{source_name}:{line_number}: malformed test token sequence")
        if tokens[0] != "÷" or tokens[-1] != "÷":
            raise DataError(f"{source_name}:{line_number}: tests must break at both ends")
        code_points: list[int] = []
        break_offsets: list[int] = []
        utf8_offset = 0
        for index, token in enumerate(tokens):
            if index % 2 == 0:
                if token not in ("÷", "×"):
                    raise DataError(
                        f"{source_name}:{line_number}: expected boundary marker, got {token!r}"
                    )
                if token == "÷":
                    break_offsets.append(utf8_offset)
            else:
                if HEX_RE.fullmatch(token) is None:
                    raise DataError(
                        f"{source_name}:{line_number}: invalid code point {token!r}"
                    )
                code_point = int(token, 16)
                if code_point > MAX_CODE_POINT or 0xD800 <= code_point <= 0xDFFF:
                    raise DataError(
                        f"{source_name}:{line_number}: invalid scalar U+{code_point:04X}"
                    )
                code_points.append(code_point)
                utf8_offset += len(chr(code_point).encode("utf-8"))
        rule_tokens = re.findall(r"\[([0-9]+(?:\.[0-9]+)?)\]", comment)
        rules = frozenset(rule_tokens)
        boundary_count = (len(tokens) + 1) // 2
        if len(rule_tokens) != boundary_count:
            raise DataError(
                f"{source_name}:{line_number}: expected {boundary_count} boundary rules, "
                f"got {len(rule_tokens)}"
            )
        cases.append(
            GraphemeCase(
                case_id=f"{version}:{source_name}:{line_number}",
                line=line_number,
                code_points=tuple(code_points),
                break_offsets=tuple(break_offsets),
                rules=rules,
            )
        )
    expected = _entry(manifest, "grapheme_break_test").get("cases")
    if expected != len(cases):
        raise DataError(f"grapheme case-count drift: expected {expected}, got {len(cases)}")
    return cases


def _ranges_for(records: list[RangeRecord], prop: str) -> list[RangeRecord]:
    return [record for record in records if record.property == prop]


def _merge_adjacent(records: list[RangeRecord]) -> list[RangeRecord]:
    merged: list[RangeRecord] = []
    for current in sorted(records, key=lambda item: (item.start, item.end)):
        if merged and merged[-1].property == current.property and merged[-1].end + 1 == current.start:
            previous = merged[-1]
            merged[-1] = RangeRecord(previous.start, current.end, previous.property, previous.line)
        else:
            merged.append(current)
    return merged


def _condition(records: list[RangeRecord], *, hexadecimal: bool = False) -> str:
    def number(value: int) -> str:
        return f"0x{value:04X}" if hexadecimal else str(value)

    parts = []
    for record in records:
        if record.start == record.end:
            parts.append(f"u32 == {number(record.start)}" if not hexadecimal else f"cp == {number(record.start)}")
        else:
            variable = "cp" if hexadecimal else "u32"
            parts.append(
                f"({number(record.start)} <= {variable} and {variable} <= {number(record.end)})"
                if hexadecimal
                else f"({variable} >= {number(record.start)} and {variable} <= {number(record.end)})"
            )
    if not parts:
        raise DataError("cannot generate an empty property condition")
    return " or ".join(parts)


def _roc_list(values: Iterable[int], *, per_line: int = 32) -> str:
    items = [str(value) for value in values]
    lines = [", ".join(items[index : index + per_line]) for index in range(0, len(items), per_line)]
    return "[\n    " + ",\n    ".join(lines) + ",\n]"


def render_grapheme_data(
    version: str,
    gcb_records: list[RangeRecord],
    incb_records: list[RangeRecord],
    emoji_records: list[RangeRecord],
) -> str:
    gcb_values = {
        "Other": 0,
        "CR": 1,
        "LF": 2,
        "Control": 3,
        "Extend": 4,
        "ZWJ": 5,
        "Regional_Indicator": 6,
        "Prepend": 7,
        "SpacingMark": 8,
        "L": 9,
        "V": 10,
        "T": 11,
        "LV": 12,
        "LVT": 13,
    }
    incb_values = {"None": 0, "Consonant": 1, "Extend": 2, "Linker": 3}

    encoded = bytearray(MAX_CODE_POINT + 1)
    for record in gcb_records:
        value = gcb_values[record.property]
        encoded[record.start : record.end + 1] = bytes((value,)) * (record.end - record.start + 1)
    for record in incb_records:
        value = incb_values[record.property] << 4
        for code_point in range(record.start, record.end + 1):
            encoded[code_point] |= value
    for record in _ranges_for(emoji_records, "Extended_Pictographic"):
        for code_point in range(record.start, record.end + 1):
            encoded[code_point] |= 0x40

    page_ids: dict[bytes, int] = {}
    pages: list[bytes] = []
    page_index: list[int] = []
    for start in range(0, len(encoded), PAGE_SIZE):
        page = bytes(encoded[start : start + PAGE_SIZE])
        page_id = page_ids.get(page)
        if page_id is None:
            page_id = len(pages)
            page_ids[page] = page_id
            pages.append(page)
        page_index.append(page_id)
    if len(pages) > 0x10000:
        raise DataError("grapheme page index no longer fits in U16")
    flat_pages = [value for page in pages for value in page]

    return (
        f"## GENERATED from Unicode {version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        f"## {len(page_index)} scalar pages; {len(pages)} distinct {PAGE_SIZE}-entry pages. ##\n\n"
        "InternalGraphemeData :: [].{\n"
        "    GCB : [Other, CR, LF, Control, Extend, ZWJ, RI, Prepend, SpacingMark, L, V, T, LV, LVT]\n"
        "    InCB : [None, Consonant, Extend, Linker]\n"
        "    Props : { gcb : GCB, incb : InCB, extended_pictographic : Bool }\n\n"
        "    lookup : U32 -> Props\n"
        "    lookup = |scalar| {\n"
        "        page_id = page_index.get(scalar.shr_wrap(8).to_u64()) ?? 0\n"
        "        offset = page_id.to_u64() * 256 + scalar.bitwise_and(255).to_u64()\n"
        "        value = pages.get(offset) ?? 0\n\n"
        "        {\n"
        "            gcb: gcb_from_u8(value.bitwise_and(0x0F)),\n"
        "            incb: incb_from_u8(value.shr_wrap(4).bitwise_and(0x03)),\n"
        "            extended_pictographic: value.bitwise_and(0x40) != 0,\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "gcb_from_u8 : U8 -> InternalGraphemeData.GCB\n"
        "gcb_from_u8 = |value| {\n"
        "    match value {\n"
        "        0 => Other\n"
        "        1 => CR\n"
        "        2 => LF\n"
        "        3 => Control\n"
        "        4 => Extend\n"
        "        5 => ZWJ\n"
        "        6 => RI\n"
        "        7 => Prepend\n"
        "        8 => SpacingMark\n"
        "        9 => L\n"
        "        10 => V\n"
        "        11 => T\n"
        "        12 => LV\n"
        "        13 => LVT\n"
        "        _ => ...\n"
        "    }\n"
        "}\n\n"
        "incb_from_u8 : U8 -> InternalGraphemeData.InCB\n"
        "incb_from_u8 = |value| {\n"
        "    match value {\n"
        "        0 => None\n"
        "        1 => Consonant\n"
        "        2 => Extend\n"
        "        3 => Linker\n"
        "        _ => ...\n"
        "    }\n"
        "}\n\n"
        "page_index : List(U16)\n"
        f"page_index = {_roc_list(page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(flat_pages)}\n"
    )


def render_gcb(version: str, records: list[RangeRecord]) -> str:
    order = (
        ("CR", "CR", "is_cr"),
        ("LF", "LF", "is_lf"),
        ("Control", "Control", "is_control"),
        ("Extend", "Extend", "is_extend_help"),
        ("ZWJ", "ZWJ", "is_zwj_help"),
        ("Regional_Indicator", "RI", "is_ri"),
        ("Prepend", "Prepend", "is_prepend"),
        ("SpacingMark", "SpacingMark", "is_spacing_mark"),
        ("L", "L", "is_l"),
        ("V", "V", "is_v"),
        ("T", "T", "is_t"),
        ("LV", "LV", "is_lv"),
        ("LVT", "LVT", "is_lvt"),
    )
    branches = "\n".join(
        f"        {'if' if index == 0 else 'else if'} {function}(u32) {{\n"
        f"            {tag}\n"
        "        }"
        for index, (_, tag, function) in enumerate(order)
    )
    helpers = []
    for prop, _, function in order:
        helpers.append(
            f"{function} : U32 -> Bool\n"
            f"{function} = |u32| {{\n"
            f"    {_condition(_ranges_for(records, prop))}\n"
            "}"
        )
    return (
        f"## GENERATED from vendor/unicode/{version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "import CodePoint\n\n"
        "InternalGBP :: {}.{\n"
        "    GBP : [CR, LF, Control, Extend, ZWJ, RI, Prepend, SpacingMark, L, V, T, LV, LVT, Other]\n\n"
        "    from_cp : CodePoint -> GBP\n"
        "    from_cp = |cp| {\n"
        "        u32 = CodePoint.to_u32(cp)\n\n"
        f"{branches} else {{\n"
        "            Other\n"
        "        }\n"
        "    }\n\n"
        "    is_extend : U32 -> Bool\n"
        "    is_extend = |u32| is_extend_help(u32)\n\n"
        "    is_zwj : U32 -> Bool\n"
        "    is_zwj = |u32| is_zwj_help(u32)\n"
        "}\n\n"
        + "\n\n".join(helpers)
        + "\n"
    )


def render_eaw(version: str, records: list[RangeRecord]) -> str:
    branches = []
    for prop in ("Na", "A", "W", "H", "F"):
        ranges = _merge_adjacent(_ranges_for(records, prop))
        branches.append(f"if {_condition(ranges, hexadecimal=True)} ({prop})")
    expression = " else ".join(branches) + " else N"
    return (
        f"## GENERATED from vendor/unicode/{version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "InternalEAW :: {}.{\n"
        "    east_asian_width : U32 -> U32\n"
        "    east_asian_width = |code_point| {\n"
        "        match east_asian_width_property(code_point) {\n"
        "            H | N | Na => 1\n"
        "            F | W | A => 2\n"
        "        }\n"
        "    }\n\n"
        "    east_asian_width_property : U32 -> [A, F, H, N, Na, W]\n"
        f"    east_asian_width_property = |cp| {expression}\n"
        "}\n"
    )


def render_emoji(version: str, emoji_version: str, records: list[RangeRecord]) -> str:
    order = (
        ("Extended_Pictographic", "Pictographic", "is_pictographic"),
        ("Emoji_Modifier_Base", "Base", "is_base"),
        ("Emoji_Modifier", "Modifier", "is_modifier"),
        ("Emoji_Presentation", "Presentation", "is_presentation"),
        ("Emoji_Component", "Component", "is_component"),
        ("Emoji", "Emoji", "is_emoji"),
    )
    branches = "\n".join(
        f"        {'if' if index == 0 else 'else if'} {function}_help(u32) {{\n"
        f"            Ok({tag})\n"
        "        }"
        for index, (_, tag, function) in enumerate(order)
    )
    wrappers = "\n\n".join(
        f"    {function} : U32 -> Bool\n"
        f"    {function} = |u32| {function}_help(u32)"
        for _, _, function in order
    )
    helpers = "\n\n".join(
        f"{function}_help : U32 -> Bool\n"
        f"{function}_help = |u32| {{\n"
        f"    {_condition(_ranges_for(records, prop))}\n"
        "}"
        for prop, _, function in order
    )
    return (
        f"## GENERATED from vendor/unicode/{version} (Emoji {emoji_version}). Run `python3 scripts/unicode_data.py generate`. ##\n"
        "import CodePoint\n\n"
        "InternalEmoji :: {}.{\n"
        "    EMOJI : [Pictographic, Base, Modifier, Presentation, Component, Emoji]\n\n"
        "    from_cp : CodePoint -> Try(EMOJI, [NonEmojiCodePoint])\n"
        "    from_cp = |cp| {\n"
        "        u32 = cp.to_u32()\n\n"
        f"{branches} else {{\n"
        "            Err(NonEmojiCodePoint)\n"
        "        }\n"
        "    }\n\n"
        f"{wrappers}\n"
        "}\n\n"
        f"{helpers}\n"
    )


def render_unicode_version(version: str) -> str:
    try:
        major, minor, patch = (int(component) for component in version.split("."))
    except (TypeError, ValueError):
        raise DataError(f"Unicode version must have major.minor.patch form, got {version!r}") from None

    return (
        f"## GENERATED from vendor/unicode/manifest.json. Run `python3 scripts/unicode_data.py generate`. ##\n\n"
        "## The Unicode data and algorithm semantics implemented by this package.\n"
        "UnicodeVersion :: { major : U16, minor : U16, patch : U16 }.{\n"
        "    current : UnicodeVersion\n"
        f"    current = {{ major: {major}, minor: {minor}, patch: {patch} }}\n\n"
        "    major : UnicodeVersion -> U16\n"
        "    major = |version_value| version_value.major\n\n"
        "    minor : UnicodeVersion -> U16\n"
        "    minor = |version_value| version_value.minor\n\n"
        "    patch : UnicodeVersion -> U16\n"
        "    patch = |version_value| version_value.patch\n\n"
        "    to_str : UnicodeVersion -> Str\n"
        f'    to_str = |_| "{version}"\n\n'
        "    is_eq : UnicodeVersion, UnicodeVersion -> Bool\n"
        "    is_eq = |left, right| {\n"
        "        left.major == right.major and left.minor == right.minor and left.patch == right.patch\n"
        "    }\n"
        "}\n"
    )


def rendered_modules(manifest: dict[str, object]) -> dict[Path, str]:
    gcb, eaw, emoji, incb = load_property_data(manifest)
    version = str(manifest["unicode_version"])
    emoji_version = str(manifest["emoji_version"])
    first = {
        ROOT / "package" / "UnicodeVersion.roc": render_unicode_version(version),
        ROOT / "package" / "InternalGraphemeData.roc": render_grapheme_data(version, gcb, incb, emoji),
        ROOT / "package" / "InternalGBP.roc": render_gcb(version, gcb),
        ROOT / "package" / "InternalEAW.roc": render_eaw(version, eaw),
        ROOT / "package" / "InternalEmoji.roc": render_emoji(version, emoji_version, emoji),
    }
    second = {
        ROOT / "package" / "UnicodeVersion.roc": render_unicode_version(version),
        ROOT / "package" / "InternalGraphemeData.roc": render_grapheme_data(version, gcb, incb, emoji),
        ROOT / "package" / "InternalGBP.roc": render_gcb(version, gcb),
        ROOT / "package" / "InternalEAW.roc": render_eaw(version, eaw),
        ROOT / "package" / "InternalEmoji.roc": render_emoji(version, emoji_version, emoji),
    }
    if first != second:
        raise DataError("generator output changed across two renders")
    return first


def validate_all(manifest: dict[str, object]) -> None:
    license_path = UNICODE_VENDOR_ROOT / str(manifest["license"])
    if not license_path.is_file():
        raise DataError(f"missing Unicode data license: {license_path}")
    load_property_data(manifest)
    parse_grapheme_tests(manifest)


def generate(manifest: dict[str, object], *, check: bool) -> None:
    outputs = rendered_modules(manifest)
    failures = []
    for path, expected in outputs.items():
        if check:
            actual = path.read_text(encoding="utf-8") if path.exists() else None
            if actual != expected:
                failures.append(str(path.relative_to(ROOT)))
        else:
            path.write_text(expected, encoding="utf-8", newline="\n")
            print(f"generated {path.relative_to(ROOT)}")
    if failures:
        raise DataError(
            "generated files are stale: "
            + ", ".join(failures)
            + "; run `python3 scripts/unicode_data.py generate`"
        )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("validate", help="validate the manifest and pinned data")
    generate_parser = subparsers.add_parser("generate", help="generate Roc lookup modules")
    generate_parser.add_argument("--check", action="store_true", help="fail if outputs are stale")
    args = parser.parse_args(argv)
    try:
        manifest = load_manifest()
        if args.command == "validate":
            validate_all(manifest)
            print(f"Unicode {manifest['unicode_version']} data are valid")
        elif args.command == "generate":
            validate_all(manifest)
            generate(manifest, check=args.check)
    except DataError as err:
        print(f"error: {err}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
