#!/usr/bin/env python3
"""Validate pinned Unicode data and generate narrow Roc lookup modules.

This deliberately uses only the Python standard library so normal CI can run
offline after checkout. The manifest describes independent release and
specification axes, canonical sources, and the artifacts that depend on them.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable
from urllib.parse import urlparse


ROOT = Path(__file__).resolve().parents[1]
UNICODE_VENDOR_ROOT = ROOT / "vendor" / "unicode"
MANIFEST_PATH = UNICODE_VENDOR_ROOT / "manifest.json"
MAX_CODE_POINT = 0x10FFFF
RANGE_RE = re.compile(
    r"^(?P<start>[0-9A-F]{4,6})(?:\.\.(?P<end>[0-9A-F]{4,6}))?"
    r"\s*;\s*(?P<property>[A-Za-z_][A-Za-z0-9_]*)(?:\s*#.*)?$"
)
NUMERIC_RANGE_RE = re.compile(
    r"^(?P<start>[0-9A-F]{4,6})(?:\.\.(?P<end>[0-9A-F]{4,6}))?"
    r"\s*;\s*(?P<property>[0-9]+)(?:\s*#.*)?$"
)
INCB_RE = re.compile(
    r"^(?P<start>[0-9A-F]{4,6})(?:\.\.(?P<end>[0-9A-F]{4,6}))?"
    r"\s*;\s*InCB\s*;\s*(?P<property>[A-Za-z_]+)(?:\s*#.*)?$"
)
HEX_RE = re.compile(r"^[0-9A-F]{4,6}$")
IDENTIFIER_RE = re.compile(r"^[a-z][a-z0-9_-]*$")
MISSING_RE = re.compile(r"^#\s*@missing:\s*(?P<range>[0-9A-F.]+)\s*;\s*(?P<body>.+?)\s*$")

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
LINE_BREAK_PROPERTIES = (
    "AI", "AK", "AL", "AP", "AS", "B2", "BA", "BB", "BK", "CB", "CJ",
    "CL", "CM", "CP", "CR", "EB", "EM", "EX", "GL", "H2", "H3", "HH",
    "HL", "HY", "ID", "IN", "IS", "JL", "JT", "JV", "LF", "NL", "NS",
    "NU", "OP", "PO", "PR", "QU", "RI", "SA", "SG", "SP", "SY", "VF",
    "VI", "WJ", "XX", "ZW", "ZWJ",
)
PUBLIC_ALIAS_PROPERTIES = (
    "Bidi_Class",
    "Bidi_Mirrored",
    "Bidi_Mirroring_Glyph",
    "Bidi_Paired_Bracket",
    "Bidi_Paired_Bracket_Type",
    "Canonical_Combining_Class",
    "Default_Ignorable_Code_Point",
    "East_Asian_Width",
    "Emoji",
    "Emoji_Component",
    "Emoji_Modifier",
    "Emoji_Modifier_Base",
    "Emoji_Presentation",
    "Extended_Pictographic",
    "General_Category",
    "Indic_Positional_Category",
    "Indic_Syllabic_Category",
    "Joining_Group",
    "Joining_Type",
    "Script",
    "Variation_Selector",
    "Vertical_Orientation",
)
SCRIPT_PROPERTIES = ("Script",)
SCRIPT_EXTENSIONS_PROPERTIES = ("Script_Extensions",)
IMPLICIT_SCRIPTS = frozenset(("Zinh", "Zyyy", "Zzzz"))
FORMAL_PROPERTY_ALIASES = {
    "Canonical_Combining_Class": ("ccc",),
    "East_Asian_Width": ("ea",),
    "General_Category": ("gc",),
    "Grapheme_Cluster_Break": ("GCB",),
    "Indic_Conjunct_Break": ("InCB",),
    "Line_Break": ("lb",),
    "Script": ("sc",),
    "Script_Extensions": ("scx",),
}

# DerivedBidiClass-17.0.0 has an ordered default cascade whose ranges are part
# of the data contract, not merely an implementation detail. In particular,
# changing one nontrivial default can silently reclassify large unassigned
# regions while every explicit record remains valid.
BIDI_CLASS_DEFAULTS = (
    (0x0000, 0x10FFFF, "Left_To_Right"),
    (0x0590, 0x05FF, "Right_To_Left"),
    (0x0600, 0x07BF, "Arabic_Letter"),
    (0x07C0, 0x085F, "Right_To_Left"),
    (0x0860, 0x08FF, "Arabic_Letter"),
    (0x20A0, 0x20CF, "European_Terminator"),
    (0xFB1D, 0xFB4F, "Right_To_Left"),
    (0xFB50, 0xFDCF, "Arabic_Letter"),
    (0xFDF0, 0xFDFF, "Arabic_Letter"),
    (0xFE70, 0xFEFF, "Arabic_Letter"),
    (0x10800, 0x10CFF, "Right_To_Left"),
    (0x10D00, 0x10D3F, "Arabic_Letter"),
    (0x10D40, 0x10EBF, "Right_To_Left"),
    (0x10EC0, 0x10EFF, "Arabic_Letter"),
    (0x10F00, 0x10F2F, "Right_To_Left"),
    (0x10F30, 0x10F6F, "Arabic_Letter"),
    (0x10F70, 0x10FFF, "Right_To_Left"),
    (0x1E800, 0x1EC6F, "Right_To_Left"),
    (0x1EC70, 0x1ECBF, "Arabic_Letter"),
    (0x1ECC0, 0x1ECFF, "Right_To_Left"),
    (0x1ED00, 0x1ED4F, "Arabic_Letter"),
    (0x1ED50, 0x1EDFF, "Right_To_Left"),
    (0x1EE00, 0x1EEFF, "Arabic_Letter"),
    (0x1EF00, 0x1EFFF, "Right_To_Left"),
)


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


@dataclass(frozen=True)
class LineBreakCase:
    line: int
    code_points: tuple[int, ...]
    break_offsets: tuple[int, ...]


@dataclass(frozen=True)
class PropertyAlias:
    short: str
    long: str
    aliases: tuple[str, ...]
    line: int


@dataclass(frozen=True)
class PropertyValueAlias:
    property: str
    identity: str
    short: str
    long: str
    aliases: tuple[str, ...]
    line: int


@dataclass(frozen=True)
class MissingDefault:
    start: int
    end: int
    property: str | None
    value: str
    line: int


@dataclass(frozen=True)
class PropertySource:
    records: tuple[RangeRecord, ...]
    defaults: tuple[MissingDefault, ...]


@dataclass(frozen=True, order=True)
class ScriptExtensionRecord:
    start: int
    end: int
    scripts: tuple[str, ...]
    line: int


@dataclass(frozen=True)
class ScriptProperties:
    scripts: tuple[RangeRecord, ...]
    script_default: str
    extensions: tuple[ScriptExtensionRecord, ...]
    aliases: tuple[PropertyValueAlias, ...]


@dataclass(frozen=True)
class AlgorithmProperties:
    grapheme: PropertySource
    east_asian_width: PropertySource
    emoji: PropertySource
    indic_conjunct_break: PropertySource


@dataclass(frozen=True)
class PagedBytes:
    page_index: tuple[int, ...]
    pages: tuple[bytes, ...]
    page_bits: int
    index_type: str

    @property
    def flat_pages(self) -> tuple[int, ...]:
        return tuple(value for page in self.pages for value in page)

    @property
    def storage_bytes(self) -> int:
        index_width = {"U8": 1, "U16": 2, "U32": 4}[self.index_type]
        return len(self.page_index) * index_width + len(self.pages) * (1 << self.page_bits)


@dataclass(frozen=True)
class PagedU16:
    page_index: tuple[int, ...]
    pages: tuple[tuple[int, ...], ...]
    page_bits: int
    index_type: str

    @property
    def flat_pages(self) -> tuple[int, ...]:
        return tuple(value for page in self.pages for value in page)

    @property
    def storage_bytes(self) -> int:
        index_width = {"U8": 1, "U16": 2, "U32": 4}[self.index_type]
        return len(self.page_index) * index_width + len(self.pages) * (1 << self.page_bits) * 2


@dataclass(frozen=True)
class CanonicalProperties:
    general_category: tuple[RangeRecord, ...]
    general_category_default: str
    canonical_combining_class: tuple[RangeRecord, ...]
    canonical_combining_class_default: int
    property_aliases: dict[str, PropertyAlias]
    property_value_aliases: dict[str, tuple[PropertyValueAlias, ...]]


@dataclass(frozen=True)
class SparseMapping:
    source: int
    target: int
    line: int


@dataclass(frozen=True)
class BidiBracket:
    source: int
    target: int
    kind: str
    line: int


@dataclass(frozen=True)
class PublicProperties:
    bidi_class: PropertySource
    bidi_mirrored: tuple[RangeRecord, ...]
    bidi_mirroring_glyph: tuple[SparseMapping, ...]
    bidi_brackets: tuple[BidiBracket, ...]
    joining_type: PropertySource
    joining_group: tuple[RangeRecord, ...]
    indic_syllabic_category: PropertySource
    indic_positional_category: PropertySource
    vertical_orientation: PropertySource
    default_ignorable: tuple[RangeRecord, ...]
    variation_selector: tuple[RangeRecord, ...]
    emoji_variation_bases: tuple[int, ...]


@dataclass(frozen=True)
class GeneratorContract:
    sources: tuple[tuple[str, tuple[str, ...]], ...]
    specifications: tuple[str, ...]
    artifact_generators: tuple[str, ...]
    output: str
    paged: bool = False
    ascii: str | None = None
    value_type: str = "U8"


@dataclass(frozen=True)
class SourceProjectionContract:
    official_suffix: str
    role: str
    release_axes: tuple[str, ...] = ("unicode",)
    emoji_header: bool = False
    has_cases: bool = False
    header: str | None = None


SOURCE_PROJECTION_CONTRACTS = {
    ("ucd-property-ranges", ("East_Asian_Width",)): SourceProjectionContract(
        "ucd/EastAsianWidth.txt", "production-and-conformance"
    ),
    ("ucd-property-ranges", ("Grapheme_Cluster_Break",)): SourceProjectionContract(
        "ucd/auxiliary/GraphemeBreakProperty.txt", "production-and-conformance"
    ),
    ("ucd-property-ranges", ("Line_Break",)): SourceProjectionContract(
        "ucd/LineBreak.txt", "production-and-conformance"
    ),
    ("ucd-derived-property-ranges", ("Line_Break",)): SourceProjectionContract(
        "ucd/extracted/DerivedLineBreak.txt", "conformance"
    ),
    ("uax-14-test", ()): SourceProjectionContract(
        "ucd/auxiliary/LineBreakTest.txt", "conformance", has_cases=True
    ),
    ("uax-29-test", ()): SourceProjectionContract(
        "ucd/auxiliary/GraphemeBreakTest.txt", "conformance", has_cases=True
    ),
    ("ucd-binary-property-ranges", EMOJI_PROPERTIES): SourceProjectionContract(
        "ucd/emoji/emoji-data.txt",
        "production-and-conformance",
        ("unicode", "emoji"),
        True,
    ),
    ("ucd-derived-core-properties", ("Default_Ignorable_Code_Point", "Indic_Conjunct_Break")): SourceProjectionContract(
        "ucd/DerivedCoreProperties.txt", "production-and-conformance"
    ),
    ("ucd-property-ranges", ("General_Category",)): SourceProjectionContract(
        "ucd/extracted/DerivedGeneralCategory.txt", "production"
    ),
    ("ucd-numeric-property-ranges", ("Canonical_Combining_Class",)): SourceProjectionContract(
        "ucd/extracted/DerivedCombiningClass.txt", "production"
    ),
    ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES): SourceProjectionContract(
        "ucd/PropertyAliases.txt", "production-metadata"
    ),
    ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES): SourceProjectionContract(
        "ucd/PropertyValueAliases.txt", "production-metadata"
    ),
    ("ucd-property-ranges", SCRIPT_PROPERTIES): SourceProjectionContract(
        "ucd/Scripts.txt", "production-and-conformance"
    ),
    ("ucd-script-extensions", SCRIPT_EXTENSIONS_PROPERTIES): SourceProjectionContract(
        "ucd/ScriptExtensions.txt", "production-and-conformance"
    ),
    ("ucd-property-ranges", ("Bidi_Class",)): SourceProjectionContract(
        "ucd/extracted/DerivedBidiClass.txt", "production"
    ),
    ("ucd-unicode-data", ("Bidi_Mirrored",)): SourceProjectionContract(
        "ucd/UnicodeData.txt",
        "production",
        header="0000;<control>;Cc;0;BN;;;;;N;NULL;;;;",
    ),
    ("ucd-bidi-mirroring", ("Bidi_Mirroring_Glyph",)): SourceProjectionContract(
        "ucd/BidiMirroring.txt", "production"
    ),
    ("ucd-bidi-brackets", ("Bidi_Paired_Bracket", "Bidi_Paired_Bracket_Type")): SourceProjectionContract(
        "ucd/BidiBrackets.txt", "production"
    ),
    ("ucd-property-ranges", ("Joining_Type",)): SourceProjectionContract(
        "ucd/extracted/DerivedJoiningType.txt", "production"
    ),
    ("ucd-arabic-shaping", ("Joining_Type", "Joining_Group")): SourceProjectionContract(
        "ucd/ArabicShaping.txt", "production"
    ),
    ("ucd-property-ranges", ("Indic_Syllabic_Category",)): SourceProjectionContract(
        "ucd/IndicSyllabicCategory.txt", "production"
    ),
    ("ucd-property-ranges", ("Indic_Positional_Category",)): SourceProjectionContract(
        "ucd/IndicPositionalCategory.txt", "production"
    ),
    ("ucd-binary-property-ranges", ("Variation_Selector",)): SourceProjectionContract(
        "ucd/PropList.txt", "production"
    ),
    ("ucd-property-ranges", ("Vertical_Orientation",)): SourceProjectionContract(
        "ucd/VerticalOrientation.txt", "production"
    ),
    ("ucd-emoji-variation-sequences", ("Emoji_Variation_Sequence",)): SourceProjectionContract(
        "ucd/emoji/emoji-variation-sequences.txt",
        "production",
        ("unicode", "emoji"),
        True,
    ),
}

SPECIFICATION_COMPATIBILITY = {
    "uax_9": ("unicode", "17.0.0", "51"),
    "uax_11": ("unicode", "17.0.0", "44"),
    "uax_14": ("unicode", "17.0.0", "55"),
    "uax_24": ("unicode", "17.0.0", "39"),
    "uax_29": ("unicode", "17.0.0", "47"),
    "uax_44": ("unicode", "17.0.0", "36"),
    "uax_50": ("unicode", "17.0.0", "33"),
    "uts_51": ("emoji", "17.0", "29"),
}

GENERATOR_CONTRACTS = {
    "unicode-version": GeneratorContract((), ("uax_44",), (), "package/UnicodeVersion.roc"),
    "grapheme-data": GeneratorContract(
        (
            ("ucd-property-ranges", ("Grapheme_Cluster_Break",)),
            ("ucd-derived-core-properties", ("Default_Ignorable_Code_Point", "Indic_Conjunct_Break")),
            ("ucd-binary-property-ranges", EMOJI_PROPERTIES),
        ),
        ("uax_29", "uts_51"),
        (),
        "package/InternalGraphemeData.roc",
        True,
        "computed",
    ),
    "legacy-grapheme-break": GeneratorContract(
        (("ucd-property-ranges", ("Grapheme_Cluster_Break",)),),
        ("uax_29",),
        (),
        "package/InternalGBP.roc",
    ),
    "east-asian-width": GeneratorContract(
        (("ucd-property-ranges", ("East_Asian_Width",)),),
        ("uax_11",),
        (),
        "package/InternalEAW.roc",
    ),
    "emoji-properties": GeneratorContract(
        (("ucd-binary-property-ranges", EMOJI_PROPERTIES),),
        ("uts_51",),
        (),
        "package/InternalEmojiData.roc",
        True,
        "computed",
    ),
    "legacy-emoji": GeneratorContract(
        (), ("uts_51",), ("emoji-properties",), "package/InternalEmoji.roc"
    ),
    "line-break-data": GeneratorContract(
        (
            ("ucd-property-ranges", ("Line_Break",)),
            ("ucd-derived-property-ranges", ("Line_Break",)),
        ),
        ("uax_14",),
        ("general-category", "east-asian-width", "emoji-properties"),
        "package/InternalLineBreakData.roc",
        True,
        "computed",
    ),
    "general-category": GeneratorContract(
        (
            ("ucd-property-ranges", ("General_Category",)),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44",),
        (),
        "package/InternalGeneralCategory.roc",
        True,
        "computed",
    ),
    "canonical-combining-class": GeneratorContract(
        (
            ("ucd-numeric-property-ranges", ("Canonical_Combining_Class",)),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44",),
        (),
        "package/InternalCanonicalCombiningClass.roc",
        True,
        "constant-zero",
    ),
    "property-aliases": GeneratorContract(
        (
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-ranges", ("General_Category",)),
        ),
        ("uax_44",),
        ("general-category",),
        "package/InternalPropertyAliases.roc",
    ),
    "bidi-properties": GeneratorContract(
        (
            ("ucd-property-ranges", ("Bidi_Class",)),
            ("ucd-unicode-data", ("Bidi_Mirrored",)),
            ("ucd-bidi-mirroring", ("Bidi_Mirroring_Glyph",)),
            ("ucd-bidi-brackets", ("Bidi_Paired_Bracket", "Bidi_Paired_Bracket_Type")),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_9", "uax_44"),
        (),
        "package/InternalBidiProperties.roc",
        True,
        "computed",
    ),
    "joining-type": GeneratorContract(
        (
            ("ucd-property-ranges", ("Joining_Type",)),
            ("ucd-arabic-shaping", ("Joining_Type", "Joining_Group")),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44",),
        (),
        "package/InternalJoiningType.roc",
        True,
        "computed",
    ),
    "joining-group": GeneratorContract(
        (
            ("ucd-arabic-shaping", ("Joining_Type", "Joining_Group")),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44",),
        (),
        "package/InternalJoiningGroup.roc",
        True,
        "constant-zero",
    ),
    "indic-syllabic-category": GeneratorContract(
        (
            ("ucd-property-ranges", ("Indic_Syllabic_Category",)),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44",),
        (),
        "package/InternalIndicSyllabicCategory.roc",
        True,
        "constant-zero",
    ),
    "indic-positional-category": GeneratorContract(
        (
            ("ucd-property-ranges", ("Indic_Positional_Category",)),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44",),
        (),
        "package/InternalIndicPositionalCategory.roc",
        True,
        "constant-zero",
    ),
    "vertical-orientation": GeneratorContract(
        (
            ("ucd-property-ranges", ("Vertical_Orientation",)),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_44", "uax_50"),
        (),
        "package/InternalVerticalOrientation.roc",
        True,
        "computed",
    ),
    "character-flags": GeneratorContract(
        (
            ("ucd-derived-core-properties", ("Default_Ignorable_Code_Point", "Indic_Conjunct_Break")),
            ("ucd-binary-property-ranges", ("Variation_Selector",)),
        ),
        ("uax_44",),
        (),
        "package/InternalCharacterFlags.roc",
        True,
        "constant-zero",
    ),
    "emoji-variations": GeneratorContract(
        (("ucd-emoji-variation-sequences", ("Emoji_Variation_Sequence",)),),
        ("uts_51",),
        (),
        "package/InternalEmojiVariations.roc",
    ),
    "composite-properties": GeneratorContract(
        (
            ("ucd-property-ranges", ("General_Category",)),
            ("ucd-numeric-property-ranges", ("Canonical_Combining_Class",)),
            ("ucd-property-ranges", ("East_Asian_Width",)),
            ("ucd-binary-property-ranges", EMOJI_PROPERTIES),
            ("ucd-property-ranges", ("Bidi_Class",)),
            ("ucd-unicode-data", ("Bidi_Mirrored",)),
            ("ucd-property-ranges", ("Joining_Type",)),
            ("ucd-arabic-shaping", ("Joining_Type", "Joining_Group")),
            ("ucd-property-ranges", ("Indic_Syllabic_Category",)),
            ("ucd-property-ranges", ("Indic_Positional_Category",)),
            ("ucd-property-ranges", ("Vertical_Orientation",)),
            ("ucd-derived-core-properties", ("Default_Ignorable_Code_Point", "Indic_Conjunct_Break")),
            ("ucd-binary-property-ranges", ("Variation_Selector",)),
            ("ucd-property-aliases", PUBLIC_ALIAS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_9", "uax_11", "uax_44", "uax_50", "uts_51"),
        (),
        "package/InternalCompositeProperties.roc",
        True,
        "computed",
        "U16",
    ),
    "script-data": GeneratorContract(
        (
            ("ucd-property-ranges", SCRIPT_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_24", "uax_44"),
        (),
        "package/InternalScriptData.roc",
        True,
        "computed",
    ),
    "script-extensions-data": GeneratorContract(
        (
            ("ucd-script-extensions", SCRIPT_EXTENSIONS_PROPERTIES),
            ("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),
        ),
        ("uax_24", "uax_44"),
        ("script-data",),
        "package/InternalScriptExtensionsData.roc",
        True,
        "constant-zero",
    ),
    "script-api": GeneratorContract(
        (("ucd-property-value-aliases", PUBLIC_ALIAS_PROPERTIES),),
        ("uax_24", "uax_44"),
        ("script-data", "script-extensions-data"),
        "package/Script.roc",
    ),
}

PAGED_LAYOUT_FIELDS = frozenset(
    (
        "kind",
        "candidate_page_bits",
        "page_bits",
        "index_type",
        "value_type",
        "expected_index_entries",
        "expected_distinct_pages",
        "expected_logical_bytes",
        "max_logical_bytes",
        "ascii",
    )
)

COMPOSITE_LAYOUT_FIELDS = frozenset(
    ("expected_row_count", "expected_column_bytes", "max_total_bytes")
)

def _require_dict(value: object, context: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise DataError(f"{context} must be a JSON object")
    return value


def _require_string_list(value: object, context: str) -> list[str]:
    if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
        raise DataError(f"{context} must be a JSON array of strings")
    return value


def _validate_identifier(name: str, context: str) -> None:
    if IDENTIFIER_RE.fullmatch(name) is None:
        raise DataError(f"{context} has invalid identifier {name!r}")


def _path_below_root(value: str, context: str) -> Path:
    relative = Path(value)
    if relative.is_absolute() or ".." in relative.parts:
        raise DataError(f"{context} must stay below the repository root")
    return ROOT / relative


def _require_fields(
    item: dict[str, object], required: Iterable[str], context: str
) -> None:
    expected = frozenset(required)
    actual = frozenset(item)
    if actual != expected:
        missing = sorted(expected - actual)
        surplus = sorted(actual - expected)
        raise DataError(f"{context} fields drifted; missing={missing}, surplus={surplus}")


def _version_tuple(version: str, components: int, context: str) -> tuple[int, ...]:
    fields = version.split(".")
    if len(fields) != components or any(not field.isdigit() for field in fields):
        raise DataError(f"{context} must have {components} numeric components")
    return tuple(int(field) for field in fields)


def release_version(manifest: dict[str, object], name: str) -> str:
    releases = _require_dict(manifest["releases"], "manifest.releases")
    release = _require_dict(releases[name], f"manifest.releases.{name}")
    return str(release["version"])


def load_manifest(path: Path = MANIFEST_PATH) -> dict[str, object]:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as err:
        raise DataError(f"unable to read {path}: {err}") from err
    manifest = _require_dict(raw, "manifest")
    if manifest.get("schema_version") != 2:
        raise DataError("manifest schema_version must be 2")
    _require_fields(
        manifest,
        ("schema_version", "releases", "specifications", "authorities", "licenses", "sources", "artifacts"),
        "manifest",
    )

    releases = _require_dict(manifest.get("releases"), "manifest.releases")
    authorities = _require_dict(manifest.get("authorities"), "manifest.authorities")
    licenses = _require_dict(manifest.get("licenses"), "manifest.licenses")
    specifications = _require_dict(manifest.get("specifications"), "manifest.specifications")
    sources = _require_dict(manifest.get("sources"), "manifest.sources")
    artifacts = _require_dict(manifest.get("artifacts"), "manifest.artifacts")
    if not releases or not sources or not artifacts:
        raise DataError("manifest releases, sources, and artifacts must not be empty")
    if licenses != {"unicode-data": {"path": "vendor/unicode/LICENSE-UNICODE"}}:
        raise DataError("manifest Unicode data license provenance drifted")
    if authorities != {
        "unicode": {
            "url_prefix": "https://www.unicode.org/",
            "license": "unicode-data",
        }
    }:
        raise DataError("manifest Unicode authority provenance drifted")

    for name, raw_license in licenses.items():
        _validate_identifier(name, "manifest.licenses")
        item = _require_dict(raw_license, f"manifest.licenses.{name}")
        _require_fields(item, ("path",), f"manifest.licenses.{name}")
        if not isinstance(item.get("path"), str):
            raise DataError(f"manifest.licenses.{name}.path has the wrong type")
        _path_below_root(str(item["path"]), f"manifest.licenses.{name}.path")

    for name, raw_authority in authorities.items():
        _validate_identifier(name, "manifest.authorities")
        item = _require_dict(raw_authority, f"manifest.authorities.{name}")
        _require_fields(item, ("url_prefix", "license"), f"manifest.authorities.{name}")
        if not isinstance(item.get("url_prefix"), str) or not str(item["url_prefix"]).startswith("https://"):
            raise DataError(f"manifest.authorities.{name}.url_prefix must be an HTTPS URL")
        if item.get("license") not in licenses:
            raise DataError(f"manifest.authorities.{name}.license names an unknown license")

    for name, raw_release in releases.items():
        _validate_identifier(name, "manifest.releases")
        item = _require_dict(raw_release, f"manifest.releases.{name}")
        for field in ("version", "authority", "kind"):
            if not isinstance(item.get(field), str):
                raise DataError(f"manifest.releases.{name}.{field} has the wrong type")
        if item["authority"] not in authorities:
            raise DataError(f"manifest.releases.{name}.authority names an unknown authority")
        kind = str(item["kind"])
        if kind == "ucd":
            _require_fields(
                item, ("version", "authority", "kind", "vendor_prefix"), f"manifest.releases.{name}"
            )
            _version_tuple(str(item["version"]), 3, f"manifest.releases.{name}.version")
            prefix = item.get("vendor_prefix")
            if not isinstance(prefix, str):
                raise DataError(f"manifest.releases.{name}.vendor_prefix has the wrong type")
            _path_below_root(prefix, f"manifest.releases.{name}.vendor_prefix")
            if Path(prefix).name != item["version"]:
                raise DataError(
                    f"manifest.releases.{name}.vendor_prefix does not match its storage release exact version"
                )
        elif kind == "emoji":
            _require_fields(
                item, ("version", "authority", "kind", "synchronized_with"), f"manifest.releases.{name}"
            )
            emoji_tuple = _version_tuple(
                str(item["version"]), 2, f"manifest.releases.{name}.version"
            )
            synchronized = item.get("synchronized_with")
            if not isinstance(synchronized, str) or synchronized not in releases:
                raise DataError(f"manifest.releases.{name}.synchronized_with names an unknown release")
            synchronized_release = _require_dict(
                releases[synchronized], f"manifest.releases.{synchronized}"
            )
            if (
                synchronized_release.get("kind") != "ucd"
                or synchronized_release.get("authority") != item["authority"]
            ):
                raise DataError(
                    f"manifest.releases.{name}.synchronized_with must name a Unicode UCD release"
                )
            unicode_tuple = _version_tuple(
                str(synchronized_release.get("version")),
                3,
                f"manifest.releases.{synchronized}.version",
            )
            if emoji_tuple != unicode_tuple[:2]:
                raise DataError(
                    f"manifest.releases.{name} is not synchronized to the Unicode major/minor version"
                )
        elif kind == "cldr":
            _require_fields(
                item, ("version", "authority", "kind", "vendor_prefix"), f"manifest.releases.{name}"
            )
            if re.fullmatch(r"[0-9]+(?:\.[0-9]+)*", str(item["version"])) is None:
                raise DataError(f"manifest.releases.{name}.version has invalid CLDR form")
            prefix = item.get("vendor_prefix")
            if not isinstance(prefix, str):
                raise DataError(f"manifest.releases.{name}.vendor_prefix has the wrong type")
            _path_below_root(prefix, f"manifest.releases.{name}.vendor_prefix")
            if Path(prefix).name != item["version"] or "cldr" not in Path(prefix).parts:
                raise DataError(
                    f"manifest.releases.{name}.vendor_prefix does not identify its CLDR version"
                )
        else:
            raise DataError(
                f"manifest.releases.{name}.kind {kind!r} has no implemented provenance validator"
            )

    release_visiting: set[str] = set()
    release_visited: set[str] = set()

    def visit_release(name: str) -> None:
        if name in release_visiting:
            raise DataError(f"manifest release synchronization cycle at {name!r}")
        if name in release_visited:
            return
        release_visiting.add(name)
        release = _require_dict(releases[name], f"manifest.releases.{name}")
        synchronized = release.get("synchronized_with")
        if isinstance(synchronized, str):
            visit_release(synchronized)
        release_visiting.remove(name)
        release_visited.add(name)

    for name in releases:
        visit_release(name)

    if frozenset(specifications) != frozenset(SPECIFICATION_COMPATIBILITY):
        raise DataError("manifest specification set drifted")
    for name, raw_specification in specifications.items():
        _validate_identifier(name, "manifest.specifications")
        item = _require_dict(raw_specification, f"manifest.specifications.{name}")
        _require_fields(item, ("revision", "release", "document"), f"manifest.specifications.{name}")
        if not isinstance(item.get("revision"), str):
            raise DataError(f"manifest.specifications.{name}.revision has the wrong type")
        if item.get("release") not in releases:
            raise DataError(f"manifest.specifications.{name}.release names an unknown release")
        expected_release, expected_version, expected_revision = SPECIFICATION_COMPATIBILITY[name]
        if (
            item["release"] != expected_release
            or release_version(manifest, expected_release) != expected_version
            or item["revision"] != expected_revision
        ):
            raise DataError(
                f"manifest.specifications.{name} is incompatible with its exact release"
            )
        number_match = re.fullmatch(r"(?:uax|uts)_([0-9]+)", name)
        if number_match is None or not isinstance(item.get("document"), str):
            raise DataError(f"manifest.specifications.{name} has invalid document metadata")
        number = number_match.group(1)
        expected_document = (
            f"https://www.unicode.org/reports/tr{number}/tr{number}-{item['revision']}.html"
        )
        if item["document"] != expected_document:
            raise DataError(f"manifest.specifications.{name}.document does not pin its revision")

    for name, raw_source in sources.items():
        _validate_identifier(name, "manifest.sources")
        item = _require_dict(raw_source, f"manifest.sources.{name}")
        source_format = item.get("format")
        properties = _require_string_list(
            item.get("properties"), f"manifest.sources.{name}.properties"
        )
        projection = (str(source_format), tuple(properties))
        projection_contract = SOURCE_PROJECTION_CONTRACTS.get(projection)
        if projection_contract is None:
            raise DataError(
                f"manifest.sources.{name} format/properties have no implemented parser"
            )
        source_fields = {
            "path", "url", "sha256", "header", "records", "role", "authority",
            "storage_release", "release_axes", "format", "properties",
        }
        if projection_contract.has_cases:
            source_fields.add("cases")
        _require_fields(item, source_fields, f"manifest.sources.{name}")
        for field, expected_type in (
            ("path", str),
            ("url", str),
            ("sha256", str),
            ("header", str),
            ("records", int),
            ("role", str),
            ("authority", str),
            ("storage_release", str),
            ("format", str),
        ):
            if not isinstance(item.get(field), expected_type):
                raise DataError(f"manifest.sources.{name}.{field} has the wrong type")
        path_value = str(item["path"])
        _path_below_root(path_value, f"manifest.sources.{name}.path")
        authority_name = str(item["authority"])
        if authority_name not in authorities:
            raise DataError(f"manifest.sources.{name}.authority names an unknown authority")
        authority = _require_dict(authorities[authority_name], f"manifest.authorities.{authority_name}")
        if not str(item["url"]).startswith(str(authority["url_prefix"])):
            raise DataError(f"manifest.sources.{name}.url does not match its authority")
        if re.fullmatch(r"[0-9a-f]{64}", str(item["sha256"])) is None:
            raise DataError(f"manifest.sources.{name}.sha256 is not a SHA-256 digest")
        storage_release = str(item["storage_release"])
        if storage_release not in releases:
            raise DataError(f"manifest.sources.{name}.storage_release names an unknown release")
        storage = _require_dict(releases[storage_release], f"manifest.releases.{storage_release}")
        if storage.get("authority") != authority_name:
            raise DataError(f"manifest.sources.{name}.authority does not match its storage release")
        prefix = storage.get("vendor_prefix")
        if not isinstance(prefix, str) or Path(path_value).parts[: len(Path(prefix).parts)] != Path(prefix).parts:
            raise DataError(f"manifest.sources.{name}.path does not match its storage release")
        release_axes = _require_string_list(item.get("release_axes"), f"manifest.sources.{name}.release_axes")
        if (
            storage_release not in release_axes
            or any(axis not in releases for axis in release_axes)
            or len(release_axes) != len(set(release_axes))
        ):
            raise DataError(f"manifest.sources.{name}.release_axes are inconsistent")
        if len(properties) != len(set(properties)):
            raise DataError(f"manifest.sources.{name}.properties contains duplicates")
        if item["role"] != projection_contract.role:
            raise DataError(f"manifest.sources.{name}.role does not match its parser contract")
        if projection_contract.has_cases and item.get("cases") != item["records"]:
            raise DataError(f"manifest.sources.{name}.cases must exactly match data records")
        unicode_version = release_version(manifest, storage_release)
        authority_prefix = str(authority["url_prefix"])
        expected_url_prefix = f"{authority_prefix}Public/{unicode_version}/"
        expected_url = expected_url_prefix + projection_contract.official_suffix
        if item["url"] != expected_url:
            raise DataError(f"manifest.sources.{name}.url does not identify its exact release")
        if Path(urlparse(str(item["url"])).path).name != Path(path_value).name:
            raise DataError(f"manifest.sources.{name}.url and path identify different files")
        if projection_contract.header is not None:
            expected_header = projection_contract.header
        elif projection_contract.emoji_header:
            expected_header = f"# Version: {release_version(manifest, 'emoji')}"
        else:
            expected_header = f"# {Path(path_value).stem}-{unicode_version}.txt"
        if item["header"] != expected_header:
            raise DataError(f"manifest.sources.{name}.header does not identify its exact release")
        if tuple(release_axes) != projection_contract.release_axes:
            raise DataError(f"manifest.sources.{name}.release_axes do not match parser semantics")
        if projection_contract.emoji_header:
            emoji_release = _require_dict(releases["emoji"], "manifest.releases.emoji")
            if emoji_release.get("synchronized_with") != storage_release:
                raise DataError(
                    f"manifest.sources.{name} storage release does not match Emoji synchronization"
                )
        if any(
            _require_dict(releases[axis], f"manifest.releases.{axis}").get("authority")
            != authority_name
            for axis in release_axes
        ):
            raise DataError(f"manifest.sources.{name}.release axes have incompatible authorities")

    outputs: set[str] = set()
    dependencies: dict[str, list[str]] = {}
    declared_generators: set[str] = set()
    for name, raw_artifact in artifacts.items():
        _validate_identifier(name, "manifest.artifacts")
        item = _require_dict(raw_artifact, f"manifest.artifacts.{name}")
        generator_name = item.get("generator")
        if not isinstance(generator_name, str) or generator_name not in GENERATOR_CONTRACTS:
            raise DataError(f"manifest.artifacts.{name}.generator is not implemented")
        if generator_name in declared_generators:
            raise DataError(f"manifest generator {generator_name!r} is declared more than once")
        declared_generators.add(generator_name)
        contract = GENERATOR_CONTRACTS[generator_name]
        artifact_fields = {"generator", "output", "sources", "specifications", "artifacts"}
        if contract.paged:
            artifact_fields.add("layout")
        _require_fields(item, artifact_fields, f"manifest.artifacts.{name}")
        for field in ("generator", "output"):
            if not isinstance(item.get(field), str):
                raise DataError(f"manifest.artifacts.{name}.{field} has the wrong type")
        output = str(item["output"])
        _path_below_root(output, f"manifest.artifacts.{name}.output")
        if output != contract.output:
            raise DataError(
                f"manifest.artifacts.{name}.output must be the authoritative generated module {contract.output!r}"
            )
        if output in outputs:
            raise DataError(f"manifest artifact output {output!r} is duplicated")
        outputs.add(output)
        source_dependencies = _require_string_list(item.get("sources"), f"manifest.artifacts.{name}.sources")
        spec_dependencies = _require_string_list(item.get("specifications"), f"manifest.artifacts.{name}.specifications")
        artifact_dependencies = _require_string_list(item.get("artifacts"), f"manifest.artifacts.{name}.artifacts")
        for field, values in (
            ("sources", source_dependencies),
            ("specifications", spec_dependencies),
            ("artifacts", artifact_dependencies),
        ):
            if len(values) != len(set(values)):
                raise DataError(f"manifest.artifacts.{name}.{field} contains duplicates")
        if any(source not in sources for source in source_dependencies):
            raise DataError(f"manifest.artifacts.{name}.sources names an unknown source")
        if any(specification not in specifications for specification in spec_dependencies):
            raise DataError(f"manifest.artifacts.{name}.specifications names an unknown specification")
        if any(artifact not in artifacts for artifact in artifact_dependencies):
            raise DataError(f"manifest.artifacts.{name}.artifacts names an unknown artifact")
        declared_source_projections = tuple(
            (
                str(_require_dict(sources[source], f"manifest.sources.{source}")["format"]),
                tuple(
                    _require_string_list(
                        _require_dict(sources[source], f"manifest.sources.{source}")["properties"],
                        f"manifest.sources.{source}.properties",
                    )
                ),
            )
            for source in source_dependencies
        )
        declared_artifact_generators = tuple(
            str(_require_dict(artifacts[artifact], f"manifest.artifacts.{artifact}")["generator"])
            for artifact in artifact_dependencies
        )
        if (
            declared_source_projections != contract.sources
            or tuple(spec_dependencies) != contract.specifications
            or declared_artifact_generators != contract.artifact_generators
        ):
            raise DataError(
                f"manifest.artifacts.{name} dependencies do not exactly match its implemented generator"
            )
        if contract.paged:
            layout = _require_dict(item["layout"], f"manifest.artifacts.{name}.layout")
            layout_fields = (
                PAGED_LAYOUT_FIELDS | COMPOSITE_LAYOUT_FIELDS
                if contract.value_type == "U16"
                else PAGED_LAYOUT_FIELDS
            )
            _require_fields(layout, layout_fields, f"manifest.artifacts.{name}.layout")
            candidates = layout.get("candidate_page_bits")
            if candidates != [6, 7, 8, 9, 10]:
                raise DataError(f"manifest.artifacts.{name}.layout candidates drifted")
            if (
                layout.get("kind") != "deduplicated-pages"
                or layout.get("index_type") != "U8"
                and layout.get("index_type") != "U16"
                or layout.get("value_type") != contract.value_type
                or layout.get("ascii") != contract.ascii
            ):
                raise DataError(f"manifest.artifacts.{name}.layout has unsupported representation")
            for field in (
                "page_bits", "expected_index_entries", "expected_distinct_pages",
                "expected_logical_bytes", "max_logical_bytes",
            ):
                if not isinstance(layout.get(field), int):
                    raise DataError(f"manifest.artifacts.{name}.layout.{field} has the wrong type")
            if layout["expected_index_entries"] != (MAX_CODE_POINT + 1) >> int(layout["page_bits"]):
                raise DataError(f"manifest.artifacts.{name}.layout index extent drifted")
            if int(layout["expected_logical_bytes"]) > int(layout["max_logical_bytes"]):
                raise DataError(f"manifest.artifacts.{name}.layout exceeds its budget")
            if contract.value_type == "U16":
                for field in COMPOSITE_LAYOUT_FIELDS:
                    if not isinstance(layout.get(field), int):
                        raise DataError(
                            f"manifest.artifacts.{name}.layout.{field} has the wrong type"
                        )
                total = int(layout["expected_logical_bytes"]) + int(
                    layout["expected_column_bytes"]
                )
                if total > int(layout["max_total_bytes"]):
                    raise DataError(
                        f"manifest.artifacts.{name}.layout exceeds its total byte budget"
                    )
        dependencies[name] = artifact_dependencies

    if declared_generators != set(GENERATOR_CONTRACTS):
        missing = sorted(set(GENERATOR_CONTRACTS) - declared_generators)
        raise DataError(f"manifest omits implemented generators {missing}")

    visiting: set[str] = set()
    visited: set[str] = set()

    def visit(name: str) -> None:
        if name in visiting:
            raise DataError(f"manifest artifact dependency cycle at {name!r}")
        if name in visited:
            return
        visiting.add(name)
        for dependency in dependencies[name]:
            visit(dependency)
        visiting.remove(name)
        visited.add(name)

    for name in artifacts:
        visit(name)
    return manifest


def _entry(manifest: dict[str, object], name: str) -> dict[str, object]:
    sources = _require_dict(manifest["sources"], "manifest.sources")
    item = _require_dict(sources[name], f"manifest.sources.{name}")
    for field, expected_type in (
        ("path", str),
        ("url", str),
        ("sha256", str),
        ("header", str),
        ("records", int),
        ("role", str),
    ):
        if not isinstance(item.get(field), expected_type):
            raise DataError(f"manifest.sources.{name}.{field} has the wrong type")
    return item


def _source_for(
    manifest: dict[str, object], source_format: str, properties: tuple[str, ...]
) -> str:
    sources = _require_dict(manifest["sources"], "manifest.sources")
    matches = []
    for name, raw_source in sources.items():
        item = _require_dict(raw_source, f"manifest.sources.{name}")
        if item.get("format") == source_format and tuple(
            _require_string_list(item.get("properties"), f"manifest.sources.{name}.properties")
        ) == properties:
            matches.append(name)
    if len(matches) != 1:
        raise DataError(
            f"manifest must declare exactly one {source_format!r} source for {properties!r}"
        )
    return matches[0]


def _artifact_for_generator(manifest: dict[str, object], generator: str) -> str:
    artifacts = _require_dict(manifest["artifacts"], "manifest.artifacts")
    matches = [
        name
        for name, raw_artifact in artifacts.items()
        if _require_dict(raw_artifact, f"manifest.artifacts.{name}").get("generator") == generator
    ]
    if len(matches) != 1:
        raise DataError(f"manifest must declare exactly one {generator!r} generator")
    return matches[0]


def _module_for_generator(manifest: dict[str, object], generator: str) -> str:
    artifact_name = _artifact_for_generator(manifest, generator)
    artifacts = _require_dict(manifest["artifacts"], "manifest.artifacts")
    artifact = _require_dict(
        artifacts[artifact_name], f"manifest.artifacts.{artifact_name}"
    )
    output = Path(str(artifact["output"]))
    contract = GENERATOR_CONTRACTS[generator]
    if (
        output.as_posix() != contract.output
        or output.parent != Path("package")
        or output.suffix != ".roc"
    ):
        raise DataError(
            f"manifest.artifacts.{artifact_name}.output is not a generated package module"
        )
    return output.stem


def data_path(manifest: dict[str, object], name: str) -> Path:
    relative = Path(str(_entry(manifest, name)["path"]))
    if relative.is_absolute() or ".." in relative.parts:
        raise DataError(f"manifest path for {name} must stay below the repository root")
    return ROOT / relative


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
    if text.splitlines().count(str(item["header"])) != 1:
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
    default_marker: str | None,
    overlaps_by_property: bool = False,
) -> list[RangeRecord]:
    if default_marker is not None and default_marker not in text:
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


def loose_alias(value: str) -> str:
    """UAX #44 revision 36, LM3 loose matching for ASCII property aliases."""
    ignored_whitespace = {
        "\u0009", "\u000A", "\u000B", "\u000C", "\u000D", "\u0020",
        "\u0085", "\u00A0", "\u1680", "\u2000", "\u2001", "\u2002",
        "\u2003", "\u2004", "\u2005", "\u2006", "\u2007", "\u2008",
        "\u2009", "\u200A", "\u2028", "\u2029", "\u202F", "\u205F",
        "\u3000",
    }
    normalized = "".join(
        character.lower()
        for character in value
        if character not in "_-" and character not in ignored_whitespace
    )
    return normalized[2:] if len(normalized) > 2 and normalized.startswith("is") else normalized


def loose_alias_hash(value: str) -> int:
    """Allocation-free runtime-compatible FNV-1a over an ASCII loose alias."""
    result = 2166136261
    for byte in value.encode("utf-8"):
        if byte in (0x20, 0x2D, 0x5F):
            continue
        if 0x41 <= byte <= 0x5A:
            byte += 0x20
        result = ((result ^ byte) * 16777619) & 0xFFFFFFFF
    return result


def parse_property_aliases(text: str, *, source: str) -> dict[str, PropertyAlias]:
    aliases: dict[str, PropertyAlias] = {}
    loose_to_short: dict[str, str] = {}
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";"))
        if len(fields) < 2 or any(not field for field in fields):
            raise DataError(f"{source}:{line_number}: malformed property alias line")
        short, long, *additional = fields
        if short in aliases:
            raise DataError(f"{source}:{line_number}: duplicate property identity {short!r}")
        record = PropertyAlias(short, long, tuple(additional), line_number)
        aliases[short] = record
        for alias in (short, long, *additional):
            loose = loose_alias(alias)
            previous = loose_to_short.get(loose)
            if previous is not None and previous != short:
                raise DataError(
                    f"{source}:{line_number}: property alias {alias!r} collides with {previous!r}"
                )
            loose_to_short[loose] = short
    if not aliases:
        raise DataError(f"{source}: no property aliases")
    return aliases


def parse_property_value_aliases(
    text: str, *, source: str
) -> dict[str, tuple[PropertyValueAlias, ...]]:
    by_property: dict[str, list[PropertyValueAlias]] = {}
    loose_values: dict[str, dict[str, str]] = {}
    identities: dict[str, set[str]] = {}
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";"))
        if len(fields) < 3 or any(not field for field in fields):
            raise DataError(f"{source}:{line_number}: malformed property-value alias line")
        prop = fields[0]
        if prop == "ccc":
            if len(fields) < 4 or not fields[1].isdigit():
                raise DataError(f"{source}:{line_number}: malformed CCC value alias")
            identity, short, long = fields[1:4]
            additional = fields[4:]
            all_aliases = (identity, short, long, *additional)
        else:
            identity, long = fields[1:3]
            short = identity
            additional = fields[3:]
            all_aliases = (short, long, *additional)
        if identity in identities.setdefault(prop, set()):
            raise DataError(
                f"{source}:{line_number}: duplicate {prop!r} value identity {identity!r}"
            )
        identities[prop].add(identity)
        record = PropertyValueAlias(prop, identity, short, long, tuple(additional), line_number)
        by_property.setdefault(prop, []).append(record)
        values = loose_values.setdefault(prop, {})
        for alias in all_aliases:
            loose = loose_alias(alias)
            previous = values.get(loose)
            if previous is not None and previous != identity:
                raise DataError(
                    f"{source}:{line_number}: {prop!r} alias {alias!r} collides with {previous!r}"
                )
            values[loose] = identity
    if not by_property:
        raise DataError(f"{source}: no property-value aliases")
    return {prop: tuple(records) for prop, records in by_property.items()}


def parse_missing_defaults(text: str, *, source: str) -> tuple[MissingDefault, ...]:
    defaults: list[MissingDefault] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        match = MISSING_RE.fullmatch(raw_line.strip())
        if match is None:
            continue
        range_fields = match.group("range").split("..")
        if len(range_fields) not in (1, 2) or any(HEX_RE.fullmatch(value) is None for value in range_fields):
            raise DataError(f"{source}:{line_number}: malformed @missing range")
        start = int(range_fields[0], 16)
        end = int(range_fields[-1], 16)
        if start > end or end > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: invalid @missing range")
        body = tuple(field.strip() for field in match.group("body").split(";"))
        if len(body) == 1:
            prop = None
            value = body[0]
        elif len(body) == 2:
            prop, value = body
        else:
            raise DataError(f"{source}:{line_number}: malformed @missing declaration")
        if not value or prop == "":
            raise DataError(f"{source}:{line_number}: empty @missing field")
        defaults.append(MissingDefault(start, end, prop, value, line_number))
    return tuple(defaults)


def _canonical_line_break_defaults(
    text: str,
    *,
    source: str,
    expected: tuple[tuple[int, int, str, tuple[str, ...]], ...],
) -> tuple[MissingDefault, ...]:
    """Canonicalize every file-level Line_Break @missing declaration."""
    property_aliases = {
        loose_alias(alias)
        for alias in ("Line_Break", *FORMAL_PROPERTY_ALIASES["Line_Break"])
    }
    declarations = parse_missing_defaults(text, source=source)
    canonical: list[MissingDefault] = []
    remaining = list(expected)
    for declaration in declarations:
        if (
            declaration.property is not None
            and loose_alias(declaration.property) not in property_aliases
        ):
            raise DataError(
                f"{source}:{declaration.line}: unexpected property in Line_Break @missing declaration"
            )
        match_index = next(
            (
                index
                for index, (start, end, _identity, aliases) in enumerate(remaining)
                if declaration.start == start
                and declaration.end == end
                and loose_alias(declaration.value)
                in {loose_alias(alias) for alias in aliases}
            ),
            None,
        )
        if match_index is None:
            raise DataError(
                f"{source}:{declaration.line}: unexpected or duplicate Line_Break @missing declaration"
            )
        start, end, identity, _aliases = remaining.pop(match_index)
        canonical.append(
            MissingDefault(start, end, "Line_Break", identity, declaration.line)
        )
    if remaining:
        missing = [
            f"{start:04X}..{end:04X}; {identity}"
            for start, end, identity, _aliases in remaining
        ]
        raise DataError(f"{source}: missing Line_Break @missing declarations {missing}")
    return tuple(canonical)


def _required_formal_default(
    text: str,
    *,
    source: str,
    property_name: str,
    declared_property: str | None,
    value: str,
) -> MissingDefault:
    property_aliases = {
        loose_alias(alias)
        for alias in (
            property_name,
            *FORMAL_PROPERTY_ALIASES.get(property_name, ()),
            *((declared_property,) if declared_property is not None else ()),
        )
    }
    # UAX #44 permits both contextual and explicitly qualified declarations.
    # Normalize both forms before checking uniqueness so syntax cannot hide a
    # second default for the same logical property.
    declarations = [
        default
        for default in parse_missing_defaults(text, source=source)
        if default.property is None
        or loose_alias(default.property) in property_aliases
    ]
    if len(declarations) != 1:
        lines = [declaration.line for declaration in declarations]
        raise DataError(
            f"{source}: expected exactly one @missing declaration for {property_name}; found lines {lines}"
        )
    match = declarations[0]
    if (
        match.start != 0
        or match.end != MAX_CODE_POINT
        or match.value != value
    ):
        raise DataError(
            f"{source}: expected exactly one formal full-domain default for {property_name}={value}"
        )
    return MissingDefault(match.start, match.end, property_name, match.value, match.line)


def _validate_default_precedence(
    records: Iterable[RangeRecord],
    defaults: Iterable[MissingDefault],
    *,
    source: str,
    properties: Iterable[str],
) -> None:
    records = tuple(records)
    defaults = tuple(defaults)
    properties = tuple(properties)
    for prop in properties:
        prop_defaults = [default for default in defaults if default.property == prop]
        if not prop_defaults:
            raise DataError(f"{source}: {prop} has no canonical default declaration")
        first = prop_defaults[0]
        if first.start != 0 or first.end != MAX_CODE_POINT:
            raise DataError(f"{source}: {prop} defaults do not cover the scalar domain")
        for default in prop_defaults[1:]:
            if default.start < first.start or default.end > first.end:
                raise DataError(f"{source}:{default.line}: ranged default escapes base coverage")
        if len(properties) > 1 and not any(record.property == prop for record in records):
            raise DataError(f"{source}: {prop} has no explicit records")
    if len(properties) == 1 and not records:
        raise DataError(f"{source}: {properties[0]} has no explicit records")


def parse_east_asian_width_defaults(text: str, *, source: str) -> tuple[MissingDefault, ...]:
    formal = _required_formal_default(
        text,
        source=source,
        property_name="East_Asian_Width",
        declared_property=None,
        value="N",
    )
    required_prose = (
        '#  - The unassigned code points in the following blocks default to "W":',
        "#  - All undesignated code points in Planes 2 and 3, whether inside or",
        '#      outside of allocated blocks, default to "W":',
    )
    lines = text.splitlines()
    for declaration in required_prose:
        if lines.count(declaration) != 1:
            raise DataError(f"{source}: missing formal East_Asian_Width default prose {declaration!r}")
    expected = (
        (0x3400, 0x4DBF),
        (0x4E00, 0x9FFF),
        (0xF900, 0xFAFF),
        (0x20000, 0x2FFFD),
        (0x30000, 0x3FFFD),
    )
    ranged: list[MissingDefault] = []
    range_pattern = re.compile(r"U\+([0-9A-F]{4,6})\.\.U\+([0-9A-F]{4,6})$")
    for line_number, raw_line in enumerate(lines, 1):
        match = range_pattern.search(raw_line)
        if match is not None and line_number < formal.line:
            ranged.append(
                MissingDefault(
                    int(match.group(1), 16),
                    int(match.group(2), 16),
                    "East_Asian_Width",
                    "W",
                    line_number,
                )
            )
    if tuple((item.start, item.end) for item in ranged) != expected:
        raise DataError(f"{source}: East_Asian_Width ranged defaults drifted")
    return (formal, *ranged)


def parse_emoji_defaults(text: str, *, source: str) -> tuple[MissingDefault, ...]:
    pattern = re.compile(r"^# All omitted code points have ([A-Za-z_]+)=No$")
    defaults: list[MissingDefault] = []
    for line_number, line in enumerate(text.splitlines(), 1):
        match = pattern.fullmatch(line)
        if match is not None:
            defaults.append(
                MissingDefault(0, MAX_CODE_POINT, match.group(1), "No", line_number)
            )
    if tuple(default.property for default in defaults) != EMOJI_PROPERTIES:
        raise DataError(f"{source}: expected the six ordered Emoji default declarations")
    return tuple(defaults)


def _section_values(text: str, prefix: str) -> tuple[str, ...]:
    return tuple(
        line[len(prefix) :].strip()
        for line in text.splitlines()
        if line.startswith(prefix)
    )


def parse_numeric_ranges(text: str, *, source: str) -> list[RangeRecord]:
    records: list[RangeRecord] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        match = NUMERIC_RANGE_RE.fullmatch(line)
        if match is None:
            raise DataError(f"{source}:{line_number}: malformed numeric data line: {raw_line!r}")
        start = int(match.group("start"), 16)
        end = int(match.group("end") or match.group("start"), 16)
        value = int(match.group("property"))
        if start > end or end > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: invalid numeric range")
        if value > 0xFF:
            raise DataError(f"{source}:{line_number}: numeric value does not fit in U8")
        records.append(RangeRecord(start, end, str(value), line_number))
    if not records:
        raise DataError(f"{source}: no numeric data records")
    ordered = sorted(records, key=lambda item: (item.start, item.end))
    for previous, current in zip(ordered, ordered[1:]):
        if current.start <= previous.end:
            raise DataError(f"{source}:{current.line}: range overlaps line {previous.line}")
    return records


def _resolve_alias(
    records: Iterable[PropertyValueAlias], value: str, *, source: str
) -> str:
    loose = loose_alias(value)
    matches = {
        record.identity
        for record in records
        if any(loose_alias(alias) == loose for alias in (record.identity, record.short, record.long, *record.aliases))
    }
    if len(matches) != 1:
        raise DataError(f"{source}: alias {value!r} did not resolve to exactly one value")
    return next(iter(matches))


def _validate_full_coverage(records: Iterable[RangeRecord], *, source: str) -> None:
    expected = 0
    for record in sorted(records, key=lambda item: (item.start, item.end)):
        if record.start != expected:
            raise DataError(f"{source}:{record.line}: property coverage has a gap at U+{expected:04X}")
        expected = record.end + 1
    if expected != MAX_CODE_POINT + 1:
        raise DataError(f"{source}: property coverage stops at U+{expected - 1:04X}")


def parse_incb(text: str, *, source: str) -> list[RangeRecord]:
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
    if _section_values(text, "# Indic_Conjunct_Break=") != (
        "Linker", "Consonant", "Extend"
    ):
        raise DataError(f"{source}: Indic_Conjunct_Break sections drifted")
    return records


def load_property_data(
    manifest: dict[str, object],
) -> AlgorithmProperties:
    gcb_name = _source_for(
        manifest, "ucd-property-ranges", ("Grapheme_Cluster_Break",)
    )
    gcb_text = verify_source(manifest, gcb_name)
    gcb_source = str(data_path(manifest, gcb_name))
    gcb = parse_ranges(
        gcb_text,
        source=gcb_source,
        allowed_properties=GCB_PROPERTIES,
        default_marker=None,
    )
    gcb_defaults = (
        _required_formal_default(
            gcb_text,
            source=gcb_source,
            property_name="Grapheme_Cluster_Break",
            declared_property=None,
            value="Other",
        ),
    )
    eaw_name = _source_for(manifest, "ucd-property-ranges", ("East_Asian_Width",))
    eaw_text = verify_source(manifest, eaw_name)
    eaw_source = str(data_path(manifest, eaw_name))
    eaw = parse_ranges(
        eaw_text,
        source=eaw_source,
        allowed_properties=EAW_PROPERTIES,
        default_marker=None,
    )
    eaw_defaults = parse_east_asian_width_defaults(eaw_text, source=eaw_source)
    emoji_name = _source_for(
        manifest, "ucd-binary-property-ranges", EMOJI_PROPERTIES
    )
    emoji_text = verify_source(manifest, emoji_name)
    emoji_source = str(data_path(manifest, emoji_name))
    emoji = parse_ranges(
        emoji_text,
        source=emoji_source,
        allowed_properties=EMOJI_PROPERTIES,
        default_marker=None,
        overlaps_by_property=True,
    )
    emoji_defaults = parse_emoji_defaults(emoji_text, source=emoji_source)
    incb_name = _source_for(
        manifest,
        "ucd-derived-core-properties",
        ("Default_Ignorable_Code_Point", "Indic_Conjunct_Break"),
    )
    incb_text = verify_source(manifest, incb_name)
    incb_source = str(data_path(manifest, incb_name))
    incb = parse_incb(
        incb_text,
        source=incb_source,
    )
    incb_defaults = (
        _required_formal_default(
            incb_text,
            source=incb_source,
            property_name="Indic_Conjunct_Break",
            declared_property="InCB",
            value="None",
        ),
    )
    _validate_default_precedence(
        gcb, gcb_defaults, source=gcb_source, properties=("Grapheme_Cluster_Break",)
    )
    _validate_default_precedence(
        eaw, eaw_defaults, source=eaw_source, properties=("East_Asian_Width",)
    )
    _validate_default_precedence(
        emoji, emoji_defaults, source=emoji_source, properties=EMOJI_PROPERTIES
    )
    _validate_default_precedence(
        incb, incb_defaults, source=incb_source, properties=("Indic_Conjunct_Break",)
    )
    if {record.property for record in gcb} != set(GCB_PROPERTIES):
        raise DataError(f"{gcb_source}: Grapheme_Cluster_Break values/sections drifted")
    if {record.property for record in eaw} != set(EAW_PROPERTIES):
        raise DataError(f"{eaw_source}: East_Asian_Width values drifted")
    if {record.property for record in incb} != set(INCB_PROPERTIES):
        raise DataError(f"{incb_source}: Indic_Conjunct_Break values drifted")
    return AlgorithmProperties(
        PropertySource(tuple(gcb), gcb_defaults),
        PropertySource(tuple(eaw), eaw_defaults),
        PropertySource(tuple(emoji), emoji_defaults),
        PropertySource(tuple(incb), incb_defaults),
    )


def load_canonical_properties(manifest: dict[str, object]) -> CanonicalProperties:
    alias_properties = PUBLIC_ALIAS_PROPERTIES
    property_alias_name = _source_for(
        manifest, "ucd-property-aliases", alias_properties
    )
    property_value_alias_name = _source_for(
        manifest, "ucd-property-value-aliases", alias_properties
    )
    property_alias_path = data_path(manifest, property_alias_name)
    property_value_alias_path = data_path(manifest, property_value_alias_name)
    property_alias_text = verify_source(manifest, property_alias_name)
    property_value_alias_text = verify_source(manifest, property_value_alias_name)
    property_aliases = parse_property_aliases(
        property_alias_text, source=str(property_alias_path)
    )
    value_aliases = parse_property_value_aliases(
        property_value_alias_text, source=str(property_value_alias_path)
    )
    for required in ("gc", "ccc"):
        if required not in property_aliases or required not in value_aliases:
            raise DataError(f"property alias sources are missing required property {required!r}")

    general_category_name = _source_for(
        manifest, "ucd-property-ranges", ("General_Category",)
    )
    general_category_path = data_path(manifest, general_category_name)
    general_category_text = verify_source(manifest, general_category_name)
    general_category_values = value_aliases["gc"]
    general_category_aliases = {record.identity for record in general_category_values}
    general_category = parse_ranges(
        general_category_text,
        source=str(general_category_path),
        allowed_properties=general_category_aliases,
        default_marker=None,
    )
    _validate_full_coverage(general_category, source=str(general_category_path))
    general_category_missing = _required_formal_default(
        property_value_alias_text,
        source=str(property_value_alias_path),
        property_name="General_Category",
        declared_property="General_Category",
        value="Unassigned",
    )
    general_category_default = _resolve_alias(
        general_category_values, general_category_missing.value, source=str(property_value_alias_path)
    )
    if general_category_default not in general_category_aliases:
        raise DataError("General_Category default is not a declared stable value")
    observed_categories = {record.property for record in general_category}
    section_categories = {
        _resolve_alias(general_category_values, value, source=str(general_category_path))
        for value in _section_values(general_category_text, "# General_Category=")
    }
    if section_categories != observed_categories or len(section_categories) != 30:
        raise DataError(f"{general_category_path}: General_Category sections drifted")

    combining_class_name = _source_for(
        manifest, "ucd-numeric-property-ranges", ("Canonical_Combining_Class",)
    )
    combining_class_path = data_path(manifest, combining_class_name)
    combining_class_text = verify_source(manifest, combining_class_name)
    combining_class_values = value_aliases["ccc"]
    combining_class = parse_numeric_ranges(
        combining_class_text, source=str(combining_class_path)
    )
    combining_class_missing = _required_formal_default(
        combining_class_text,
        source=str(combining_class_path),
        property_name="Canonical_Combining_Class",
        declared_property=None,
        value="Not_Reordered",
    )
    combining_class_default_identity = _resolve_alias(
        combining_class_values, combining_class_missing.value, source=str(combining_class_path)
    )
    combining_class_default = int(combining_class_default_identity)
    declared_classes = {int(record.identity) for record in combining_class_values}
    observed_classes = {int(record.property) for record in combining_class}
    unknown_classes = observed_classes - declared_classes
    if unknown_classes:
        raise DataError(
            f"Canonical_Combining_Class data use undeclared values {sorted(unknown_classes)}"
        )
    combining_sections = _section_values(
        combining_class_text, "# Canonical_Combining_Class="
    )
    section_classes = {
        int(_resolve_alias(combining_class_values, value, source=str(combining_class_path)))
        for value in combining_sections
    }
    if section_classes != observed_classes or len(section_classes) != len(combining_sections):
        raise DataError(f"{combining_class_path}: Canonical_Combining_Class sections drifted")

    return CanonicalProperties(
        tuple(general_category),
        general_category_default,
        tuple(combining_class),
        combining_class_default,
        property_aliases,
        value_aliases,
    )


def _alias_spellings(records: Iterable[PropertyValueAlias]) -> set[str]:
    return {
        alias
        for record in records
        for alias in (record.identity, record.short, record.long, *record.aliases)
    }


def _resolved_property_source(
    text: str,
    *,
    source: str,
    property_name: str,
    aliases: tuple[PropertyValueAlias, ...],
    expected_base: str,
) -> PropertySource:
    records = parse_ranges(
        text,
        source=source,
        allowed_properties=_alias_spellings(aliases),
        default_marker=None,
    )
    resolved_records = tuple(
        RangeRecord(
            record.start,
            record.end,
            _resolve_alias(aliases, record.property, source=source),
            record.line,
        )
        for record in records
    )
    declarations = parse_missing_defaults(text, source=source)
    if not declarations:
        raise DataError(f"{source}: {property_name} has no formal @missing declaration")
    resolved_defaults = tuple(
        MissingDefault(
            declaration.start,
            declaration.end,
            property_name,
            _resolve_alias(aliases, declaration.value, source=source),
            declaration.line,
        )
        for declaration in declarations
        if declaration.property is None
        or loose_alias(declaration.property)
        in {loose_alias(property_name), loose_alias(property_name.replace("_", ""))}
    )
    expected_identity = _resolve_alias(aliases, expected_base, source=source)
    if (
        not resolved_defaults
        or resolved_defaults[0].start != 0
        or resolved_defaults[0].end != MAX_CODE_POINT
        or resolved_defaults[0].value != expected_identity
    ):
        raise DataError(
            f"{source}: {property_name} must begin with the full-domain default {expected_base}"
        )
    _validate_default_precedence(
        resolved_records,
        resolved_defaults,
        source=source,
        properties=(property_name,),
    )
    return PropertySource(resolved_records, resolved_defaults)


def _is_unicode_scalar(code_point: int) -> bool:
    return 0 <= code_point <= MAX_CODE_POINT and not 0xD800 <= code_point <= 0xDFFF


def _parse_unicode_data_mirrored(text: str, *, source: str) -> tuple[RangeRecord, ...]:
    records: list[RangeRecord] = []
    previous = -1
    pending_first: tuple[int, str, bool, int] | None = None
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        if not raw_line:
            continue
        fields = raw_line.split(";")
        if len(fields) != 15 or HEX_RE.fullmatch(fields[0]) is None:
            raise DataError(f"{source}:{line_number}: malformed UnicodeData record")
        code_point = int(fields[0], 16)
        if code_point <= previous or code_point > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: UnicodeData code points are not strictly ordered")
        previous = code_point
        if fields[9] not in ("Y", "N"):
            raise DataError(f"{source}:{line_number}: invalid Bidi_Mirrored value {fields[9]!r}")
        name = fields[1]
        if name.endswith(", First>"):
            if pending_first is not None:
                raise DataError(f"{source}:{line_number}: nested UnicodeData First range")
            pending_first = (code_point, name[:-8], fields[9] == "Y", line_number)
        elif name.endswith(", Last>"):
            if pending_first is None or pending_first[1] != name[:-7]:
                raise DataError(f"{source}:{line_number}: unmatched UnicodeData Last range")
            start, _, mirrored, first_line = pending_first
            if mirrored != (fields[9] == "Y"):
                raise DataError(f"{source}:{line_number}: UnicodeData range changes Bidi_Mirrored")
            if mirrored:
                records.append(RangeRecord(start, code_point, "Y", first_line))
            pending_first = None
        else:
            if pending_first is not None:
                raise DataError(f"{source}:{line_number}: unterminated UnicodeData range")
            if fields[9] == "Y":
                records.append(RangeRecord(code_point, code_point, "Y", line_number))
    if pending_first is not None:
        raise DataError(f"{source}: unterminated UnicodeData range")
    if not records:
        raise DataError(f"{source}: no Bidi_Mirrored=Yes records")
    return tuple(records)


def _parse_sparse_mapping(text: str, *, source: str) -> tuple[SparseMapping, ...]:
    records: list[SparseMapping] = []
    seen: set[int] = set()
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";"))
        if len(fields) != 2 or any(HEX_RE.fullmatch(field) is None for field in fields):
            raise DataError(f"{source}:{line_number}: malformed scalar mapping")
        source_cp, target_cp = (int(field, 16) for field in fields)
        if (
            source_cp in seen
            or not _is_unicode_scalar(source_cp)
            or not _is_unicode_scalar(target_cp)
        ):
            raise DataError(f"{source}:{line_number}: duplicate or non-scalar mapping endpoint")
        seen.add(source_cp)
        records.append(SparseMapping(source_cp, target_cp, line_number))
    if not records or tuple(record.source for record in records) != tuple(sorted(seen)):
        raise DataError(f"{source}: scalar mappings must be nonempty and strictly ordered")
    return tuple(records)


def _parse_bidi_brackets(text: str, *, source: str) -> tuple[BidiBracket, ...]:
    records: list[BidiBracket] = []
    by_source: dict[int, BidiBracket] = {}
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";"))
        if (
            len(fields) != 3
            or HEX_RE.fullmatch(fields[0]) is None
            or HEX_RE.fullmatch(fields[1]) is None
            or fields[2] not in ("o", "c")
        ):
            raise DataError(f"{source}:{line_number}: malformed bidi bracket record")
        record = BidiBracket(int(fields[0], 16), int(fields[1], 16), fields[2], line_number)
        if (
            record.source in by_source
            or not _is_unicode_scalar(record.source)
            or not _is_unicode_scalar(record.target)
        ):
            raise DataError(f"{source}:{line_number}: duplicate or non-scalar bidi bracket endpoint")
        by_source[record.source] = record
        records.append(record)
    if tuple(record.source for record in records) != tuple(sorted(by_source)):
        raise DataError(f"{source}: bidi brackets are not strictly ordered")
    for record in records:
        partner = by_source.get(record.target)
        expected_kind = "c" if record.kind == "o" else "o"
        if partner is None or partner.target != record.source or partner.kind != expected_kind:
            raise DataError(f"{source}:{record.line}: bidi bracket pair is not reciprocal")
    return tuple(records)


def _parse_arabic_shaping(
    text: str,
    *,
    source: str,
    jt_aliases: tuple[PropertyValueAlias, ...],
    jg_aliases: tuple[PropertyValueAlias, ...],
) -> tuple[tuple[RangeRecord, ...], tuple[RangeRecord, ...]]:
    joining_types: list[RangeRecord] = []
    joining_groups: list[RangeRecord] = []
    previous = -1
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";"))
        if len(fields) != 4 or HEX_RE.fullmatch(fields[0]) is None or any(not field for field in fields):
            raise DataError(f"{source}:{line_number}: malformed ArabicShaping record")
        code_point = int(fields[0], 16)
        if code_point <= previous or code_point > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: ArabicShaping code points are not strictly ordered")
        previous = code_point
        joining_types.append(
            RangeRecord(
                code_point,
                code_point,
                _resolve_alias(jt_aliases, fields[2], source=source),
                line_number,
            )
        )
        joining_groups.append(
            RangeRecord(
                code_point,
                code_point,
                _resolve_alias(jg_aliases, fields[3].replace(" ", "_"), source=source),
                line_number,
            )
        )
    if not joining_types:
        raise DataError(f"{source}: no ArabicShaping records")
    return tuple(joining_types), tuple(joining_groups)


def _parse_binary_projection(
    text: str, *, source: str, property_name: str
) -> tuple[RangeRecord, ...]:
    records: list[RangeRecord] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        body = line.split("#", 1)[0].strip()
        fields = tuple(field.strip() for field in body.split(";"))
        range_fields = fields[0].split("..") if fields else []
        if (
            len(fields) not in (2, 3)
            or len(range_fields) not in (1, 2)
            or any(HEX_RE.fullmatch(field) is None for field in range_fields)
            or any(not field for field in fields[1:])
        ):
            raise DataError(f"{source}:{line_number}: malformed binary-property record")
        if fields[1] != property_name:
            continue
        if len(fields) != 2:
            raise DataError(f"{source}:{line_number}: {property_name} unexpectedly has a value field")
        start = int(range_fields[0], 16)
        end = int(range_fields[-1], 16)
        if start > end or end > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: invalid binary-property range")
        records.append(RangeRecord(start, end, property_name, line_number))
    ordered = sorted(records, key=lambda record: record.start)
    for previous, current in zip(ordered, ordered[1:]):
        if current.start <= previous.end:
            raise DataError(f"{source}:{current.line}: overlapping {property_name} range")
    if not records:
        raise DataError(f"{source}: no {property_name} records")
    return tuple(records)


def _parse_emoji_variation_bases(text: str, *, source: str) -> tuple[int, ...]:
    by_base: dict[int, dict[int, str]] = {}
    previous: tuple[int, int] | None = None
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";") if field.strip())
        code_points = fields[0].split() if fields else []
        if (
            len(fields) != 2
            or len(code_points) != 2
            or any(HEX_RE.fullmatch(field) is None for field in code_points)
            or fields[1] not in ("text style", "emoji style")
        ):
            raise DataError(f"{source}:{line_number}: malformed emoji variation sequence")
        base, selector = (int(field, 16) for field in code_points)
        pair = (base, selector)
        if previous is not None and pair <= previous:
            raise DataError(f"{source}:{line_number}: emoji variation sequences are not ordered")
        previous = pair
        if selector not in (0xFE0E, 0xFE0F) or not _is_unicode_scalar(base):
            raise DataError(f"{source}:{line_number}: non-scalar or invalid emoji variation sequence")
        if selector in by_base.setdefault(base, {}):
            raise DataError(f"{source}:{line_number}: duplicate emoji variation selector")
        by_base[base][selector] = fields[1]
    for base, selectors in by_base.items():
        if selectors != {0xFE0E: "text style", 0xFE0F: "emoji style"}:
            raise DataError(f"{source}: U+{base:04X} does not have both VS15 and VS16")
    return tuple(by_base)


def _encoded_property(
    source: PropertySource,
    identities: tuple[str, ...],
) -> bytearray:
    identity_to_u8 = {identity: index for index, identity in enumerate(identities)}
    if len(identity_to_u8) != len(identities) or len(identities) > 256:
        raise DataError("property identities must be unique and fit in U8")
    encoded = bytearray((MAX_CODE_POINT + 1))
    for default in source.defaults:
        value = identity_to_u8[default.value]
        encoded[default.start : default.end + 1] = bytes((value,)) * (default.end - default.start + 1)
    for record in source.records:
        value = identity_to_u8[record.property]
        encoded[record.start : record.end + 1] = bytes((value,)) * (record.end - record.start + 1)
    return encoded


def load_public_properties(
    manifest: dict[str, object], canonical: CanonicalProperties
) -> PublicProperties:
    value_aliases = canonical.property_value_aliases
    required = ("bc", "bpt", "jt", "jg", "InSC", "InPC", "vo")
    if any(name not in value_aliases for name in required):
        raise DataError(f"property value aliases omit one of {required!r}")

    def load_source(source_format: str, projection: tuple[str, ...]) -> tuple[str, str]:
        name = _source_for(manifest, source_format, projection)
        return verify_source(manifest, name), str(data_path(manifest, name))

    bidi_text, bidi_path = load_source("ucd-property-ranges", ("Bidi_Class",))
    bidi_class = _resolved_property_source(
        bidi_text,
        source=bidi_path,
        property_name="Bidi_Class",
        aliases=value_aliases["bc"],
        expected_base="Left_To_Right",
    )
    bidi_declarations = parse_missing_defaults(bidi_text, source=bidi_path)
    expected_bidi_defaults = tuple(
        (
            start,
            end,
            _resolve_alias(value_aliases["bc"], value, source=bidi_path),
        )
        for start, end, value in BIDI_CLASS_DEFAULTS
    )
    actual_bidi_defaults = tuple(
        (default.start, default.end, default.value)
        for default in bidi_class.defaults
    )
    if (
        len(bidi_declarations) != len(bidi_class.defaults)
        or actual_bidi_defaults != expected_bidi_defaults
    ):
        raise DataError(
            f"{bidi_path}: Bidi_Class @missing declarations drifted from the exact Unicode 17.0.0 cascade"
        )

    unicode_data_text, unicode_data_path = load_source("ucd-unicode-data", ("Bidi_Mirrored",))
    mirrored = _parse_unicode_data_mirrored(unicode_data_text, source=unicode_data_path)
    mirroring_text, mirroring_path = load_source("ucd-bidi-mirroring", ("Bidi_Mirroring_Glyph",))
    mirroring = _parse_sparse_mapping(mirroring_text, source=mirroring_path)
    mirrored_scalars = {
        code_point
        for record in mirrored
        for code_point in range(record.start, record.end + 1)
    }
    if any(record.source not in mirrored_scalars for record in mirroring):
        raise DataError(f"{mirroring_path}: mapping source is not Bidi_Mirrored=Yes")

    brackets_text, brackets_path = load_source(
        "ucd-bidi-brackets", ("Bidi_Paired_Bracket", "Bidi_Paired_Bracket_Type")
    )
    brackets = _parse_bidi_brackets(brackets_text, source=brackets_path)

    joining_text, joining_path = load_source("ucd-property-ranges", ("Joining_Type",))
    joining_type = _resolved_property_source(
        joining_text,
        source=joining_path,
        property_name="Joining_Type",
        aliases=value_aliases["jt"],
        expected_base="Non_Joining",
    )
    shaping_text, shaping_path = load_source(
        "ucd-arabic-shaping", ("Joining_Type", "Joining_Group")
    )
    shaping_types, shaping_groups = _parse_arabic_shaping(
        shaping_text,
        source=shaping_path,
        jt_aliases=value_aliases["jt"],
        jg_aliases=value_aliases["jg"],
    )
    jt_identities = (
        _resolve_alias(value_aliases["jt"], "Non_Joining", source=joining_path),
        *(
            record.identity
            for record in value_aliases["jt"]
            if record.identity != _resolve_alias(value_aliases["jt"], "Non_Joining", source=joining_path)
        ),
    )
    encoded_jt = _encoded_property(joining_type, jt_identities)
    for record in shaping_types:
        if jt_identities[encoded_jt[record.start]] != record.property:
            raise DataError(f"{shaping_path}:{record.line}: Joining_Type disagrees with derived data")

    general_category_by_cp = bytearray(MAX_CODE_POINT + 1)
    gc_symbols = {"Mn", "Me", "Cf"}
    for record in canonical.general_category:
        if record.property in gc_symbols:
            general_category_by_cp[record.start : record.end + 1] = b"\x01" * (
                record.end - record.start + 1
            )
    shaping_by_cp = {record.start: record.property for record in shaping_types}
    transparent = _resolve_alias(value_aliases["jt"], "Transparent", source=shaping_path)
    non_joining = _resolve_alias(value_aliases["jt"], "Non_Joining", source=shaping_path)
    for code_point in range(MAX_CODE_POINT + 1):
        expected = shaping_by_cp.get(
            code_point, transparent if general_category_by_cp[code_point] else non_joining
        )
        if jt_identities[encoded_jt[code_point]] != expected:
            raise DataError(
                f"{joining_path}: U+{code_point:04X} disagrees with ArabicShaping + GC derivation"
            )

    no_group = _resolve_alias(value_aliases["jg"], "No_Joining_Group", source=shaping_path)
    joining_group = tuple(
        record for record in shaping_groups if record.property != no_group
    )

    insc_text, insc_path = load_source("ucd-property-ranges", ("Indic_Syllabic_Category",))
    insc = _resolved_property_source(
        insc_text,
        source=insc_path,
        property_name="Indic_Syllabic_Category",
        aliases=value_aliases["InSC"],
        expected_base="Other",
    )
    inpc_text, inpc_path = load_source("ucd-property-ranges", ("Indic_Positional_Category",))
    inpc = _resolved_property_source(
        inpc_text,
        source=inpc_path,
        property_name="Indic_Positional_Category",
        aliases=value_aliases["InPC"],
        expected_base="Not_Applicable",
    )
    vo_text, vo_path = load_source("ucd-property-ranges", ("Vertical_Orientation",))
    vo = _resolved_property_source(
        vo_text,
        source=vo_path,
        property_name="Vertical_Orientation",
        aliases=value_aliases["vo"],
        expected_base="R",
    )

    derived_text, derived_path = load_source(
        "ucd-derived-core-properties",
        ("Default_Ignorable_Code_Point", "Indic_Conjunct_Break"),
    )
    default_ignorable = _parse_binary_projection(
        derived_text, source=derived_path, property_name="Default_Ignorable_Code_Point"
    )
    prop_text, prop_path = load_source("ucd-binary-property-ranges", ("Variation_Selector",))
    variation_selector = _parse_binary_projection(
        prop_text, source=prop_path, property_name="Variation_Selector"
    )
    variation_text, variation_path = load_source(
        "ucd-emoji-variation-sequences", ("Emoji_Variation_Sequence",)
    )
    variation_bases = _parse_emoji_variation_bases(variation_text, source=variation_path)

    return PublicProperties(
        bidi_class,
        mirrored,
        mirroring,
        brackets,
        joining_type,
        joining_group,
        insc,
        inpc,
        vo,
        default_ignorable,
        variation_selector,
        variation_bases,
    )


def parse_script_extensions(
    text: str,
    *,
    source: str,
    aliases: Iterable[PropertyValueAlias],
) -> list[ScriptExtensionRecord]:
    aliases = tuple(aliases)
    _required_formal_default(
        text,
        source=source,
        property_name="Script_Extensions",
        declared_property=None,
        value="<script>",
    )
    records: list[ScriptExtensionRecord] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = tuple(field.strip() for field in body.split(";"))
        if len(fields) != 2 or not fields[0] or not fields[1]:
            raise DataError(f"{source}:{line_number}: malformed Script_Extensions line")
        range_fields = fields[0].split("..")
        if (
            len(range_fields) not in (1, 2)
            or any(HEX_RE.fullmatch(value) is None for value in range_fields)
        ):
            raise DataError(f"{source}:{line_number}: malformed Script_Extensions range")
        start = int(range_fields[0], 16)
        end = int(range_fields[-1], 16)
        if start > end or end > MAX_CODE_POINT:
            raise DataError(f"{source}:{line_number}: invalid Script_Extensions range")
        raw_members = fields[1].split()
        if not raw_members:
            raise DataError(f"{source}:{line_number}: empty Script_Extensions set")
        members = tuple(
            sorted(_resolve_alias(aliases, member, source=source) for member in raw_members)
        )
        if len(members) != len(set(members)):
            raise DataError(f"{source}:{line_number}: duplicate Script_Extensions member")
        implicit = sorted(set(members) & IMPLICIT_SCRIPTS)
        if implicit:
            raise DataError(
                f"{source}:{line_number}: explicit Script_Extensions set contains implicit values {implicit}"
            )
        records.append(ScriptExtensionRecord(start, end, members, line_number))
    if not records:
        raise DataError(f"{source}: no Script_Extensions records")
    ordered = sorted(records, key=lambda item: (item.start, item.end))
    for previous, current in zip(ordered, ordered[1:]):
        if current.start <= previous.end:
            raise DataError(
                f"{source}:{current.line}: Script_Extensions range overlaps line {previous.line}"
            )
    return records


def load_script_properties(manifest: dict[str, object]) -> ScriptProperties:
    alias_properties = PUBLIC_ALIAS_PROPERTIES
    value_alias_name = _source_for(
        manifest, "ucd-property-value-aliases", alias_properties
    )
    value_alias_path = data_path(manifest, value_alias_name)
    value_aliases = parse_property_value_aliases(
        verify_source(manifest, value_alias_name), source=str(value_alias_path)
    )
    if "sc" not in value_aliases:
        raise DataError("property-value aliases omit the Script property")
    script_aliases = tuple(value_aliases["sc"])
    stable_identities = {record.identity for record in script_aliases}
    if len(stable_identities) > 0x100:
        raise DataError("Script private encoding no longer fits in U8")

    script_name = _source_for(manifest, "ucd-property-ranges", SCRIPT_PROPERTIES)
    script_path = data_path(manifest, script_name)
    script_text = verify_source(manifest, script_name)
    accepted_values = {
        alias
        for record in script_aliases
        for alias in (record.identity, record.short, record.long, *record.aliases)
    }
    parsed_scripts = parse_ranges(
        script_text,
        source=str(script_path),
        allowed_properties=accepted_values,
        default_marker=None,
    )
    scripts = tuple(
        RangeRecord(
            record.start,
            record.end,
            _resolve_alias(script_aliases, record.property, source=str(script_path)),
            record.line,
        )
        for record in parsed_scripts
    )
    formal_default = _required_formal_default(
        script_text,
        source=str(script_path),
        property_name="Script",
        declared_property=None,
        value="Unknown",
    )
    script_default = _resolve_alias(
        script_aliases, formal_default.value, source=str(script_path)
    )
    if script_default != "Zzzz":
        raise DataError("Script default must resolve to the stable Unknown identity Zzzz")

    extension_name = _source_for(
        manifest, "ucd-script-extensions", SCRIPT_EXTENSIONS_PROPERTIES
    )
    extension_path = data_path(manifest, extension_name)
    extensions = tuple(
        parse_script_extensions(
            verify_source(manifest, extension_name),
            source=str(extension_path),
            aliases=script_aliases,
        )
    )
    private_ids = {
        identity: index for index, identity in enumerate(sorted(stable_identities))
    }
    primary = bytearray((private_ids[script_default],)) * (MAX_CODE_POINT + 1)
    for record in scripts:
        value = private_ids[record.property]
        primary[record.start : record.end + 1] = bytes((value,)) * (
            record.end - record.start + 1
        )
    identities = tuple(sorted(stable_identities))
    for record in extensions:
        for code_point in range(record.start, record.end + 1):
            script = identities[primary[code_point]]
            if script not in IMPLICIT_SCRIPTS and script not in record.scripts:
                raise DataError(
                    f"{extension_path}:{record.line}: explicit Script {script} is absent from Script_Extensions"
                )
    explicit_sets = {record.scripts for record in extensions}
    if len(explicit_sets) > 0xFF:
        raise DataError("Script_Extensions override set count no longer fits in U8")
    return ScriptProperties(scripts, script_default, extensions, script_aliases)


def parse_grapheme_tests(
    manifest: dict[str, object], text: str | None = None
) -> list[GraphemeCase]:
    version = release_version(manifest, "unicode")
    source_key = _source_for(manifest, "uax-29-test", ())
    source_name = Path(str(_entry(manifest, source_key)["path"])).name
    if text is None:
        text = verify_source(manifest, source_key)
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
    expected = _entry(manifest, source_key).get("cases")
    if expected != len(cases):
        raise DataError(f"grapheme case-count drift: expected {expected}, got {len(cases)}")
    return cases


def parse_line_break_tests(
    manifest: dict[str, object], text: str | None = None
) -> list[LineBreakCase]:
    source_key = _source_for(manifest, "uax-14-test", ())
    source_name = Path(str(_entry(manifest, source_key)["path"])).name
    if text is None:
        text = verify_source(manifest, source_key)
    cases: list[LineBreakCase] = []
    for line_number, raw_line in enumerate(text.splitlines(), 1):
        body = raw_line.split("#", 1)[0].strip()
        if not body:
            continue
        tokens = body.split()
        if len(tokens) < 3 or len(tokens) % 2 == 0:
            raise DataError(f"{source_name}:{line_number}: malformed test token sequence")
        if tokens[0] != "×" or tokens[-1] != "÷":
            raise DataError(f"{source_name}:{line_number}: tests must prohibit sot and break at eot")
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
                    raise DataError(f"{source_name}:{line_number}: invalid code point {token!r}")
                code_point = int(token, 16)
                if code_point > MAX_CODE_POINT or 0xD800 <= code_point <= 0xDFFF:
                    raise DataError(f"{source_name}:{line_number}: invalid scalar U+{code_point:04X}")
                code_points.append(code_point)
                utf8_offset += len(chr(code_point).encode("utf-8"))
        cases.append(LineBreakCase(line_number, tuple(code_points), tuple(break_offsets)))
    expected = _entry(manifest, source_key).get("cases")
    if expected != len(cases):
        raise DataError(f"line-break case-count drift: expected {expected}, got {len(cases)}")
    return cases


def load_line_break_properties(
    manifest: dict[str, object],
) -> tuple[tuple[RangeRecord, ...], bytes]:
    raw_name = _source_for(manifest, "ucd-property-ranges", ("Line_Break",))
    derived_name = _source_for(
        manifest, "ucd-derived-property-ranges", ("Line_Break",)
    )
    raw_text = verify_source(manifest, raw_name)
    derived_text = verify_source(manifest, derived_name)
    raw_source = str(data_path(manifest, raw_name))
    derived_source = str(data_path(manifest, derived_name))
    raw_defaults = _canonical_line_break_defaults(
        raw_text,
        source=raw_source,
        expected=((0, MAX_CODE_POINT, "XX", ("XX", "Unknown")),),
    )
    derived_default_contract = (
        (0, MAX_CODE_POINT, "XX", ("XX", "Unknown")),
        (0x20A0, 0x20CF, "PR", ("PR", "Prefix_Numeric")),
        (0x3400, 0x4DBF, "ID", ("ID", "Ideographic")),
        (0x4E00, 0x9FFF, "ID", ("ID", "Ideographic")),
        (0xF900, 0xFAFF, "ID", ("ID", "Ideographic")),
        (0x1F000, 0x1F7FF, "ID", ("ID", "Ideographic")),
        (0x1F900, 0x1FAFF, "ID", ("ID", "Ideographic")),
        (0x1FC00, 0x1FFFD, "ID", ("ID", "Ideographic")),
        (0x20000, 0x2FFFD, "ID", ("ID", "Ideographic")),
        (0x30000, 0x3FFFD, "ID", ("ID", "Ideographic")),
    )
    derived_defaults = _canonical_line_break_defaults(
        derived_text,
        source=derived_source,
        expected=derived_default_contract,
    )
    raw = parse_ranges(
        raw_text,
        source=raw_source,
        allowed_properties=LINE_BREAK_PROPERTIES,
        default_marker=None,
    )
    derived = parse_ranges(
        derived_text,
        source=derived_source,
        allowed_properties=LINE_BREAK_PROPERTIES,
        default_marker=None,
    )
    if {record.property for record in raw} != set(LINE_BREAK_PROPERTIES):
        raise DataError(f"{raw_source}: Line_Break values drifted")
    if {record.property for record in derived} != set(LINE_BREAK_PROPERTIES):
        raise DataError(f"{derived_source}: derived Line_Break values drifted")

    private_ids = {value: index for index, value in enumerate(LINE_BREAK_PROPERTIES)}
    default_ranges = tuple(
        (default.start, default.end, default.value)
        for default in derived_defaults
        if default.start != 0 or default.end != MAX_CODE_POINT
    )

    def materialize(records: Iterable[RangeRecord]) -> bytearray:
        values = bytearray((private_ids[raw_defaults[0].value],)) * (MAX_CODE_POINT + 1)
        for start, end, prop in default_ranges:
            values[start : end + 1] = bytes((private_ids[prop],)) * (end - start + 1)
        for record in records:
            values[record.start : record.end + 1] = bytes((private_ids[record.property],)) * (
                record.end - record.start + 1
            )
        return values

    raw_values = materialize(raw)
    derived_values = materialize(derived)
    if raw_values != derived_values:
        mismatch = next(
            code_point
            for code_point, (left, right) in enumerate(zip(raw_values, derived_values))
            if left != right
        )
        raise DataError(
            f"LineBreak.txt and DerivedLineBreak.txt disagree at U+{mismatch:04X}"
        )
    return tuple(raw), bytes(raw_values)


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


def _paged_bytes(values: bytes | bytearray, *, page_bits: int) -> PagedBytes:
    if len(values) != MAX_CODE_POINT + 1:
        raise DataError("paged scalar view must cover exactly U+0000..U+10FFFF")
    page_size = 1 << page_bits
    page_ids: dict[bytes, int] = {}
    pages: list[bytes] = []
    page_index: list[int] = []
    for start in range(0, len(values), page_size):
        page = bytes(values[start : start + page_size])
        page_id = page_ids.get(page)
        if page_id is None:
            page_id = len(pages)
            page_ids[page] = page_id
            pages.append(page)
        page_index.append(page_id)
    if len(pages) <= 0x100:
        index_type = "U8"
    elif len(pages) <= 0x10000:
        index_type = "U16"
    elif len(pages) <= 0x100000000:
        index_type = "U32"
    else:
        raise DataError("page index no longer fits in U32")
    return PagedBytes(tuple(page_index), tuple(pages), page_bits, index_type)


def _selected_paged_bytes(
    values: bytes | bytearray,
    *,
    manifest: dict[str, object],
    generator: str,
) -> PagedBytes:
    artifacts = _require_dict(manifest["artifacts"], "manifest.artifacts")
    artifact_name = _artifact_for_generator(manifest, generator)
    artifact = _require_dict(artifacts[artifact_name], f"manifest.artifacts.{artifact_name}")
    layout = _require_dict(artifact["layout"], f"manifest.artifacts.{artifact_name}.layout")
    raw_candidates = layout["candidate_page_bits"]
    if not isinstance(raw_candidates, list) or not all(
        isinstance(page_bits, int) for page_bits in raw_candidates
    ):
        raise DataError(f"manifest.artifacts.{artifact_name}.layout candidates are invalid")
    candidates = tuple(
        _paged_bytes(values, page_bits=int(page_bits))
        for page_bits in raw_candidates
    )
    selected = min(
        candidates,
        key=lambda candidate: (
            candidate.storage_bytes,
            candidate.page_bits,
            {"U8": 1, "U16": 2, "U32": 4}[candidate.index_type],
        ),
    )
    expected = (
        int(layout["page_bits"]),
        str(layout["index_type"]),
        int(layout["expected_index_entries"]),
        int(layout["expected_distinct_pages"]),
        int(layout["expected_logical_bytes"]),
    )
    actual = (
        selected.page_bits,
        selected.index_type,
        len(selected.page_index),
        len(selected.pages),
        selected.storage_bytes,
    )
    if actual != expected:
        raise DataError(
            f"manifest.artifacts.{artifact_name}.layout drifted: expected {expected}, got {actual}"
        )
    if selected.storage_bytes > int(layout["max_logical_bytes"]):
        raise DataError(f"manifest.artifacts.{artifact_name}.layout exceeds its byte budget")
    return selected


def _paged_u16(values: list[int], *, page_bits: int) -> PagedU16:
    if len(values) != MAX_CODE_POINT + 1 or any(value < 0 or value > 0xFFFF for value in values):
        raise DataError("paged U16 scalar view must cover the code-point domain with U16 values")
    page_size = 1 << page_bits
    page_ids: dict[tuple[int, ...], int] = {}
    pages: list[tuple[int, ...]] = []
    page_index: list[int] = []
    for start in range(0, len(values), page_size):
        page = tuple(values[start : start + page_size])
        page_id = page_ids.get(page)
        if page_id is None:
            page_id = len(pages)
            page_ids[page] = page_id
            pages.append(page)
        page_index.append(page_id)
    index_type = "U8" if len(pages) <= 0x100 else "U16" if len(pages) <= 0x10000 else "U32"
    return PagedU16(tuple(page_index), tuple(pages), page_bits, index_type)


def _selected_paged_u16(
    values: list[int], *, manifest: dict[str, object], generator: str
) -> PagedU16:
    artifacts = _require_dict(manifest["artifacts"], "manifest.artifacts")
    artifact_name = _artifact_for_generator(manifest, generator)
    artifact = _require_dict(artifacts[artifact_name], f"manifest.artifacts.{artifact_name}")
    layout = _require_dict(artifact["layout"], f"manifest.artifacts.{artifact_name}.layout")
    candidates = tuple(
        _paged_u16(values, page_bits=page_bits)
        for page_bits in layout["candidate_page_bits"]
    )
    selected = min(candidates, key=lambda item: (item.storage_bytes, item.page_bits))
    expected = (
        int(layout["page_bits"]),
        str(layout["index_type"]),
        int(layout["expected_index_entries"]),
        int(layout["expected_distinct_pages"]),
        int(layout["expected_logical_bytes"]),
    )
    actual = (
        selected.page_bits,
        selected.index_type,
        len(selected.page_index),
        len(selected.pages),
        selected.storage_bytes,
    )
    if actual != expected:
        raise DataError(
            f"manifest.artifacts.{artifact_name}.layout drifted: expected {expected}, got {actual}"
        )
    return selected


def _ascii_ranges(values: bytes | bytearray, target: int) -> list[RangeRecord]:
    records: list[RangeRecord] = []
    start: int | None = None
    for code_point in range(128):
        if values[code_point] == target and start is None:
            start = code_point
        if start is not None and (values[code_point] != target or code_point == 127):
            end = code_point if values[code_point] == target and code_point == 127 else code_point - 1
            records.append(RangeRecord(start, end, str(target), 0))
            start = None
    return records


def _ascii_value_expression(values: bytes | bytearray, *, default: int) -> str:
    branches = []
    for value in sorted(set(values[:128])):
        if value == default:
            continue
        branches.append(f"if {_condition(_ascii_ranges(values, value))} ({value})")
    return " else ".join(branches) + f" else {default}" if branches else str(default)


def render_grapheme_data(
    manifest: dict[str, object],
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

    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="grapheme-data")
    page_size = 1 << paged.page_bits
    ascii_expression = _ascii_value_expression(encoded, default=0)

    return (
        f"## GENERATED from Unicode {version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        f"## layout: {len(paged.page_index)} {paged.index_type} page ids + {len(paged.pages)} x {page_size} U8 values;"
        f" logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalGraphemeData :: [].{\n"
        "    GCB : [Other, CR, LF, Control, Extend, ZWJ, RI, Prepend, SpacingMark, L, V, T, LV, LVT]\n"
        "    InCB : [None, Consonant, Extend, Linker]\n"
        "    Props : { gcb : GCB, incb : InCB, extended_pictographic : Bool }\n\n"
        "    lookup : U32 -> Props\n"
        "    lookup = |scalar| {\n"
        "        value = if scalar < 128 {\n"
        "            ascii_value(scalar)\n"
        "        } else if scalar > 0x10FFFF {\n"
        "            0\n"
        "        } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        "            pages.get(offset) ?? 0\n"
        "        }\n\n"
        "        {\n"
        "            gcb: gcb_from_u8(value.bitwise_and(0x0F)),\n"
        "            incb: incb_from_u8(value.shr_wrap(4).bitwise_and(0x03)),\n"
        "            extended_pictographic: value.bitwise_and(0x40) != 0,\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "ascii_value : U32 -> U8\n"
        f"ascii_value = |u32| {ascii_expression}\n\n"
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
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_line_break_data(
    manifest: dict[str, object],
    version: str,
    raw_values: bytes,
    canonical: CanonicalProperties,
    eaw_source: PropertySource,
    emoji_records: list[RangeRecord],
) -> str:
    raw_ids = {value: index for index, value in enumerate(LINE_BREAK_PROPERTIES)}
    resolved_properties = tuple(
        value
        for value in LINE_BREAK_PROPERTIES
        if value not in ("AI", "CJ", "SA", "SG", "XX")
    )
    class_ids = {value: index for index, value in enumerate(resolved_properties)}

    gc_flags = bytearray(MAX_CODE_POINT + 1)
    for record in canonical.general_category:
        flag = {
            "Mn": 0x01,
            "Mc": 0x02,
            "Pi": 0x04,
            "Pf": 0x08,
            "Cn": 0x10,
        }.get(record.property, 0)
        if flag:
            gc_flags[record.start : record.end + 1] = bytes((flag,)) * (
                record.end - record.start + 1
            )

    eaw_values = bytearray(MAX_CODE_POINT + 1)
    for default in eaw_source.defaults:
        value = 1 if default.value in ("F", "W", "H") else 0
        eaw_values[default.start : default.end + 1] = bytes((value,)) * (
            default.end - default.start + 1
        )
    for record in eaw_source.records:
        value = 1 if record.property in ("F", "W", "H") else 0
        eaw_values[record.start : record.end + 1] = bytes((value,)) * (
            record.end - record.start + 1
        )

    extended_pictographic = bytearray(MAX_CODE_POINT + 1)
    for record in _ranges_for(emoji_records, "Extended_Pictographic"):
        extended_pictographic[record.start : record.end + 1] = b"\x01" * (
            record.end - record.start + 1
        )

    rows: list[tuple[int, int]] = []
    row_ids: dict[tuple[int, int], int] = {}
    encoded = bytearray(MAX_CODE_POINT + 1)
    for code_point, raw_id in enumerate(raw_values):
        raw = LINE_BREAK_PROPERTIES[raw_id]
        if raw in ("AI", "SG", "XX"):
            resolved = "AL"
        elif raw == "CJ":
            resolved = "NS"
        elif raw == "SA":
            resolved = "CM" if gc_flags[code_point] & 0x03 else "AL"
        else:
            resolved = raw
        flags = 0
        if resolved == "QU" and gc_flags[code_point] & 0x04:
            flags |= 0x01
        if resolved == "QU" and gc_flags[code_point] & 0x08:
            flags |= 0x02
        if eaw_values[code_point]:
            flags |= 0x04
        if gc_flags[code_point] & 0x10 and extended_pictographic[code_point]:
            flags |= 0x08
        row = (class_ids[resolved], flags)
        row_id = row_ids.get(row)
        if row_id is None:
            row_id = len(rows)
            if row_id > 0xFF:
                raise DataError("line-break row pool no longer fits in U8")
            rows.append(row)
            row_ids[row] = row_id
        encoded[code_point] = row_id

    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="line-break-data")
    artifacts = _require_dict(manifest["artifacts"], "manifest.artifacts")
    artifact_name = _artifact_for_generator(manifest, "line-break-data")
    artifact = _require_dict(artifacts[artifact_name], f"manifest.artifacts.{artifact_name}")
    layout = _require_dict(artifact["layout"], f"manifest.artifacts.{artifact_name}.layout")
    total_logical_bytes = paged.storage_bytes + len(rows) * 2
    if total_logical_bytes > int(layout["max_logical_bytes"]):
        raise DataError(
            f"manifest.artifacts.{artifact_name}.layout exceeds its total row-view byte budget"
        )
    page_size = 1 << paged.page_bits
    ascii_expression = _ascii_value_expression(encoded, default=encoded[0])
    matches = "\n".join(
        f"        {class_id} => {line_class}" for line_class, class_id in class_ids.items()
    )
    row_classes = tuple(row[0] for row in rows)
    row_flags = tuple(row[1] for row in rows)
    return (
        f"## GENERATED from Unicode {version} Line_Break through the canonical GC/EAW/Emoji graph. "
        "Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## LB1 is resolved in this narrow view. Row ids and flag bits are private storage identities. ##\n"
        f"## layout: {len(paged.page_index)} {paged.index_type} page ids + {len(paged.pages)} x {page_size} U8 row ids "
        f"+ {len(rows)} x 2 U8 row fields; logical payload {total_logical_bytes} bytes. ##\n\n"
        "InternalLineBreakData :: [].{\n"
        f"    Class : [{', '.join(resolved_properties)}]\n"
        "    Props : { class : Class, initial_quote : Bool, final_quote : Bool, east_asian : Bool, unassigned_extended_pictographic : Bool }\n\n"
        "    lookup : U32 -> Props\n"
        "    lookup = |scalar| {\n"
        "        row_id = if scalar < 128 {\n"
        "            ascii_row(scalar)\n"
        "        } else if scalar > 0x10FFFF {\n"
        f"            {encoded[0]}\n"
        "        } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        f"            pages.get(offset) ?? {encoded[0]}\n"
        "        }\n"
        f"        class_id = row_classes.get(row_id.to_u64()) ?? {class_ids['AL']}\n"
        "        flags = row_flags.get(row_id.to_u64()) ?? 0\n"
        "        {\n"
        "            class: class_from_u8(class_id),\n"
        "            initial_quote: flags.bitwise_and(0x01) != 0,\n"
        "            final_quote: flags.bitwise_and(0x02) != 0,\n"
        "            east_asian: flags.bitwise_and(0x04) != 0,\n"
        "            unassigned_extended_pictographic: flags.bitwise_and(0x08) != 0,\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "ascii_row : U32 -> U8\n"
        f"ascii_row = |u32| {ascii_expression}\n\n"
        "class_from_u8 : U8 -> InternalLineBreakData.Class\n"
        "class_from_u8 = |value| {\n"
        "    match value {\n"
        f"{matches}\n"
        "        _ => AL\n"
        "    }\n"
        "}\n\n"
        "row_classes : List(U8)\n"
        f"row_classes = {_roc_list(row_classes)}\n\n"
        "row_flags : List(U8)\n"
        f"row_flags = {_roc_list(row_flags)}\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_general_category(
    manifest: dict[str, object],
    version: str,
    records: Iterable[RangeRecord],
    default: str,
) -> str:
    records = tuple(records)
    categories = tuple(sorted({record.property for record in records}))
    if default not in categories:
        raise DataError("General_Category default is absent from the canonical data")
    private_ids = {category: index for index, category in enumerate(categories)}
    if len(private_ids) > 0x100:
        raise DataError("General_Category private encoding no longer fits in U8")
    encoded = bytearray((private_ids[default],)) * (MAX_CODE_POINT + 1)
    for record in records:
        value = private_ids[record.property]
        encoded[record.start : record.end + 1] = bytes((value,)) * (record.end - record.start + 1)
    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="general-category")
    page_size = 1 << paged.page_bits
    ascii_branches = []
    for category in categories:
        value = private_ids[category]
        ranges = _ascii_ranges(encoded, value)
        if ranges:
            ascii_branches.append(f"if {_condition(ranges)} ({category})")
    ascii_expression = " else ".join(ascii_branches) + f" else {default}"
    matches = "\n".join(
        f"        {private_ids[category]} => {category}" for category in categories
    )
    return (
        f"## GENERATED from Unicode {version} General_Category and aliases. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## Named tags are stable Unicode aliases; byte values below are private storage IDs. ##\n"
        f"## default: {default}; layout: {len(paged.page_index)} {paged.index_type} page ids + "
        f"{len(paged.pages)} x {page_size} U8 values; logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalGeneralCategory :: [].{\n"
        f"    GeneralCategory : [{', '.join(categories)}]\n\n"
        "    lookup : U32 -> GeneralCategory\n"
        "    lookup = |scalar| {\n"
        f"        if scalar < 128 {{\n            ascii_category(scalar)\n        }} else if scalar > 0x10FFFF {{\n            {default}\n        }} else {{\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        f"            category_from_u8(pages.get(offset) ?? {private_ids[default]})\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "ascii_category : U32 -> InternalGeneralCategory.GeneralCategory\n"
        f"ascii_category = |u32| {ascii_expression}\n\n"
        "category_from_u8 : U8 -> InternalGeneralCategory.GeneralCategory\n"
        "category_from_u8 = |value| {\n"
        "    match value {\n"
        f"{matches}\n"
        f"        _ => {default}\n"
        "    }\n"
        "}\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_canonical_combining_class(
    manifest: dict[str, object],
    version: str,
    records: Iterable[RangeRecord],
    default: int,
) -> str:
    encoded = bytearray((default,)) * (MAX_CODE_POINT + 1)
    for record in records:
        value = int(record.property)
        encoded[record.start : record.end + 1] = bytes((value,)) * (record.end - record.start + 1)
    paged = _selected_paged_bytes(
        encoded, manifest=manifest, generator="canonical-combining-class"
    )
    page_size = 1 << paged.page_bits
    return (
        f"## GENERATED from Unicode {version} Canonical_Combining_Class and aliases. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## Returned U8 values are the stable, exact Unicode combining-class numbers. ##\n"
        f"## default: {default}; layout: {len(paged.page_index)} {paged.index_type} page ids + "
        f"{len(paged.pages)} x {page_size} U8 values; logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalCanonicalCombiningClass :: [].{\n"
        "    lookup : U32 -> U8\n"
        "    lookup = |scalar| {\n"
        f"        if scalar < 128 {{\n            {default}\n        }} else if scalar > 0x10FFFF {{\n            {default}\n        }} else {{\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        f"            pages.get(offset) ?? {default}\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def _script_aliases_by_identity(
    properties: ScriptProperties,
) -> tuple[PropertyValueAlias, ...]:
    # Public ScriptSet traversal/comparison is defined in canonical short-alias
    # lexicographic order. PropertyValueAliases currently makes the stable
    # identity equal to that short alias, but assert the relationship here so a
    # future parser/data change cannot silently make public order depend on an
    # incidental private table layout.
    ordered = tuple(sorted(properties.aliases, key=lambda record: record.short))
    if any(record.identity != record.short for record in ordered):
        raise DataError(
            "Script stable identities must equal canonical short aliases for public ordering"
        )
    if tuple(record.short for record in ordered) != tuple(
        sorted(record.short for record in ordered)
    ):
        raise DataError("Script identities are not in canonical short-alias order")
    return ordered


def render_script_data(
    manifest: dict[str, object], version: str, properties: ScriptProperties
) -> str:
    aliases = _script_aliases_by_identity(properties)
    identities = tuple(record.identity for record in aliases)
    private_ids = {identity: index for index, identity in enumerate(identities)}
    encoded = bytearray((private_ids[properties.script_default],)) * (
        MAX_CODE_POINT + 1
    )
    for record in properties.scripts:
        value = private_ids[record.property]
        encoded[record.start : record.end + 1] = bytes((value,)) * (
            record.end - record.start + 1
        )
    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="script-data")
    page_size = 1 << paged.page_bits
    ascii_branches = []
    for identity in identities:
        ranges = _ascii_ranges(encoded, private_ids[identity])
        if ranges:
            ascii_branches.append(
                f"if {_condition(ranges)} {private_ids[identity]}"
            )
    ascii_private_expression = " else ".join(ascii_branches) + f" else {private_ids[properties.script_default]}"
    from_matches = "\n".join(
        f"        {private_ids[identity]} => {identity}" for identity in identities
    )
    id_matches = "\n".join(
        f"        {identity} => {private_ids[identity]}" for identity in identities
    )
    short_matches = "\n".join(
        f"        {record.identity} => {_roc_string(record.short)}" for record in aliases
    )
    long_matches = "\n".join(
        f"        {record.identity} => {_roc_string(record.long)}" for record in aliases
    )
    alias_count_matches = []
    alias_at_matches = []
    from_alias_buckets: dict[int, list[tuple[str, str]]] = {}
    for record in aliases:
        names = _unique_aliases((record.short, record.long, *record.aliases))
        alias_count_matches.append(f"        {record.identity} => {len(names)}")
        for index, alias in enumerate(names):
            alias_at_matches.append(
                f"        ({record.identity}, {index}) => Some({_roc_string(alias)})"
            )
            from_alias_buckets.setdefault(loose_alias_hash(alias), []).append(
                (alias, record.identity)
            )
    from_alias_matches = []
    for hash_value, bucket in sorted(from_alias_buckets.items()):
        conditions = " else ".join(
            f"if loose_eq(value, {_roc_string(alias)}) Some({identity})"
            for alias, identity in bucket
        )
        from_alias_matches.append(
            f"            {hash_value} => {conditions} else None"
        )
    return (
        f"## GENERATED from Unicode {version} Scripts.txt and PropertyValueAliases.txt under UAX #24 revision 39. ##\n"
        "## Run `python3 scripts/unicode_data.py generate`. Named tags/aliases are stable; U8 values are private. ##\n"
        "## Private IDs are generator-asserted canonical short-alias order solely to implement the public order contract. ##\n"
        f"## default: {properties.script_default}; {len(identities)} identities; layout: "
        f"{len(paged.page_index)} {paged.index_type} page ids + {len(paged.pages)} x {page_size} U8 values; "
        f"logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalScriptData :: [].{\n"
        f"    Script : [{', '.join(identities)}]\n\n"
        "    lookup : U32 -> Script\n"
        "    lookup = |scalar| from_private_id(lookup_private(scalar))\n\n"
        "    lookup_private : U32 -> U8\n"
        "    lookup_private = |scalar| {\n"
        f"        if scalar < 128 {{\n            ascii_private_id(scalar)\n        }} else if scalar > 0x10FFFF {{\n            {private_ids[properties.script_default]}\n        }} else {{\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        f"            pages.get(offset) ?? {private_ids[properties.script_default]}\n"
        "        }\n"
        "    }\n\n"
        f"    common_private_id : U8\n    common_private_id = {private_ids['Zyyy']}\n\n"
        f"    inherited_private_id : U8\n    inherited_private_id = {private_ids['Zinh']}\n\n"
        f"    unknown_private_id : U8\n    unknown_private_id = {private_ids['Zzzz']}\n\n"
        "    private_id : Script -> U8\n"
        "    private_id = |script| {\n        match script {\n"
        f"{id_matches}\n        }}\n    }}\n\n"
        "    from_private_id : U8 -> Script\n"
        "    from_private_id = |value| {\n        match value {\n"
        f"{from_matches}\n        _ => {properties.script_default}\n        }}\n    }}\n\n"
        "    short_alias : Script -> Str\n"
        "    short_alias = |script| {\n        match script {\n"
        f"{short_matches}\n        }}\n    }}\n\n"
        "    long_alias : Script -> Str\n"
        "    long_alias = |script| {\n        match script {\n"
        f"{long_matches}\n        }}\n    }}\n\n"
        "    alias_count : Script -> U8\n"
        "    alias_count = |script| {\n        match script {\n"
        + "\n".join(alias_count_matches)
        + "\n        }\n    }\n\n"
        "    alias_at : Script, U8 -> [Some(Str), None]\n"
        "    alias_at = |script, index| {\n        match (script, index) {\n"
        + "\n".join(alias_at_matches)
        + "\n        _ => None\n        }\n    }\n\n"
        "    from_alias : Str -> [Some(Script), None]\n"
        "    from_alias = |value| {\n"
        "        match loose_hash(value) {\n"
        + "\n".join(from_alias_matches)
        + "\n            _ => None\n        }\n    }\n"
        "}\n\n"
        "ascii_private_id : U32 -> U8\n"
        f"ascii_private_id = |u32| {ascii_private_expression}\n\n"
        "loose_hash : Str -> U32\n"
        "loose_hash = |value| {\n"
        "    var hash = 2166136261.U32\n"
        "    for byte in value.iter_utf8() {\n"
        "        if byte != 0x20 and byte != 0x2D and byte != 0x5F {\n"
        "            hash = hash.bitwise_xor(ascii_lower(byte).to_u32()).times_wrap(16777619)\n"
        "        }\n"
        "    }\n"
        "    hash\n"
        "}\n\n"
        "loose_eq : Str, Str -> Bool\n"
        "loose_eq = |left, right| {\n"
        "    var left_bytes = left.iter_utf8()\n"
        "    var right_bytes = right.iter_utf8()\n"
        "    while Bool.True {\n"
        "        left_next = next_loose(left_bytes)\n"
        "        right_next = next_loose(right_bytes)\n"
        "        match (left_next, right_next) {\n"
        "            (End, End) => return Bool.True\n"
        "            (Byte(_), End) => return Bool.False\n"
        "            (End, Byte(_)) => return Bool.False\n"
        "            (Byte(left_item), Byte(right_item)) => {\n"
        "                if ascii_lower(left_item.value) != ascii_lower(right_item.value) {\n"
        "                    return Bool.False\n"
        "                }\n"
        "                left_bytes = left_item.rest\n"
        "                right_bytes = right_item.rest\n"
        "            }\n"
        "        }\n"
        "    }\n"
        "    Bool.False\n"
        "}\n\n"
        "next_loose = |initial| {\n"
        "    var iterator = initial\n"
        "    while Bool.True {\n"
        "        match Iter.next(iterator) {\n"
        "            Done => return End\n"
        "            Skip({ rest }) => { iterator = rest }\n"
        "            One({ item, rest }) => {\n"
        "                if item == 0x20 or item == 0x2D or item == 0x5F {\n"
        "                    iterator = rest\n"
        "                } else {\n"
        "                    return Byte({ value: item, rest })\n"
        "                }\n"
        "            }\n"
        "        }\n"
        "    }\n"
        "    End\n"
        "}\n\n"
        "ascii_lower = |byte| if 0x41 <= byte and byte <= 0x5A { byte + 0x20 } else { byte }\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_script_extensions_data(
    manifest: dict[str, object], version: str, properties: ScriptProperties
) -> str:
    identities = tuple(record.identity for record in _script_aliases_by_identity(properties))
    private_ids = {identity: index for index, identity in enumerate(identities)}
    sets = tuple(
        sorted(
            {
                tuple(sorted(private_ids[identity] for identity in record.scripts))
                for record in properties.extensions
            }
        )
    )
    set_ids = {members: index + 1 for index, members in enumerate(sets)}
    encoded = bytearray(MAX_CODE_POINT + 1)
    for record in properties.extensions:
        members = tuple(sorted(private_ids[identity] for identity in record.scripts))
        value = set_ids[members]
        encoded[record.start : record.end + 1] = bytes((value,)) * (
            record.end - record.start + 1
        )
    paged = _selected_paged_bytes(
        encoded, manifest=manifest, generator="script-extensions-data"
    )
    page_size = 1 << paged.page_bits
    words0 = []
    words1 = []
    words2 = []
    lengths = []
    for members in sets:
        words = [0, 0, 0]
        for member in members:
            words[member // 64] |= 1 << (member % 64)
        words0.append(words[0])
        words1.append(words[1])
        words2.append(words[2])
        lengths.append(len(members))
    return (
        f"## GENERATED from Unicode {version} ScriptExtensions.txt and PropertyValueAliases.txt under UAX #24 revision 39. ##\n"
        "## Zero means the normative implicit singleton Script(cp); nonzero U8 values name interned sets. ##\n"
        f"## {len(sets)} explicit sets, {sum(lengths)} pooled members, max {max(lengths)} members; layout: "
        f"{len(paged.page_index)} {paged.index_type} page ids + {len(paged.pages)} x {page_size} U8 values; "
        f"logical lookup payload {paged.storage_bytes} bytes; pool payload {len(sets) * 25} bytes. ##\n\n"
        "InternalScriptExtensionsData :: [].{\n"
        "    SetBits : { word0 : U64, word1 : U64, word2 : U64, length : U8 }\n\n"
        "    lookup_override : U32 -> U8\n"
        "    lookup_override = |scalar| {\n"
        "        if scalar < 128 {\n            0\n        } else if scalar > 0x10FFFF {\n            0\n        } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        "            pages.get(offset) ?? 0\n"
        "        }\n"
        "    }\n\n"
        "    set_bits : U8 -> SetBits\n"
        "    set_bits = |override_id| {\n"
        "        if override_id == 0 {\n"
        "            { word0: 0, word1: 0, word2: 0, length: 0 }\n"
        "        } else {\n"
        "            index = (override_id - 1).to_u64()\n"
        "            {\n"
        "                word0: set_word0.get(index) ?? 0,\n"
        "                word1: set_word1.get(index) ?? 0,\n"
        "                word2: set_word2.get(index) ?? 0,\n"
        "                length: set_lengths.get(index) ?? 0,\n"
        "            }\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n\n"
        "set_word0 : List(U64)\n"
        f"set_word0 = {_roc_list(words0, per_line=4)}\n\n"
        "set_word1 : List(U64)\n"
        f"set_word1 = {_roc_list(words1, per_line=4)}\n\n"
        "set_word2 : List(U64)\n"
        f"set_word2 = {_roc_list(words2, per_line=4)}\n\n"
        "set_lengths : List(U8)\n"
        f"set_lengths = {_roc_list(lengths)}\n"
    )


def render_script_api(version: str, properties: ScriptProperties) -> str:
    identities = tuple(record.identity for record in _script_aliases_by_identity(properties))
    return (
        f"## GENERATED public Unicode {version} Script/Script_Extensions API from PropertyValueAliases.txt under UAX #24 revision 39. ##\n"
        "## Run `python3 scripts/unicode_data.py generate`; representation IDs and bit ordering are private. ##\n"
        "## ScriptSet traversal and comparison use canonical short-alias lexicographic order. ##\n\n"
        "import InternalScriptData\n"
        "import InternalScriptExtensionsData\n"
        "import Scalar\n\n"
        "## Normative Unicode Script and Script_Extensions scalar properties.\n"
        "##\n"
        "## Script is not a block, language, direction, font, or security classification.\n"
        "## Common, Inherited, and Unknown are real property values. Script_Extensions\n"
        "## is always nonempty: absent override data means the singleton Script(cp).\n"
        "Script :: [].{\n"
        f"    Value := [{', '.join(identities)}].{{ is_eq : _ }}\n\n"
        "    ScriptSet := { word0 : U64, word1 : U64, word2 : U64, length : U8 }\n\n"
        "    of_scalar : Scalar -> Value\n"
        "    of_scalar = |scalar| InternalScriptData.lookup(Scalar.to_u32(scalar))\n\n"
        "    extensions_of_scalar : Scalar -> ScriptSet\n"
        "    extensions_of_scalar = |scalar| {\n"
        "        code_point = Scalar.to_u32(scalar)\n"
        "        override_id = InternalScriptExtensionsData.lookup_override(code_point)\n"
        "        if override_id == 0 { singleton(InternalScriptData.lookup(code_point)) } else {\n"
        "            bits = InternalScriptExtensionsData.set_bits(override_id)\n"
        "            { word0: bits.word0, word1: bits.word1, word2: bits.word2, length: bits.length }\n"
        "        }\n"
        "    }\n\n"
        "    from_alias : Str -> Try(Value, [UnrecognizedScriptAlias])\n"
        "    from_alias = |alias| match InternalScriptData.from_alias(alias) {\n"
        "        Some(script) => Ok(script)\n"
        "        None => Err(UnrecognizedScriptAlias)\n"
        "    }\n\n"
        "    short_alias : Value -> Str\n"
        "    short_alias = |script| InternalScriptData.short_alias(script)\n\n"
        "    long_alias : Value -> Str\n"
        "    long_alias = |script| InternalScriptData.long_alias(script)\n\n"
        "    alias_count : Value -> U8\n"
        "    alias_count = |script| InternalScriptData.alias_count(script)\n\n"
        "    alias_at : Value, U8 -> [Some(Str), None]\n"
        "    alias_at = |script, index| InternalScriptData.alias_at(script, index)\n\n"
        "    is_common : Value -> Bool\n"
        "    is_common = |script| script == Zyyy\n\n"
        "    is_inherited : Value -> Bool\n"
        "    is_inherited = |script| script == Zinh\n\n"
        "    is_unknown : Value -> Bool\n"
        "    is_unknown = |script| script == Zzzz\n\n"
        "    is_explicit : Value -> Bool\n"
        "    is_explicit = |script| script != Zyyy and script != Zinh and script != Zzzz\n\n"
        "    singleton : Value -> ScriptSet\n"
        "    singleton = |script| {\n"
        "        private_id = InternalScriptData.private_id(script)\n"
        "        word = private_id / 64\n"
        "        bit = 1.U64.shl_wrap(private_id % 64)\n"
        "        match word {\n"
        "            0 => { word0: bit, word1: 0, word2: 0, length: 1 }\n"
        "            1 => { word0: 0, word1: bit, word2: 0, length: 1 }\n"
        "            _ => { word0: 0, word1: 0, word2: bit, length: 1 }\n"
        "        }\n"
        "    }\n\n"
        "    contains : ScriptSet, Value -> Bool\n"
        "    contains = |set, script| {\n"
        "        private_id = InternalScriptData.private_id(script)\n"
        "        contains_private(set, private_id)\n"
        "    }\n\n"
        "    len : ScriptSet -> U64\n"
        "    len = |set| set.length.to_u64()\n\n"
        "    intersection : ScriptSet, ScriptSet -> [Some(ScriptSet), None]\n"
        "    intersection = |left, right| {\n"
        "        word0 = left.word0.bitwise_and(right.word0)\n"
        "        word1 = left.word1.bitwise_and(right.word1)\n"
        "        word2 = left.word2.bitwise_and(right.word2)\n"
        "        length = U64.count_one_bits(word0) + U64.count_one_bits(word1) + U64.count_one_bits(word2)\n"
        "        if length == 0 { None } else {\n"
        "            Some({ word0, word1, word2, length })\n"
        "        }\n"
        "    }\n\n"
        "    explicit_members : ScriptSet -> [Some(ScriptSet), None]\n"
        "    explicit_members = |set| {\n"
        "        without_common = remove(set, Zyyy)\n"
        "        without_inherited = remove(without_common, Zinh)\n"
        "        without_unknown = remove(without_inherited, Zzzz)\n"
        "        if without_unknown.length == 0 { None } else { Some(without_unknown) }\n"
        "    }\n\n"
        "    is_eq_set : ScriptSet, ScriptSet -> Bool\n"
        "    is_eq_set = |left, right| left.word0 == right.word0 and left.word1 == right.word1 and left.word2 == right.word2\n\n"
        "    ## Lexicographic comparison in stable canonical short-alias order.\n"
        "    compare : ScriptSet, ScriptSet -> [Before, Equal, After]\n"
        "    compare = |left, right| {\n"
        "        var private_id = 0.U8\n"
        "        var left_seen = 0.U8\n"
        "        var right_seen = 0.U8\n"
        f"        while private_id < {len(identities)} {{\n"
        "            left_has = contains_private(left, private_id)\n"
        "            right_has = contains_private(right, private_id)\n"
        "            if left_has and right_has {\n"
        "                left_seen = left_seen + 1\n"
        "                right_seen = right_seen + 1\n"
        "            } else if left_has {\n"
        "                return if right_seen == right.length { After } else { Before }\n"
        "            } else if right_has {\n"
        "                return if left_seen == left.length { Before } else { After }\n"
        "            }\n"
        "            private_id = private_id + 1\n"
        "        }\n"
        "        Equal\n"
        "    }\n\n"
        "    ## Member by stable canonical short-alias order.\n"
        "    at : ScriptSet, U8 -> [Some(Value), None]\n"
        "    at = |set, wanted| {\n"
        "        if wanted >= set.length { return None }\n"
        "        var private_id = 0.U8\n"
        "        var seen = 0.U8\n"
        f"        while private_id < {len(identities)} {{\n"
        "            if contains_private(set, private_id) {\n"
        "                if seen == wanted { return Some(InternalScriptData.from_private_id(private_id)) }\n"
        "                seen = seen + 1\n"
        "            }\n"
        "            private_id = private_id + 1\n"
        "        }\n"
        "        None\n"
        "    }\n\n"
        "    ## Visit members in stable canonical short-alias order without allocating.\n"
        "    walk : ScriptSet, state, (state, Value -> state) -> state\n"
        "    walk = |set, initial, visit| {\n"
        "        var state = initial\n"
        "        var private_id = 0.U8\n"
        f"        while private_id < {len(identities)} {{\n"
        "            if contains_private(set, private_id) {\n"
        "                state = visit(state, InternalScriptData.from_private_id(private_id))\n"
        "            }\n"
        "            private_id = private_id + 1\n"
        "        }\n"
        "        state\n"
        "    }\n\n"
        "    ## Materialize members in stable canonical short-alias order.\n"
        "    to_list : ScriptSet -> List(Value)\n"
        "    to_list = |set| walk(set, [], |scripts, script| scripts.append(script))\n"
        "}\n\n"
        "remove = |set, script| {\n"
        "    private_id = InternalScriptData.private_id(script)\n"
        "    bit = 1.U64.shl_wrap(private_id % 64)\n"
        "    mask = bit.bitwise_not()\n"
        "    if !contains_private(set, private_id) { set } else {\n"
        "        match private_id / 64 {\n"
        "            0 => { word0: set.word0.bitwise_and(mask), word1: set.word1, word2: set.word2, length: set.length - 1 }\n"
        "            1 => { word0: set.word0, word1: set.word1.bitwise_and(mask), word2: set.word2, length: set.length - 1 }\n"
        "            _ => { word0: set.word0, word1: set.word1, word2: set.word2.bitwise_and(mask), length: set.length - 1 }\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "contains_private = |set, private_id| {\n"
        "    bit = 1.U64.shl_wrap(private_id % 64)\n"
        "    word = match private_id / 64 {\n"
        "        0 => set.word0\n"
        "        1 => set.word1\n"
        "        _ => set.word2\n"
        "    }\n"
        "    word.bitwise_and(bit) != 0\n"
        "}\n"
    )


def render_emoji_data(
    manifest: dict[str, object],
    version: str,
    emoji_version: str,
    records: Iterable[RangeRecord],
) -> str:
    bits = {
        "Emoji": 0x01,
        "Emoji_Presentation": 0x02,
        "Emoji_Modifier": 0x04,
        "Emoji_Modifier_Base": 0x08,
        "Emoji_Component": 0x10,
        "Extended_Pictographic": 0x20,
    }
    encoded = bytearray(MAX_CODE_POINT + 1)
    for record in records:
        bit = bits[record.property]
        for code_point in range(record.start, record.end + 1):
            encoded[code_point] |= bit
    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="emoji-properties")
    page_size = 1 << paged.page_bits
    ascii_expression = _ascii_value_expression(encoded, default=0)
    return (
        f"## GENERATED from Unicode {version} / Emoji {emoji_version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## Each binary Emoji property remains independently observable. ##\n"
        f"## default: all False; layout: {len(paged.page_index)} {paged.index_type} page ids + "
        f"{len(paged.pages)} x {page_size} U8 bitsets; logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalEmojiData :: [].{\n"
        "    Properties : {\n"
        "        emoji : Bool,\n"
        "        emoji_presentation : Bool,\n"
        "        emoji_modifier : Bool,\n"
        "        emoji_modifier_base : Bool,\n"
        "        emoji_component : Bool,\n"
        "        extended_pictographic : Bool,\n"
        "    }\n\n"
        "    lookup : U32 -> Properties\n"
        "    lookup = |scalar| {\n"
        "        value = if scalar < 128 {\n"
        "            ascii_value(scalar)\n"
        "        } else if scalar > 0x10FFFF {\n"
        "            0\n"
        "        } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        "            pages.get(offset) ?? 0\n"
        "        }\n\n"
        "        {\n"
        "            emoji: value.bitwise_and(0x01) != 0,\n"
        "            emoji_presentation: value.bitwise_and(0x02) != 0,\n"
        "            emoji_modifier: value.bitwise_and(0x04) != 0,\n"
        "            emoji_modifier_base: value.bitwise_and(0x08) != 0,\n"
        "            emoji_component: value.bitwise_and(0x10) != 0,\n"
        "            extended_pictographic: value.bitwise_and(0x20) != 0,\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "ascii_value : U32 -> U8\n"
        f"ascii_value = |u32| {ascii_expression}\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def _roc_string(value: str) -> str:
    return json.dumps(value, ensure_ascii=True)


def _unique_aliases(values: Iterable[str]) -> tuple[str, ...]:
    result: list[str] = []
    seen: set[str] = set()
    for value in values:
        if value not in seen:
            result.append(value)
            seen.add(value)
    return tuple(result)


def _enum_identities(
    aliases: tuple[PropertyValueAlias, ...], default_alias: str, *, source: str
) -> tuple[str, ...]:
    default = _resolve_alias(aliases, default_alias, source=source)
    return (default, *(record.identity for record in aliases if record.identity != default))


def _render_alias_members(
    identities: tuple[str, ...], aliases: tuple[PropertyValueAlias, ...]
) -> str:
    by_identity = {record.identity: record for record in aliases}
    short_matches: list[str] = []
    long_matches: list[str] = []
    count_matches: list[str] = []
    at_matches: list[str] = []
    parse_branches: list[str] = []
    for identity in identities:
        record = by_identity[identity]
        values = _unique_aliases((record.short, record.long, *record.aliases))
        short_matches.append(f"            {identity} => {_roc_string(record.short)}")
        long_matches.append(f"            {identity} => {_roc_string(record.long)}")
        count_matches.append(f"            {identity} => {len(values)}")
        for index, alias in enumerate(values):
            at_matches.append(
                f"            ({identity}, {index}) => Some({_roc_string(alias)})"
            )
        condition = " or ".join(
            f"InternalLooseAlias.matches(name, {_roc_string(alias)})" for alias in values
        )
        parse_branches.append(f"        if {condition} Some({identity})")
    parse_expression = " else ".join(parse_branches) + " else None"
    return (
        "    short : Value -> Str\n"
        "    short = |value| {\n"
        "        match value {\n"
        + "\n".join(short_matches)
        + "\n        }\n"
        "    }\n\n"
        "    long : Value -> Str\n"
        "    long = |value| {\n"
        "        match value {\n"
        + "\n".join(long_matches)
        + "\n        }\n"
        "    }\n\n"
        "    alias_count : Value -> U8\n"
        "    alias_count = |value| {\n"
        "        match value {\n"
        + "\n".join(count_matches)
        + "\n        }\n"
        "    }\n\n"
        "    alias_at : Value, U8 -> [Some(Str), None]\n"
        "    alias_at = |value, index| {\n"
        "        match (value, index) {\n"
        + "\n".join(at_matches)
        + "\n            _ => None\n"
        "        }\n"
        "    }\n\n"
        "    parse : Str -> [Some(Value), None]\n"
        f"    parse = |name| {parse_expression}\n"
    )


def render_enum_property(
    manifest: dict[str, object],
    version: str,
    *,
    generator: str,
    module: str,
    source: PropertySource,
    aliases: tuple[PropertyValueAlias, ...],
    property_alias: PropertyAlias,
    default_alias: str,
) -> str:
    identities = _enum_identities(aliases, default_alias, source=module)
    encoded = _encoded_property(source, identities)
    paged = _selected_paged_bytes(encoded, manifest=manifest, generator=generator)
    page_size = 1 << paged.page_bits
    ascii_expression = _ascii_value_expression(encoded, default=0)
    ascii_parameter = "u32" if "u32" in ascii_expression else "_u32"
    from_matches = "\n".join(
        f"        {index} => {identity}" for index, identity in enumerate(identities)
    )
    to_matches = "\n".join(
        f"            {identity} => {index}" for index, identity in enumerate(identities)
    )
    aliases_members = _render_alias_members(identities, aliases)
    return (
        f"## GENERATED from Unicode {version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        f"## layout: {len(paged.page_index)} {paged.index_type} page ids + {len(paged.pages)} x {page_size} U8 values; "
        f"logical payload {paged.storage_bytes} bytes. ##\n\n"
        "import InternalLooseAlias\n\n"
        f"{module} :: [].{{\n"
        f"    Value : [{', '.join(identities)}]\n"
        "    PropertyName : { short : Str, long : Str }\n\n"
        "    property_name : PropertyName\n"
        f"    property_name = {{ short: {_roc_string(property_alias.short)}, long: {_roc_string(property_alias.long)} }}\n\n"
        "    lookup : U32 -> Value\n"
        "    lookup = |scalar| from_u8(lookup_u8(scalar))\n\n"
        "    lookup_u8 : U32 -> U8\n"
        "    lookup_u8 = |scalar| {\n"
        "        if scalar < 128 {\n"
        "            ascii_value(scalar)\n"
        "        } else if scalar > 0x10FFFF {\n"
        "            0\n"
        "        } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            offset = page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()\n"
        "            pages.get(offset) ?? 0\n"
        "        }\n"
        "    }\n\n"
        + aliases_members
        + "}\n\n"
        f"from_u8 : U8 -> {module}.Value\n"
        "from_u8 = |value| {\n"
        "    match value {\n"
        f"{from_matches}\n"
        f"        _ => {identities[0]}\n"
        "    }\n"
        "}\n\n"
        f"to_u8 : {module}.Value -> U8\n"
        "to_u8 = |value| {\n"
        "    match value {\n"
        f"{to_matches}\n"
        "    }\n"
        "}\n\n"
        "ascii_value : U32 -> U8\n"
        f"ascii_value = |{ascii_parameter}| {ascii_expression}\n\n"
        f"page_index : List({paged.index_type})\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def _render_sparse_lookup(
    name: str, records: Iterable[SparseMapping]
) -> str:
    records = tuple(records)
    return (
        f"    {name} : U32 -> [Some(U32), None]\n"
        f"    {name} = |scalar| lookup_mapping(scalar, {name}_sources, {name}_targets)\n"
        f"\n{name}_sources : List(U32)\n"
        f"{name}_sources = {_roc_list(record.source for record in records)}\n\n"
        f"{name}_targets : List(U32)\n"
        f"{name}_targets = {_roc_list(record.target for record in records)}\n"
    )


def render_bidi_properties(
    manifest: dict[str, object],
    version: str,
    public: PublicProperties,
    canonical: CanonicalProperties,
) -> str:
    aliases = canonical.property_value_aliases["bc"]
    identities = _enum_identities(aliases, "Left_To_Right", source="Bidi_Class")
    encoded = _encoded_property(public.bidi_class, identities)
    for record in public.bidi_mirrored:
        for code_point in range(record.start, record.end + 1):
            encoded[code_point] |= 0x20
    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="bidi-properties")
    page_size = 1 << paged.page_bits
    ascii_expression = _ascii_value_expression(encoded, default=0)
    from_matches = "\n".join(
        f"        {index} => {identity}" for index, identity in enumerate(identities)
    )
    aliases_members = _render_alias_members(identities, aliases)
    property_alias = canonical.property_aliases["bc"]
    bracket_sources = _roc_list(record.source for record in public.bidi_brackets)
    bracket_targets = _roc_list(record.target for record in public.bidi_brackets)
    bracket_kinds = _roc_list(1 if record.kind == "o" else 2 for record in public.bidi_brackets)
    mirror_lookup = _render_sparse_lookup("mirroring_glyph", public.bidi_mirroring_glyph)
    return (
        f"## GENERATED from Unicode {version} bidi data. Run `python3 scripts/unicode_data.py generate`. ##\n"
        f"## Bidi_Class preserves {len(public.bidi_class.defaults)} ordered defaults; layout: "
        f"{len(paged.page_index)} {paged.index_type} ids + {len(paged.pages)} x {page_size} U8 values; "
        f"logical payload {paged.storage_bytes} bytes. ##\n\n"
        "import InternalLooseAlias\n\n"
        "InternalBidiProperties :: [].{\n"
        f"    Value : [{', '.join(identities)}]\n"
        "    BracketType : [Open, Close]\n"
        "    PropertyName : { short : Str, long : Str }\n\n"
        f"    property_name : PropertyName\n    property_name = {{ short: {_roc_string(property_alias.short)}, long: {_roc_string(property_alias.long)} }}\n\n"
        "    lookup : U32 -> Value\n    lookup = |scalar| class_from_u8(lookup_u8(scalar).bitwise_and(0x1F))\n\n"
        "    is_mirrored : U32 -> Bool\n    is_mirrored = |scalar| lookup_u8(scalar).bitwise_and(0x20) != 0\n\n"
        "    lookup_u8 : U32 -> U8\n"
        "    lookup_u8 = |scalar| {\n"
        "        if scalar < 128 { ascii_value(scalar) } else if scalar > 0x10FFFF { 0 } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            pages.get(page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()) ?? 0\n"
        "        }\n"
        "    }\n\n"
        + mirror_lookup
        + "\n\n    paired_bracket : U32 -> [Some({ scalar : U32, kind : BracketType }), None]\n"
        "    paired_bracket = |scalar| {\n"
        "        match lookup_index(scalar, bracket_sources) {\n"
        "            None => None\n"
        "            Some(index) => {\n"
        "                match (bracket_targets.get(index), bracket_kinds.get(index)) {\n"
        "                    (Ok(target), Ok(kind_id)) => Some({ scalar: target, kind: if kind_id == 1 Open else Close })\n"
        "                    _ => None\n"
        "                }\n"
        "            }\n"
        "        }\n"
        "    }\n\n"
        + aliases_members
        + "}\n\n"
        "class_from_u8 : U8 -> InternalBidiProperties.Value\nclass_from_u8 = |value| {\n    match value {\n"
        f"{from_matches}\n        _ => {identities[0]}\n    }}\n}}\n\n"
        "ascii_value : U32 -> U8\n"
        f"ascii_value = |u32| {ascii_expression}\n\n"
        "lookup_mapping : U32, List(U32), List(U32) -> [Some(U32), None]\n"
        "lookup_mapping = |scalar, sources, mapping_targets| {\n"
        "    match lookup_index(scalar, sources) {\n"
        "        None => None\n"
        "        Some(index) => match mapping_targets.get(index) { Ok(target) => Some(target), Err(_) => None }\n"
        "    }\n"
        "}\n\n"
        "lookup_index : U32, List(U32) -> [Some(U64), None]\n"
        "lookup_index = |scalar, sources| {\n"
        "    var low = 0.U64\n    var high = sources.len()\n"
        "    while low < high {\n"
        "        middle = low + (high - low) / 2\n"
        "        candidate = match sources.get(middle) { Ok(value) => value, Err(_) => return None }\n"
        "        if candidate < scalar { low = middle + 1 } else { high = middle }\n"
        "    }\n"
        "    if low >= sources.len() { None } else {\n"
        "        match sources.get(low) { Ok(value) if value == scalar => Some(low), _ => None }\n"
        "    }\n"
        "}\n\n"
        f"page_index : List({paged.index_type})\npage_index = {_roc_list(paged.page_index)}\n\n"
        f"pages : List(U8)\npages = {_roc_list(paged.flat_pages)}\n\n"
        f"bracket_sources : List(U32)\nbracket_sources = {bracket_sources}\n\n"
        f"bracket_targets : List(U32)\nbracket_targets = {bracket_targets}\n\n"
        f"bracket_kinds : List(U8)\nbracket_kinds = {bracket_kinds}\n"
    )


def render_character_flags(
    manifest: dict[str, object], version: str, public: PublicProperties
) -> str:
    encoded = bytearray(MAX_CODE_POINT + 1)
    for record in public.default_ignorable:
        for code_point in range(record.start, record.end + 1):
            encoded[code_point] |= 1
    for record in public.variation_selector:
        for code_point in range(record.start, record.end + 1):
            encoded[code_point] |= 2
    paged = _selected_paged_bytes(encoded, manifest=manifest, generator="character-flags")
    page_size = 1 << paged.page_bits
    return (
        f"## GENERATED from Unicode {version} binary properties. Run `python3 scripts/unicode_data.py generate`. ##\n"
        f"## layout: {len(paged.page_index)} {paged.index_type} ids + {len(paged.pages)} x {page_size} U8 values; logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalCharacterFlags :: [].{\n"
        "    lookup : U32 -> { default_ignorable : Bool, variation_selector : Bool }\n"
        "    lookup = |scalar| {\n"
        "        value = if scalar < 128 or scalar > 0x10FFFF { 0 } else {\n"
        f"            page_id = page_index.get(scalar.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"            pages.get(page_id.to_u64() * {page_size} + scalar.bitwise_and({page_size - 1}).to_u64()) ?? 0\n"
        "        }\n"
        "        { default_ignorable: value.bitwise_and(1) != 0, variation_selector: value.bitwise_and(2) != 0 }\n"
        "    }\n}\n\n"
        f"page_index : List({paged.index_type})\npage_index = {_roc_list(paged.page_index)}\n\n"
        f"pages : List(U8)\npages = {_roc_list(paged.flat_pages)}\n"
    )


def render_emoji_variations(version: str, bases: tuple[int, ...]) -> str:
    return (
        f"## GENERATED from Unicode {version} emoji variation sequences. Run `python3 scripts/unicode_data.py generate`. ##\n\n"
        "InternalEmojiVariations :: [].{\n"
        "    lookup : U32, U32 -> [Some([Text, Emoji]), None]\n"
        "    lookup = |base, selector| {\n"
        "        if !contains(base) { None } else if selector == 0xFE0E { Some(Text) } else if selector == 0xFE0F { Some(Emoji) } else { None }\n"
        "    }\n}\n\n"
        "contains : U32 -> Bool\ncontains = |scalar| {\n"
        "    var low = 0.U64\n    var high = bases.len()\n"
        "    while low < high {\n"
        "        middle = low + (high - low) / 2\n"
        "        candidate = match bases.get(middle) { Ok(value) => value, Err(_) => return Bool.False }\n"
        "        if candidate < scalar { low = middle + 1 } else { high = middle }\n"
        "    }\n"
        "    if low >= bases.len() { Bool.False } else {\n"
        "        match bases.get(low) { Ok(value) => value == scalar, Err(_) => Bool.False }\n"
        "    }\n}\n\n"
        f"bases : List(U32)\nbases = {_roc_list(bases)}\n"
    )


def _binary_byte(records: Iterable[RangeRecord], bit: int = 1) -> bytearray:
    encoded = bytearray(MAX_CODE_POINT + 1)
    for record in records:
        encoded[record.start : record.end + 1] = bytes((bit,)) * (
            record.end - record.start + 1
        )
    return encoded


def _canonical_scalar_columns(
    canonical: CanonicalProperties,
    algorithm: AlgorithmProperties,
    public: PublicProperties,
) -> tuple[dict[str, bytearray], dict[str, tuple[str, ...]]]:
    categories = tuple(sorted({record.property for record in canonical.general_category}))
    gc_source = PropertySource(
        canonical.general_category,
        (MissingDefault(0, MAX_CODE_POINT, "General_Category", canonical.general_category_default, 0),),
    )
    gc = _encoded_property(gc_source, categories)

    ccc = bytearray((canonical.canonical_combining_class_default,)) * (MAX_CODE_POINT + 1)
    for record in canonical.canonical_combining_class:
        ccc[record.start : record.end + 1] = bytes((int(record.property),)) * (
            record.end - record.start + 1
        )

    eaw_identities = EAW_PROPERTIES
    eaw = bytearray((eaw_identities.index("N"),)) * (MAX_CODE_POINT + 1)
    for default in algorithm.east_asian_width.defaults:
        value = eaw_identities.index(default.value)
        eaw[default.start : default.end + 1] = bytes((value,)) * (default.end - default.start + 1)
    for record in algorithm.east_asian_width.records:
        value = eaw_identities.index(record.property)
        eaw[record.start : record.end + 1] = bytes((value,)) * (record.end - record.start + 1)

    emoji = bytearray(MAX_CODE_POINT + 1)
    for bit, property_name in enumerate(EMOJI_PROPERTIES):
        for record in _ranges_for(list(algorithm.emoji.records), property_name):
            for code_point in range(record.start, record.end + 1):
                emoji[code_point] |= 1 << bit

    bc_ids = _enum_identities(canonical.property_value_aliases["bc"], "Left_To_Right", source="bc")
    bc = _encoded_property(public.bidi_class, bc_ids)
    for record in public.bidi_mirrored:
        for code_point in range(record.start, record.end + 1):
            bc[code_point] |= 0x20

    jt_ids = _enum_identities(canonical.property_value_aliases["jt"], "Non_Joining", source="jt")
    jt = _encoded_property(public.joining_type, jt_ids)
    jg_ids = _enum_identities(canonical.property_value_aliases["jg"], "No_Joining_Group", source="jg")
    jg_source = PropertySource(
        public.joining_group,
        (MissingDefault(0, MAX_CODE_POINT, "Joining_Group", jg_ids[0], 0),),
    )
    jg = _encoded_property(jg_source, jg_ids)
    insc_ids = _enum_identities(canonical.property_value_aliases["InSC"], "Other", source="InSC")
    insc = _encoded_property(public.indic_syllabic_category, insc_ids)
    inpc_ids = _enum_identities(canonical.property_value_aliases["InPC"], "Not_Applicable", source="InPC")
    inpc = _encoded_property(public.indic_positional_category, inpc_ids)
    vo_ids = _enum_identities(canonical.property_value_aliases["vo"], "R", source="vo")
    vo = _encoded_property(public.vertical_orientation, vo_ids)

    flags = bytearray(MAX_CODE_POINT + 1)
    for record in public.default_ignorable:
        for code_point in range(record.start, record.end + 1):
            flags[code_point] |= 1
    for record in public.variation_selector:
        for code_point in range(record.start, record.end + 1):
            flags[code_point] |= 2
    return (
        {
            "general_category": gc,
            "canonical_combining_class": ccc,
            "east_asian_width": eaw,
            "emoji": emoji,
            "bidi": bc,
            "joining_type": jt,
            "joining_group": jg,
            "indic_syllabic_category": insc,
            "indic_positional_category": inpc,
            "vertical_orientation": vo,
            "flags": flags,
        },
        {
            "general_category": categories,
            "east_asian_width": eaw_identities,
            "bidi": bc_ids,
            "joining_type": jt_ids,
            "joining_group": jg_ids,
            "indic_syllabic_category": insc_ids,
            "indic_positional_category": inpc_ids,
            "vertical_orientation": vo_ids,
        },
    )


def render_composite_properties(
    manifest: dict[str, object],
    version: str,
    canonical: CanonicalProperties,
    algorithm: AlgorithmProperties,
    public: PublicProperties,
) -> str:
    scalar_columns, identities = _canonical_scalar_columns(canonical, algorithm, public)
    names = tuple(scalar_columns)
    row_ids: list[int] = []
    row_by_values: dict[tuple[int, ...], int] = {}
    rows: list[tuple[int, ...]] = []
    for code_point in range(MAX_CODE_POINT + 1):
        values = tuple(scalar_columns[name][code_point] for name in names)
        row_id = row_by_values.get(values)
        if row_id is None:
            row_id = len(rows)
            row_by_values[values] = row_id
            rows.append(values)
        row_ids.append(row_id)
    paged = _selected_paged_u16(row_ids, manifest=manifest, generator="composite-properties")
    artifact_name = _artifact_for_generator(manifest, "composite-properties")
    layout = _require_dict(
        _require_dict(manifest["artifacts"], "manifest.artifacts")[artifact_name]["layout"],
        f"manifest.artifacts.{artifact_name}.layout",
    )
    column_bytes = len(rows) * len(names)
    if len(rows) != layout["expected_row_count"] or column_bytes != layout["expected_column_bytes"]:
        raise DataError(
            f"composite row layout drifted: expected {layout['expected_row_count']} rows/{layout['expected_column_bytes']} bytes, "
            f"got {len(rows)} rows/{column_bytes} bytes"
        )
    page_size = 1 << paged.page_bits
    column_lists = {name: tuple(row[index] for row in rows) for index, name in enumerate(names)}
    rendered_columns = "\n\n".join(
        f"{name}_rows : List(U8)\n{name}_rows = {_roc_list(values)}"
        for name, values in column_lists.items()
    )
    def conversion(name: str, type_name: str) -> str:
        values = identities[name]
        matches = "\n".join(
            f"        {index} => {identity}" for index, identity in enumerate(values)
        )
        return (
            f"{name}_from_u8 : U8 -> InternalCompositeProperties.{type_name}\n"
            f"{name}_from_u8 = |value| {{\n    match value {{\n{matches}\n        _ => {values[0]}\n    }}\n}}"
        )

    conversions = "\n\n".join(
        (
            conversion("general_category", "GeneralCategory"),
            conversion("east_asian_width", "EastAsianWidth"),
            conversion("bidi", "BidiClass").replace("bidi_from_u8", "bidi_class_from_u8"),
            conversion("joining_type", "JoiningType"),
            conversion("joining_group", "JoiningGroup"),
            conversion("indic_syllabic_category", "IndicSyllabicCategory"),
            conversion("indic_positional_category", "IndicPositionalCategory"),
            conversion("vertical_orientation", "VerticalOrientation"),
        )
    )
    return (
        f"## GENERATED from Unicode {version}. Composite-only fused scalar view. ##\n"
        "## Direct property modules do not import this table. ##\n"
        f"## layout: {len(paged.page_index)} {paged.index_type} ids + {len(paged.pages)} x {page_size} U16 row ids "
        f"({paged.storage_bytes} bytes), {len(rows)} rows x {len(names)} U8 columns ({column_bytes} bytes), "
        f"total {paged.storage_bytes + column_bytes} bytes. ##\n\n"
        "InternalCompositeProperties :: [].{\n"
        f"    GeneralCategory : [{', '.join(identities['general_category'])}]\n"
        f"    EastAsianWidth : [{', '.join(identities['east_asian_width'])}]\n"
        f"    BidiClass : [{', '.join(identities['bidi'])}]\n"
        f"    JoiningType : [{', '.join(identities['joining_type'])}]\n"
        f"    JoiningGroup : [{', '.join(identities['joining_group'])}]\n"
        f"    IndicSyllabicCategory : [{', '.join(identities['indic_syllabic_category'])}]\n"
        f"    IndicPositionalCategory : [{', '.join(identities['indic_positional_category'])}]\n"
        f"    VerticalOrientation : [{', '.join(identities['vertical_orientation'])}]\n"
        "    EmojiProperties : { emoji : Bool, emoji_presentation : Bool, emoji_modifier : Bool, emoji_modifier_base : Bool, emoji_component : Bool, extended_pictographic : Bool }\n"
        "    RowId : U16\n\n"
        "    ## Resolve the dense composite row once. Individual columns remain\n"
        "    ## lazy so callers do not materialize properties they never inspect.\n"
        "    lookup_id : U32 -> RowId\n"
        "    lookup_id = |scalar| {\n"
        "        bounded = if scalar <= 0x10FFFF scalar else 0\n"
        f"        page_id = page_index.get(bounded.shr_wrap({paged.page_bits}).to_u64()) ?? 0\n"
        f"        row_ids.get(page_id.to_u64() * {page_size} + bounded.bitwise_and({page_size - 1}).to_u64()) ?? 0\n"
        "    }\n\n"
        "    general_category : RowId -> GeneralCategory\n"
        "    general_category = |row_id| general_category_from_u8(general_category_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    canonical_combining_class : RowId -> U8\n"
        "    canonical_combining_class = |row_id| canonical_combining_class_rows.get(row_id.to_u64()) ?? 0\n\n"
        "    east_asian_width : RowId -> EastAsianWidth\n"
        "    east_asian_width = |row_id| east_asian_width_from_u8(east_asian_width_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    emoji : RowId -> EmojiProperties\n"
        "    emoji = |row_id| emoji_from_u8(emoji_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    bidi_class : RowId -> BidiClass\n"
        "    bidi_class = |row_id| bidi_class_from_u8((bidi_rows.get(row_id.to_u64()) ?? 0).bitwise_and(0x1F))\n\n"
        "    bidi_mirrored : RowId -> Bool\n"
        "    bidi_mirrored = |row_id| (bidi_rows.get(row_id.to_u64()) ?? 0).bitwise_and(0x20) != 0\n\n"
        "    joining_type : RowId -> JoiningType\n"
        "    joining_type = |row_id| joining_type_from_u8(joining_type_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    joining_group : RowId -> JoiningGroup\n"
        "    joining_group = |row_id| joining_group_from_u8(joining_group_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    indic_syllabic_category : RowId -> IndicSyllabicCategory\n"
        "    indic_syllabic_category = |row_id| indic_syllabic_category_from_u8(indic_syllabic_category_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    indic_positional_category : RowId -> IndicPositionalCategory\n"
        "    indic_positional_category = |row_id| indic_positional_category_from_u8(indic_positional_category_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    vertical_orientation : RowId -> VerticalOrientation\n"
        "    vertical_orientation = |row_id| vertical_orientation_from_u8(vertical_orientation_rows.get(row_id.to_u64()) ?? 0)\n\n"
        "    default_ignorable : RowId -> Bool\n"
        "    default_ignorable = |row_id| (flags_rows.get(row_id.to_u64()) ?? 0).bitwise_and(1) != 0\n\n"
        "    variation_selector : RowId -> Bool\n"
        "    variation_selector = |row_id| (flags_rows.get(row_id.to_u64()) ?? 0).bitwise_and(2) != 0\n"
        "}\n\n"
        f"page_index : List({paged.index_type})\npage_index = {_roc_list(paged.page_index)}\n\n"
        f"row_ids : List(U16)\nrow_ids = {_roc_list(paged.flat_pages)}\n\n"
        "emoji_from_u8 : U8 -> InternalCompositeProperties.EmojiProperties\n"
        "emoji_from_u8 = |value| {\n"
        "    emoji: value.bitwise_and(1) != 0,\n"
        "    emoji_presentation: value.bitwise_and(2) != 0,\n"
        "    emoji_modifier: value.bitwise_and(4) != 0,\n"
        "    emoji_modifier_base: value.bitwise_and(8) != 0,\n"
        "    emoji_component: value.bitwise_and(16) != 0,\n"
        "    extended_pictographic: value.bitwise_and(32) != 0,\n"
        "}\n\n"
        f"{conversions}\n\n"
        f"{rendered_columns}\n"
    )


def render_property_aliases(
    version: str,
    canonical: CanonicalProperties,
    general_category_module: str,
) -> str:
    general_category_property = canonical.property_aliases["gc"]
    combining_class_property = canonical.property_aliases["ccc"]
    observed_categories = {record.property for record in canonical.general_category}
    general_category_values = sorted(
        (
            record
            for record in canonical.property_value_aliases["gc"]
            if record.identity in observed_categories
        ),
        key=lambda record: record.identity,
    )
    combining_class_values = sorted(
        canonical.property_value_aliases["ccc"], key=lambda record: int(record.identity)
    )

    gc_short_matches = []
    gc_long_matches = []
    gc_count_matches = []
    gc_at_matches = []
    gc_parse_branches = []
    for record in general_category_values:
        aliases = _unique_aliases((record.short, record.long, *record.aliases))
        gc_short_matches.append(
            f"        {record.identity} => {_roc_string(record.short)}"
        )
        gc_long_matches.append(
            f"        {record.identity} => {_roc_string(record.long)}"
        )
        gc_count_matches.append(f"        {record.identity} => {len(aliases)}")
        for index, alias in enumerate(aliases):
            gc_at_matches.append(
                f"        ({record.identity}, {index}) => Some({_roc_string(alias)})"
            )
        gc_parse_branches.append(
            "        if "
            + " or ".join(
                f"InternalLooseAlias.matches(name, {_roc_string(alias)})" for alias in aliases
            )
            + f" Some({record.identity})"
        )
    ccc_short_matches = []
    ccc_long_matches = []
    ccc_count_matches = []
    ccc_at_matches = []
    ccc_parse_branches = []
    for record in combining_class_values:
        aliases = _unique_aliases(
            (record.identity, record.short, record.long, *record.aliases)
        )
        ccc_short_matches.append(
            f"        {record.identity} => Some({_roc_string(record.short)})"
        )
        ccc_long_matches.append(
            f"        {record.identity} => Some({_roc_string(record.long)})"
        )
        ccc_count_matches.append(f"        {record.identity} => {len(aliases)}")
        for index, alias in enumerate(aliases):
            ccc_at_matches.append(
                f"        ({record.identity}, {index}) => Some({_roc_string(alias)})"
            )
        ccc_parse_branches.append(
            "        if "
            + " or ".join(
                f"InternalLooseAlias.matches(name, {_roc_string(alias)})" for alias in aliases
            )
            + f" Some({record.identity})"
        )

    return (
        f"## GENERATED from Unicode {version} PropertyAliases and PropertyValueAliases. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## These canonical names are metadata for stable identities, never storage ordinals. ##\n"
        f"## metadata: {len(general_category_values)} scalar category identities; "
        f"{len(combining_class_values)} exact CCC identities. ##\n\n"
        f"import {general_category_module}\n"
        "import InternalLooseAlias\n\n"
        "InternalPropertyAliases :: [].{\n"
        "    PropertyName : { short : Str, long : Str }\n\n"
        "    general_category_property : PropertyName\n"
        "    general_category_property = { "
        f"short: {_roc_string(general_category_property.short)}, "
        f"long: {_roc_string(general_category_property.long)} }}\n\n"
        "    canonical_combining_class_property : PropertyName\n"
        "    canonical_combining_class_property = { "
        f"short: {_roc_string(combining_class_property.short)}, "
        f"long: {_roc_string(combining_class_property.long)} }}\n\n"
        f"    general_category_short : {general_category_module}.GeneralCategory -> Str\n"
        "    general_category_short = |category| {\n"
        "        match category {\n"
        + "\n".join(gc_short_matches)
        + "\n        }\n"
        "    }\n\n"
        f"    general_category_long : {general_category_module}.GeneralCategory -> Str\n"
        "    general_category_long = |category| {\n"
        "        match category {\n"
        + "\n".join(gc_long_matches)
        + "\n        }\n"
        "    }\n\n"
        f"    general_category_alias_count : {general_category_module}.GeneralCategory -> U8\n"
        "    general_category_alias_count = |category| {\n"
        "        match category {\n"
        + "\n".join(gc_count_matches)
        + "\n        }\n"
        "    }\n\n"
        f"    general_category_alias_at : {general_category_module}.GeneralCategory, U8 -> [Some(Str), None]\n"
        "    general_category_alias_at = |category, index| {\n"
        "        match (category, index) {\n"
        + "\n".join(gc_at_matches)
        + "\n            _ => None\n"
        "        }\n"
        "    }\n\n"
        f"    general_category_parse : Str -> [Some({general_category_module}.GeneralCategory), None]\n"
        "    general_category_parse = |name| "
        + " else ".join(gc_parse_branches)
        + " else None\n\n"
        "    canonical_combining_class_short : U8 -> [Some(Str), None]\n"
        "    canonical_combining_class_short = |value| {\n"
        "        match value {\n"
        + "\n".join(ccc_short_matches)
        + "\n            _ => None\n"
        "        }\n"
        "    }\n\n"
        "    canonical_combining_class_long : U8 -> [Some(Str), None]\n"
        "    canonical_combining_class_long = |value| {\n"
        "        match value {\n"
        + "\n".join(ccc_long_matches)
        + "\n            _ => None\n"
        "        }\n"
        "    }\n\n"
        "    canonical_combining_class_alias_count : U8 -> U8\n"
        "    canonical_combining_class_alias_count = |value| {\n"
        "        match value {\n"
        + "\n".join(ccc_count_matches)
        + "\n            _ => 0\n"
        "        }\n"
        "    }\n\n"
        "    canonical_combining_class_alias_at : U8, U8 -> [Some(Str), None]\n"
        "    canonical_combining_class_alias_at = |value, index| {\n"
        "        match (value, index) {\n"
        + "\n".join(ccc_at_matches)
        + "\n            _ => None\n"
        "        }\n"
        "    }\n\n"
        "    canonical_combining_class_parse : Str -> [Some(U8), None]\n"
        "    canonical_combining_class_parse = |name| "
        + " else ".join(ccc_parse_branches)
        + " else None\n"
        "}\n"
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


def render_eaw(
    version: str, records: list[RangeRecord], defaults: Iterable[MissingDefault]
) -> str:
    branches = []
    for prop in ("Na", "A", "W", "H", "F", "N"):
        ranges = _merge_adjacent(_ranges_for(records, prop))
        branches.append(f"if {_condition(ranges, hexadecimal=True)} ({prop})")
    wide_defaults = [
        RangeRecord(default.start, default.end, default.value, default.line)
        for default in defaults
        if default.value == "W"
    ]
    branches.append(f"if {_condition(wide_defaults, hexadecimal=True)} (W)")
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


def render_legacy_emoji(
    version: str, emoji_version: str, emoji_data_module: str
) -> str:
    return (
        f"## GENERATED from vendor/unicode/{version} (Emoji {emoji_version}). Run `python3 scripts/unicode_data.py generate`. ##\n"
        "import CodePoint\n"
        f"import {emoji_data_module}\n\n"
        "InternalEmoji :: {}.{\n"
        "    EMOJI : [Pictographic, Base, Modifier, Presentation, Component, Emoji]\n\n"
        "    from_cp : CodePoint -> Try(EMOJI, [NonEmojiCodePoint])\n"
        "    from_cp = |cp| {\n"
        "        u32 = cp.to_u32()\n\n"
        f"        properties = {emoji_data_module}.lookup(u32)\n"
        "        if properties.extended_pictographic {\n"
        "            Ok(Pictographic)\n"
        "        } else if properties.emoji_modifier_base {\n"
        "            Ok(Base)\n"
        "        } else if properties.emoji_modifier {\n"
        "            Ok(Modifier)\n"
        "        } else if properties.emoji_presentation {\n"
        "            Ok(Presentation)\n"
        "        } else if properties.emoji_component {\n"
        "            Ok(Component)\n"
        "        } else if properties.emoji {\n"
        "            Ok(Emoji)\n"
        "        } else {\n"
        "            Err(NonEmojiCodePoint)\n"
        "        }\n"
        "    }\n\n"
        "    is_pictographic : U32 -> Bool\n"
        f"    is_pictographic = |u32| {emoji_data_module}.lookup(u32).extended_pictographic\n\n"
        "    is_base : U32 -> Bool\n"
        f"    is_base = |u32| {emoji_data_module}.lookup(u32).emoji_modifier_base\n\n"
        "    is_modifier : U32 -> Bool\n"
        f"    is_modifier = |u32| {emoji_data_module}.lookup(u32).emoji_modifier\n\n"
        "    is_presentation : U32 -> Bool\n"
        f"    is_presentation = |u32| {emoji_data_module}.lookup(u32).emoji_presentation\n\n"
        "    is_component : U32 -> Bool\n"
        f"    is_component = |u32| {emoji_data_module}.lookup(u32).emoji_component\n\n"
        "    is_emoji : U32 -> Bool\n"
        f"    is_emoji = |u32| {emoji_data_module}.lookup(u32).emoji\n"
        "}\n"
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
    properties = load_property_data(manifest)
    gcb = list(properties.grapheme.records)
    eaw = list(properties.east_asian_width.records)
    emoji = list(properties.emoji.records)
    incb = list(properties.indic_conjunct_break.records)
    canonical = load_canonical_properties(manifest)
    _line_break_records, line_break_values = load_line_break_properties(manifest)
    public = load_public_properties(manifest, canonical)
    scripts = load_script_properties(manifest)
    version = release_version(manifest, "unicode")
    emoji_version = release_version(manifest, "emoji")
    emoji_data_module = _module_for_generator(manifest, "emoji-properties")
    general_category_module = _module_for_generator(manifest, "general-category")
    jg_identities = _enum_identities(
        canonical.property_value_aliases["jg"], "No_Joining_Group", source="Joining_Group"
    )
    joining_group_source = PropertySource(
        public.joining_group,
        (MissingDefault(0, MAX_CODE_POINT, "Joining_Group", jg_identities[0], 0),),
    )
    generators: dict[str, Callable[[], str]] = {
        "unicode-version": lambda: render_unicode_version(version),
        "grapheme-data": lambda: render_grapheme_data(manifest, version, gcb, incb, emoji),
        "legacy-grapheme-break": lambda: render_gcb(version, gcb),
        "east-asian-width": lambda: render_eaw(
            version, eaw, properties.east_asian_width.defaults
        ),
        "emoji-properties": lambda: render_emoji_data(
            manifest, version, emoji_version, emoji
        ),
        "legacy-emoji": lambda: render_legacy_emoji(
            version, emoji_version, emoji_data_module
        ),
        "line-break-data": lambda: render_line_break_data(
            manifest,
            version,
            line_break_values,
            canonical,
            properties.east_asian_width,
            emoji,
        ),
        "general-category": lambda: render_general_category(
            manifest, version, canonical.general_category, canonical.general_category_default
        ),
        "canonical-combining-class": lambda: render_canonical_combining_class(
            manifest,
            version,
            canonical.canonical_combining_class,
            canonical.canonical_combining_class_default,
        ),
        "property-aliases": lambda: render_property_aliases(
            version, canonical, general_category_module
        ),
        "bidi-properties": lambda: render_bidi_properties(
            manifest, version, public, canonical
        ),
        "joining-type": lambda: render_enum_property(
            manifest,
            version,
            generator="joining-type",
            module="InternalJoiningType",
            source=public.joining_type,
            aliases=canonical.property_value_aliases["jt"],
            property_alias=canonical.property_aliases["jt"],
            default_alias="Non_Joining",
        ),
        "joining-group": lambda: render_enum_property(
            manifest,
            version,
            generator="joining-group",
            module="InternalJoiningGroup",
            source=joining_group_source,
            aliases=canonical.property_value_aliases["jg"],
            property_alias=canonical.property_aliases["jg"],
            default_alias="No_Joining_Group",
        ),
        "indic-syllabic-category": lambda: render_enum_property(
            manifest,
            version,
            generator="indic-syllabic-category",
            module="InternalIndicSyllabicCategory",
            source=public.indic_syllabic_category,
            aliases=canonical.property_value_aliases["InSC"],
            property_alias=canonical.property_aliases["InSC"],
            default_alias="Other",
        ),
        "indic-positional-category": lambda: render_enum_property(
            manifest,
            version,
            generator="indic-positional-category",
            module="InternalIndicPositionalCategory",
            source=public.indic_positional_category,
            aliases=canonical.property_value_aliases["InPC"],
            property_alias=canonical.property_aliases["InPC"],
            default_alias="Not_Applicable",
        ),
        "vertical-orientation": lambda: render_enum_property(
            manifest,
            version,
            generator="vertical-orientation",
            module="InternalVerticalOrientation",
            source=public.vertical_orientation,
            aliases=canonical.property_value_aliases["vo"],
            property_alias=canonical.property_aliases["vo"],
            default_alias="R",
        ),
        "character-flags": lambda: render_character_flags(manifest, version, public),
        "emoji-variations": lambda: render_emoji_variations(
            version, public.emoji_variation_bases
        ),
        "composite-properties": lambda: render_composite_properties(
            manifest, version, canonical, properties, public
        ),
        "script-data": lambda: render_script_data(manifest, version, scripts),
        "script-extensions-data": lambda: render_script_extensions_data(
            manifest, version, scripts
        ),
        "script-api": lambda: render_script_api(version, scripts),
    }

    artifacts = _require_dict(manifest["artifacts"], "manifest.artifacts")

    def render_once() -> dict[Path, str]:
        rendered: dict[Path, str] = {}
        for name in sorted(artifacts):
            artifact = _require_dict(artifacts[name], f"manifest.artifacts.{name}")
            generator_name = str(artifact["generator"])
            definition = generators.get(generator_name)
            if definition is None:
                raise DataError(
                    f"manifest.artifacts.{name}.generator {generator_name!r} is not implemented"
                )
            contract = GENERATOR_CONTRACTS[generator_name]
            declared_output = str(artifact["output"])
            if declared_output != contract.output:
                raise DataError(
                    f"manifest.artifacts.{name}.output must be the authoritative generated module {contract.output!r}"
                )
            output = Path(declared_output)
            if output.parent != Path("package") or output.suffix != ".roc":
                raise DataError(
                    f"manifest.artifacts.{name}.output is not a generated package module"
                )
            rendered[
                _path_below_root(declared_output, f"manifest.artifacts.{name}.output")
            ] = definition()
        return rendered

    first = render_once()
    second = render_once()
    if first != second:
        raise DataError("generator output changed across two renders")
    return first


def validate_all(manifest: dict[str, object]) -> None:
    licenses = _require_dict(manifest["licenses"], "manifest.licenses")
    for name, raw_license in licenses.items():
        license_item = _require_dict(raw_license, f"manifest.licenses.{name}")
        license_path = _path_below_root(
            str(license_item["path"]), f"manifest.licenses.{name}.path"
        )
        if not license_path.is_file():
            raise DataError(f"missing data license: {license_path}")
    for source in _require_dict(manifest["sources"], "manifest.sources"):
        verify_source(manifest, source)
    load_property_data(manifest)
    canonical = load_canonical_properties(manifest)
    load_line_break_properties(manifest)
    load_public_properties(manifest, canonical)
    load_script_properties(manifest)
    parse_grapheme_tests(manifest)
    parse_line_break_tests(manifest)
    rendered_modules(manifest)


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
            print(f"Unicode {release_version(manifest, 'unicode')} data are valid")
        elif args.command == "generate":
            validate_all(manifest)
            generate(manifest, check=args.check)
    except DataError as err:
        print(f"error: {err}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
