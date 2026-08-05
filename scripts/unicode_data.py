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
class PagedBytes:
    page_index: tuple[int, ...]
    pages: tuple[bytes, ...]

    @property
    def flat_pages(self) -> tuple[int, ...]:
        return tuple(value for page in self.pages for value in page)

    @property
    def storage_bytes(self) -> int:
        return len(self.page_index) * 2 + len(self.pages) * PAGE_SIZE


@dataclass(frozen=True)
class CanonicalProperties:
    general_category: tuple[RangeRecord, ...]
    general_category_default: str
    canonical_combining_class: tuple[RangeRecord, ...]
    canonical_combining_class_default: int
    property_aliases: dict[str, PropertyAlias]
    property_value_aliases: dict[str, tuple[PropertyValueAlias, ...]]

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

    releases = _require_dict(manifest.get("releases"), "manifest.releases")
    authorities = _require_dict(manifest.get("authorities"), "manifest.authorities")
    licenses = _require_dict(manifest.get("licenses"), "manifest.licenses")
    specifications = _require_dict(manifest.get("specifications"), "manifest.specifications")
    sources = _require_dict(manifest.get("sources"), "manifest.sources")
    artifacts = _require_dict(manifest.get("artifacts"), "manifest.artifacts")
    if not releases or not sources or not artifacts:
        raise DataError("manifest releases, sources, and artifacts must not be empty")

    for name, raw_license in licenses.items():
        _validate_identifier(name, "manifest.licenses")
        item = _require_dict(raw_license, f"manifest.licenses.{name}")
        if not isinstance(item.get("path"), str):
            raise DataError(f"manifest.licenses.{name}.path has the wrong type")
        _path_below_root(str(item["path"]), f"manifest.licenses.{name}.path")

    for name, raw_authority in authorities.items():
        _validate_identifier(name, "manifest.authorities")
        item = _require_dict(raw_authority, f"manifest.authorities.{name}")
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
        synchronized = item.get("synchronized_with")
        if synchronized is not None and synchronized not in releases:
            raise DataError(f"manifest.releases.{name}.synchronized_with names an unknown release")
        prefix = item.get("vendor_prefix")
        if prefix is not None:
            if not isinstance(prefix, str):
                raise DataError(f"manifest.releases.{name}.vendor_prefix has the wrong type")
            _path_below_root(prefix, f"manifest.releases.{name}.vendor_prefix")

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

    for name, raw_specification in specifications.items():
        _validate_identifier(name, "manifest.specifications")
        item = _require_dict(raw_specification, f"manifest.specifications.{name}")
        if not isinstance(item.get("revision"), str):
            raise DataError(f"manifest.specifications.{name}.revision has the wrong type")
        if item.get("release") not in releases:
            raise DataError(f"manifest.specifications.{name}.release names an unknown release")

    for name, raw_source in sources.items():
        _validate_identifier(name, "manifest.sources")
        item = _require_dict(raw_source, f"manifest.sources.{name}")
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
        properties = _require_string_list(item.get("properties"), f"manifest.sources.{name}.properties")
        if len(properties) != len(set(properties)):
            raise DataError(f"manifest.sources.{name}.properties contains duplicates")
        if not any(release_version(manifest, axis) in str(item["header"]) for axis in release_axes):
            raise DataError(f"manifest.sources.{name}.header does not match a release axis")

    outputs: set[str] = set()
    dependencies: dict[str, list[str]] = {}
    for name, raw_artifact in artifacts.items():
        _validate_identifier(name, "manifest.artifacts")
        item = _require_dict(raw_artifact, f"manifest.artifacts.{name}")
        for field in ("generator", "output"):
            if not isinstance(item.get(field), str):
                raise DataError(f"manifest.artifacts.{name}.{field} has the wrong type")
        output = str(item["output"])
        _path_below_root(output, f"manifest.artifacts.{name}.output")
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
        dependencies[name] = artifact_dependencies

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
    """Unicode loose matching for property and property-value aliases."""
    return "".join(character.lower() for character in value if character not in " _-")


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


def _full_domain_default(
    defaults: Iterable[MissingDefault], *, source: str, property_name: str | None
) -> str:
    matches = [
        default
        for default in defaults
        if default.start == 0
        and default.end == MAX_CODE_POINT
        and (property_name is None or default.property == property_name)
    ]
    if len(matches) != 1:
        label = property_name or "the property"
        raise DataError(f"{source}: expected exactly one full-domain @missing default for {label}")
    return matches[0].value


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


def load_canonical_properties(manifest: dict[str, object]) -> CanonicalProperties:
    property_alias_path = data_path(manifest, "property_aliases")
    property_value_alias_path = data_path(manifest, "property_value_aliases")
    property_alias_text = verify_source(manifest, "property_aliases")
    property_value_alias_text = verify_source(manifest, "property_value_aliases")
    property_aliases = parse_property_aliases(
        property_alias_text, source=str(property_alias_path)
    )
    value_aliases = parse_property_value_aliases(
        property_value_alias_text, source=str(property_value_alias_path)
    )
    for required in ("gc", "ccc"):
        if required not in property_aliases or required not in value_aliases:
            raise DataError(f"property alias sources are missing required property {required!r}")

    general_category_path = data_path(manifest, "derived_general_category")
    general_category_text = verify_source(manifest, "derived_general_category")
    general_category_values = value_aliases["gc"]
    general_category_aliases = {record.identity for record in general_category_values}
    general_category = parse_ranges(
        general_category_text,
        source=str(general_category_path),
        allowed_properties=general_category_aliases,
        default_marker=None,
    )
    _validate_full_coverage(general_category, source=str(general_category_path))
    general_category_missing = _full_domain_default(
        parse_missing_defaults(property_value_alias_text, source=str(property_value_alias_path)),
        source=str(property_value_alias_path),
        property_name="General_Category",
    )
    general_category_default = _resolve_alias(
        general_category_values, general_category_missing, source=str(property_value_alias_path)
    )
    if general_category_default not in general_category_aliases:
        raise DataError("General_Category default is not a declared stable value")

    combining_class_path = data_path(manifest, "derived_combining_class")
    combining_class_text = verify_source(manifest, "derived_combining_class")
    combining_class_values = value_aliases["ccc"]
    combining_class = parse_numeric_ranges(
        combining_class_text, source=str(combining_class_path)
    )
    combining_class_missing = _full_domain_default(
        parse_missing_defaults(combining_class_text, source=str(combining_class_path)),
        source=str(combining_class_path),
        property_name=None,
    )
    combining_class_default_identity = _resolve_alias(
        combining_class_values, combining_class_missing, source=str(combining_class_path)
    )
    combining_class_default = int(combining_class_default_identity)
    declared_classes = {int(record.identity) for record in combining_class_values}
    observed_classes = {int(record.property) for record in combining_class}
    unknown_classes = observed_classes - declared_classes
    if unknown_classes:
        raise DataError(
            f"Canonical_Combining_Class data use undeclared values {sorted(unknown_classes)}"
        )

    return CanonicalProperties(
        tuple(general_category),
        general_category_default,
        tuple(combining_class),
        combining_class_default,
        property_aliases,
        value_aliases,
    )


def parse_grapheme_tests(
    manifest: dict[str, object], text: str | None = None
) -> list[GraphemeCase]:
    version = release_version(manifest, "unicode")
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


def _paged_bytes(values: bytes | bytearray) -> PagedBytes:
    if len(values) != MAX_CODE_POINT + 1:
        raise DataError("paged scalar view must cover exactly U+0000..U+10FFFF")
    page_ids: dict[bytes, int] = {}
    pages: list[bytes] = []
    page_index: list[int] = []
    for start in range(0, len(values), PAGE_SIZE):
        page = bytes(values[start : start + PAGE_SIZE])
        page_id = page_ids.get(page)
        if page_id is None:
            page_id = len(pages)
            page_ids[page] = page_id
            pages.append(page)
        page_index.append(page_id)
    if len(pages) > 0x10000:
        raise DataError("page index no longer fits in U16")
    return PagedBytes(tuple(page_index), tuple(pages))


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

    paged = _paged_bytes(encoded)

    return (
        f"## GENERATED from Unicode {version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        f"## layout: {len(paged.page_index)} U16 page ids + {len(paged.pages)} x {PAGE_SIZE} U8 values;"
        f" logical payload {paged.storage_bytes} bytes. ##\n\n"
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
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_general_category(
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
    paged = _paged_bytes(encoded)
    matches = "\n".join(
        f"        {private_ids[category]} => {category}" for category in categories
    )
    return (
        f"## GENERATED from Unicode {version} General_Category and aliases. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## Named tags are stable Unicode aliases; byte values below are private storage IDs. ##\n"
        f"## default: {default}; layout: {len(paged.page_index)} U16 page ids + "
        f"{len(paged.pages)} x {PAGE_SIZE} U8 values; logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalGeneralCategory :: [].{\n"
        f"    GeneralCategory : [{', '.join(categories)}]\n\n"
        "    lookup : U32 -> GeneralCategory\n"
        "    lookup = |scalar| {\n"
        f"        if scalar > 0x10FFFF {{\n            {default}\n        }} else {{\n"
        "            page_id = page_index.get(scalar.shr_wrap(8).to_u64()) ?? 0\n"
        "            offset = page_id.to_u64() * 256 + scalar.bitwise_and(255).to_u64()\n"
        f"            category_from_u8(pages.get(offset) ?? {private_ids[default]})\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "category_from_u8 : U8 -> InternalGeneralCategory.GeneralCategory\n"
        "category_from_u8 = |value| {\n"
        "    match value {\n"
        f"{matches}\n"
        f"        _ => {default}\n"
        "    }\n"
        "}\n\n"
        "page_index : List(U16)\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_canonical_combining_class(
    version: str,
    records: Iterable[RangeRecord],
    default: int,
) -> str:
    encoded = bytearray((default,)) * (MAX_CODE_POINT + 1)
    for record in records:
        value = int(record.property)
        encoded[record.start : record.end + 1] = bytes((value,)) * (record.end - record.start + 1)
    paged = _paged_bytes(encoded)
    return (
        f"## GENERATED from Unicode {version} Canonical_Combining_Class and aliases. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## Returned U8 values are the stable, exact Unicode combining-class numbers. ##\n"
        f"## default: {default}; layout: {len(paged.page_index)} U16 page ids + "
        f"{len(paged.pages)} x {PAGE_SIZE} U8 values; logical payload {paged.storage_bytes} bytes. ##\n\n"
        "InternalCanonicalCombiningClass :: [].{\n"
        "    lookup : U32 -> U8\n"
        "    lookup = |scalar| {\n"
        f"        if scalar > 0x10FFFF {{\n            {default}\n        }} else {{\n"
        "            page_id = page_index.get(scalar.shr_wrap(8).to_u64()) ?? 0\n"
        "            offset = page_id.to_u64() * 256 + scalar.bitwise_and(255).to_u64()\n"
        f"            pages.get(offset) ?? {default}\n"
        "        }\n"
        "    }\n"
        "}\n\n"
        "page_index : List(U16)\n"
        f"page_index = {_roc_list(paged.page_index)}\n\n"
        "pages : List(U8)\n"
        f"pages = {_roc_list(paged.flat_pages)}\n"
    )


def render_emoji_data(
    version: str, emoji_version: str, records: Iterable[RangeRecord]
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
    paged = _paged_bytes(encoded)
    return (
        f"## GENERATED from Unicode {version} / Emoji {emoji_version}. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## Each binary Emoji property remains independently observable. ##\n"
        f"## default: all False; layout: {len(paged.page_index)} U16 page ids + "
        f"{len(paged.pages)} x {PAGE_SIZE} U8 bitsets; logical payload {paged.storage_bytes} bytes. ##\n\n"
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
        "        value = if scalar > 0x10FFFF {\n"
        "            0\n"
        "        } else {\n"
        "            page_id = page_index.get(scalar.shr_wrap(8).to_u64()) ?? 0\n"
        "            offset = page_id.to_u64() * 256 + scalar.bitwise_and(255).to_u64()\n"
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
        "page_index : List(U16)\n"
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


def render_property_aliases(
    version: str,
    canonical: CanonicalProperties,
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

    gc_matches = []
    for record in general_category_values:
        aliases = _unique_aliases((record.short, record.long, *record.aliases))
        gc_matches.append(
            f"        {record.identity} => [{', '.join(_roc_string(alias) for alias in aliases)}]"
        )
    ccc_matches = []
    for record in combining_class_values:
        aliases = _unique_aliases(
            (record.identity, record.short, record.long, *record.aliases)
        )
        ccc_matches.append(
            f"        {record.identity} => [{', '.join(_roc_string(alias) for alias in aliases)}]"
        )

    return (
        f"## GENERATED from Unicode {version} PropertyAliases and PropertyValueAliases. Run `python3 scripts/unicode_data.py generate`. ##\n"
        "## These canonical names are metadata for stable identities, never storage ordinals. ##\n"
        f"## metadata: {len(general_category_values)} scalar category identities; "
        f"{len(combining_class_values)} exact CCC identities. ##\n\n"
        "import InternalGeneralCategory\n\n"
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
        "    general_category_aliases : InternalGeneralCategory.GeneralCategory -> List(Str)\n"
        "    general_category_aliases = |category| {\n"
        "        match category {\n"
        + "\n".join(gc_matches)
        + "\n        }\n"
        "    }\n\n"
        "    canonical_combining_class_aliases : U8 -> List(Str)\n"
        "    canonical_combining_class_aliases = |value| {\n"
        "        match value {\n"
        + "\n".join(ccc_matches)
        + "\n            _ => []\n"
        "        }\n"
        "    }\n"
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


def render_legacy_emoji(version: str, emoji_version: str) -> str:
    return (
        f"## GENERATED from vendor/unicode/{version} (Emoji {emoji_version}). Run `python3 scripts/unicode_data.py generate`. ##\n"
        "import CodePoint\n"
        "import InternalEmojiData\n\n"
        "InternalEmoji :: {}.{\n"
        "    EMOJI : [Pictographic, Base, Modifier, Presentation, Component, Emoji]\n\n"
        "    from_cp : CodePoint -> Try(EMOJI, [NonEmojiCodePoint])\n"
        "    from_cp = |cp| {\n"
        "        u32 = cp.to_u32()\n\n"
        "        properties = InternalEmojiData.lookup(u32)\n"
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
        "    is_pictographic = |u32| InternalEmojiData.lookup(u32).extended_pictographic\n\n"
        "    is_base : U32 -> Bool\n"
        "    is_base = |u32| InternalEmojiData.lookup(u32).emoji_modifier_base\n\n"
        "    is_modifier : U32 -> Bool\n"
        "    is_modifier = |u32| InternalEmojiData.lookup(u32).emoji_modifier\n\n"
        "    is_presentation : U32 -> Bool\n"
        "    is_presentation = |u32| InternalEmojiData.lookup(u32).emoji_presentation\n\n"
        "    is_component : U32 -> Bool\n"
        "    is_component = |u32| InternalEmojiData.lookup(u32).emoji_component\n\n"
        "    is_emoji : U32 -> Bool\n"
        "    is_emoji = |u32| InternalEmojiData.lookup(u32).emoji\n"
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
    gcb, eaw, emoji, incb = load_property_data(manifest)
    canonical = load_canonical_properties(manifest)
    version = release_version(manifest, "unicode")
    emoji_version = release_version(manifest, "emoji")
    generators: dict[str, tuple[Callable[[], str], frozenset[str]]] = {
        "unicode-version": (lambda: render_unicode_version(version), frozenset()),
        "grapheme-data": (
            lambda: render_grapheme_data(version, gcb, incb, emoji),
            frozenset(("grapheme_break_property", "derived_core_properties", "emoji_data")),
        ),
        "legacy-grapheme-break": (
            lambda: render_gcb(version, gcb),
            frozenset(("grapheme_break_property",)),
        ),
        "east-asian-width": (
            lambda: render_eaw(version, eaw),
            frozenset(("east_asian_width",)),
        ),
        "emoji-properties": (
            lambda: render_emoji_data(version, emoji_version, emoji),
            frozenset(("emoji_data",)),
        ),
        "legacy-emoji": (
            lambda: render_legacy_emoji(version, emoji_version),
            frozenset(),
        ),
        "general-category": (
            lambda: render_general_category(
                version, canonical.general_category, canonical.general_category_default
            ),
            frozenset(("derived_general_category", "property_aliases", "property_value_aliases")),
        ),
        "canonical-combining-class": (
            lambda: render_canonical_combining_class(
                version,
                canonical.canonical_combining_class,
                canonical.canonical_combining_class_default,
            ),
            frozenset(("derived_combining_class", "property_aliases", "property_value_aliases")),
        ),
        "property-aliases": (
            lambda: render_property_aliases(version, canonical),
            frozenset(
                (
                    "property_aliases",
                    "property_value_aliases",
                    "derived_general_category",
                    "derived_combining_class",
                )
            ),
        ),
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
            render, required_sources = definition
            declared_sources = frozenset(
                _require_string_list(
                    artifact["sources"], f"manifest.artifacts.{name}.sources"
                )
            )
            missing_sources = required_sources - declared_sources
            if missing_sources:
                raise DataError(
                    f"manifest.artifacts.{name} omits generator sources {sorted(missing_sources)}"
                )
            rendered[_path_below_root(str(artifact["output"]), f"manifest.artifacts.{name}.output")] = render()
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
    load_canonical_properties(manifest)
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
