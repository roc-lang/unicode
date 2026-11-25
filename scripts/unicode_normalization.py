"""Canonical Unicode normalization tables and independent conformance rows."""

from dataclasses import dataclass
from functools import cache
import re


MAX_SCALAR = 0x10FFFF
QUICK_CHECK_FORMS = ("NFC_QC", "NFD_QC", "NFKC_QC", "NFKD_QC")
PROPERTIES = ("Full_Composition_Exclusion", *QUICK_CHECK_FORMS, "NFKC_CF")
COMPATIBILITY_TAGS = {
    "<font>", "<noBreak>", "<initial>", "<medial>", "<final>", "<isolated>",
    "<circle>", "<super>", "<sub>", "<vertical>", "<wide>", "<narrow>",
    "<small>", "<square>", "<fraction>", "<compat>",
}
OTHER_PROPERTIES = {
    "Changes_When_NFKC_Casefolded", "Expands_On_NFC", "Expands_On_NFD",
    "Expands_On_NFKC", "Expands_On_NFKD", "FC_NFKC", "NFKC_SCF",
}


@dataclass(frozen=True)
class NormalizationData:
    canonical: dict[int, tuple[int, ...]]
    compatibility: dict[int, tuple[int, ...]]
    composition: dict[int, int]
    quick_checks: dict[int, int]
    identifier_fold: dict[int, tuple[int, ...]]


def code_point(token):
    if not re.fullmatch(r"[0-9A-F]{4,6}", token):
        raise ValueError(f"invalid code point {token!r}")
    number = int(token, 16)
    if number > MAX_SCALAR:
        raise ValueError(f"out-of-domain code point {token!r}")
    return number


def scalar(token):
    number = code_point(token)
    if 0xD800 <= number <= 0xDFFF:
        raise ValueError(f"surrogate {token!r} in scalar list")
    return number


def scalar_range(token):
    bounds = token.split("..")
    if len(bounds) == 1:
        start = end = scalar(bounds[0])
    elif len(bounds) == 2:
        start, end = map(scalar, bounds)
    else:
        raise ValueError(f"malformed range {token!r}")
    if start > end or (start < 0xD800 and end > 0xDFFF):
        raise ValueError(f"invalid scalar range {token!r}")
    return range(start, end + 1)


def parse_decompositions(text):
    direct = {}
    previous = -1
    for line in text.splitlines():
        fields = line.split(";")
        if len(fields) != 15:
            raise ValueError(f"malformed UnicodeData record {line!r}")
        cp = code_point(fields[0])
        if cp <= previous:
            raise ValueError("UnicodeData records are not strictly ascending")
        previous = cp
        values = fields[5].split()
        if not values:
            continue
        scalar(fields[0])
        tagged = values[0].startswith("<")
        if tagged and values.pop(0) not in COMPATIBILITY_TAGS:
            raise ValueError(f"unknown compatibility tag for U+{cp:04X}")
        if not values or (not tagged and len(values) > 2):
            raise ValueError(f"invalid decomposition length for U+{cp:04X}")
        direct[cp] = (tagged, tuple(map(scalar, values)))
    return direct


def parse_properties(text):
    seen = {name: set() for name in PROPERTIES}
    defaults = {}
    quick_checks = {}
    fold = {}
    for line in text.splitlines():
        if line.startswith("# @missing:"):
            default = tuple(part.strip() for part in line.split(":", 1)[1].split(";"))
            if len(default) != 3:
                raise ValueError(f"malformed normalization default {line!r}")
            domain, name, value = default
            if name in PROPERTIES:
                if domain != "0000..10FFFF" or name in defaults:
                    raise ValueError(f"conflicting default for {name}")
                defaults[name] = value
            elif name not in OTHER_PROPERTIES:
                raise ValueError(f"unknown normalization default {name}")
        body = line.split("#", 1)[0].strip()
        if not body:
            continue
        fields = [field.strip() for field in body.split(";")]
        if len(fields) < 2:
            raise ValueError(f"malformed normalization property {line!r}")
        span, name, *value = fields
        if name in OTHER_PROPERTIES:
            continue
        if name not in seen:
            raise ValueError(f"unknown normalization property {name}")
        values = scalar_range(span)
        if any(cp in seen[name] for cp in values):
            raise ValueError(f"overlapping records for {name} at {span}")
        seen[name].update(values)
        if name == "Full_Composition_Exclusion":
            if value:
                raise ValueError(f"unexpected value for {name}")
        elif name == "NFKC_CF":
            if len(value) != 1:
                raise ValueError(f"missing mapping for {name}")
            mapped = tuple(map(scalar, value[0].split()))
            fold.update((cp, mapped) for cp in values)
        else:
            if value == ["N"]:
                flag = 1
            elif value == ["M"] and name in ("NFC_QC", "NFKC_QC"):
                flag = 2
            else:
                raise ValueError(f"invalid {name} value {value!r}")
            shift = QUICK_CHECK_FORMS.index(name) * 2
            for cp in values:
                quick_checks[cp] = quick_checks.get(cp, 0) | flag << shift
    if defaults != ({name: "Yes" for name in QUICK_CHECK_FORMS} | {"NFKC_CF": "<code point>"}):
        raise ValueError("normalization property defaults changed")
    return seen["Full_Composition_Exclusion"], quick_checks, fold


def parse_data(unicode_data, properties):
    direct = parse_decompositions(unicode_data)
    exclusion, checks, fold = parse_properties(properties)
    active = set()

    @cache
    def expand(cp, compatibility):
        if 0xAC00 <= cp < 0xAC00 + 11172:
            index = cp - 0xAC00
            l = 0x1100 + index // 588
            v = 0x1161 + index % 588 // 28
            t = index % 28
            return (l, v, 0x11A7 + t) if t else (l, v)
        if cp not in direct or (direct[cp][0] and not compatibility):
            return (cp,)
        if cp in active:
            raise ValueError(f"cyclic decomposition at U+{cp:04X}")
        active.add(cp)
        expanded = tuple(item for member in direct[cp][1] for item in expand(member, compatibility))
        active.remove(cp)
        return expanded

    canonical = {cp: expand(cp, False) for cp, (tagged, _) in direct.items() if not tagged}
    compatibility = {cp: expand(cp, True) for cp in direct if expand(cp, True) != canonical.get(cp)}
    compositions = {}
    for cp, (tagged, members) in direct.items():
        if tagged or cp in exclusion:
            continue
        if len(members) != 2:
            raise ValueError(f"unexcluded singleton composition U+{cp:04X}")
        first, second = members
        key = (first << 21) | second
        if key in compositions:
            raise ValueError(f"duplicate canonical composition {members!r}")
        compositions[key] = cp
    return NormalizationData(canonical, compatibility, compositions, checks, fold)


def conformance_rows(text, *, with_parts=False):
    parts = []
    for line in text.splitlines():
        body = line.split("#", 1)[0].strip()
        if not body:
            continue
        if body.startswith("@"):
            if body != f"@Part{len(parts)}":
                raise ValueError(f"unexpected normalization test section {body!r}")
            parts.append(body)
            continue
        fields = [field.strip() for field in body.split(";")]
        if not parts or len(fields) != 6 or fields[-1] or any(not field for field in fields[:5]):
            raise ValueError(f"malformed normalization test {line!r}")
        columns = tuple(tuple(map(scalar, field.split())) for field in fields[:5])
        yield (len(parts) - 1, columns) if with_parts else columns
    if len(parts) < 4:
        raise ValueError("missing normalization test sections")


def render(data, version, render_list):
    # Mapping tokens use U64 throughout the index and U32 in the shared
    # sequence pool. Length zero is also a real NFKC_CF mapping (removal).
    sequences = []
    slots = {}

    def token(sequence):
        if sequence not in slots:
            start = len(sequences)
            if len(sequence) > 0xFF or start >= 1 << 56:
                raise ValueError("normalization mapping exceeds its encoding")
            slots[sequence] = (start << 8) | len(sequence)
            sequences.extend(sequence)
        return slots[sequence]

    tables = {
        "canonical": {cp: token(seq) for cp, seq in sorted(data.canonical.items())},
        "compatibility": {cp: token(seq) for cp, seq in sorted(data.compatibility.items())},
        "fold": {cp: token(seq) for cp, seq in sorted(data.identifier_fold.items())},
        "composition": data.composition,
        "quick_check": data.quick_checks,
    }
    lines = [
        "# Generated by scripts/unicode_data.py. Do not edit.",
        f"# Unicode {version} normalization data from UnicodeData and DerivedNormalizationProps.",
        "InternalNormalizationData :: [].{",
    ]
    for name, mapping in tables.items():
        lines.append(f"    {name} : U64 -> [None, Some(U64)]")
        if name in ("canonical", "compatibility", "quick_check"):
            if any(cp < 128 for cp in mapping):
                raise ValueError(f"{name} requires an explicit ASCII mapping")
            lines.append(
                f"    {name} = |key| if key < 128 {{ None }} else {{ lookup(key, {name}_keys, {name}_values) }}"
            )
        elif name == "fold":
            lines.extend([
                "    fold = |key| {",
                "        if key < 128 {",
                "            entry = ascii_fold.get(key) ?? 0",
                "            if entry == 0 { None } else { Some(entry - 1) }",
                "        } else {",
                "            lookup(key, fold_keys, fold_values)",
                "        }",
                "    }",
            ])
        else:
            lines.append(f"    {name} = |key| lookup(key, {name}_keys, {name}_values)")
    lines.extend([
        "    mapping_scalar : U64 -> Try(U32, [OutOfBounds])",
        "    mapping_scalar = |offset| sequences.get(offset)",
        "}",
        "",
        "lookup : U64, List(U64), List(U64) -> [None, Some(U64)]",
        "lookup = |key, keys, values| {",
        "    var $low = 0.U64",
        "    var $high = keys.len()",
        "    while $low < $high {",
        "        at = $low + ($high - $low) // 2",
        "        current = keys.get(at) ?? return None",
        "        if key < current {",
        "            $high = at",
        "        } else if key > current {",
        "            $low = at + 1",
        "        } else {",
        "            return match values.get(at) {",
        "                Ok(value) => Some(value)",
        "                Err(_) => None",
        "            }",
        "        }",
        "    }",
        "    None",
        "}",
        "",
    ])
    lines.extend([
        "ascii_fold : List(U64)",
        f"ascii_fold = {render_list(tables['fold'].get(cp, -1) + 1 for cp in range(128))}",
        "",
    ])
    for name, mapping in tables.items():
        keys = sorted(cp for cp in mapping if name != "fold" or cp >= 128)
        lines.extend([
            f"{name}_keys : List(U64)",
            f"{name}_keys = {render_list(keys)}",
            f"{name}_values : List(U64)",
            f"{name}_values = {render_list(mapping[key] for key in keys)}",
            "",
        ])
    lines.extend(["sequences : List(U32)", f"sequences = {render_list(sequences)}", ""])
    return "\n".join(lines)
