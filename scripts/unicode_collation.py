"""Generate the UCA element trie and explicit CLDR 49 tailoring overlays."""

from dataclasses import dataclass, field, replace
import re
import xml.etree.ElementTree as ET


CE_RE = re.compile(r"\[([.*])([0-9A-F]{4})\.([0-9A-F]{4})\.([0-9A-F]{4})\]")
UPPER_TERTIARY = {8, 9, 10, 11, 12, 14, 17, 18, 29}
RELATION_RE = re.compile(r"(<<<|<<|<)")


@dataclass(frozen=True)
class Element:
    primary: int
    secondary: int
    tertiary: int
    variable: bool = False
    case: int = 0
    cldr_variable: bool = False

    def encode(self):
        if (not 0 <= self.primary < 1 << 18
                or not 0 <= self.secondary < 1 << 16
                or not 0 <= self.tertiary < 1 << 16
                or self.case not in (0, 1, 2)
                or (self.variable or self.cldr_variable) and not self.primary):
            raise ValueError("collation element exceeds its packed width or variable-weight domain")
        return (
            (int(self.cldr_variable) << 53) | (self.case << 51) | (int(self.variable) << 50)
            | (self.primary << 32) | (self.secondary << 16) | self.tertiary
        )

    def levels(self):
        return self.primary, self.secondary, self.tertiary


@dataclass
class Node:
    children: dict[int, int] = field(default_factory=dict)
    terminal: int = 0


def point(token):
    if not re.fullmatch(r"[0-9A-F]{4,6}", token):
        raise ValueError(f"invalid UCA code point {token!r}")
    value = int(token, 16)
    if not 0 <= value <= 0x10FFFF:
        raise ValueError(f"out-of-domain UCA code point {token!r}")
    return value


def parse_ducet(text, version, *, cldr=False):
    entries = {}
    implicit = []
    got_version = False
    for line_number, line in enumerate(text.splitlines(), 1):
        body = re.split(r"[#%]", line, maxsplit=1)[0].strip()
        if not body:
            continue
        if body == f"@version {version}" and not got_version:
            got_version = True
            continue
        if not got_version:
            raise ValueError("allkeys version is absent or differs from the Unicode pin")
        if body.startswith("@implicitweights "):
            source, weight = map(str.strip, body.removeprefix("@implicitweights ").split(";"))
            start, end = map(point, source.split(".."))
            if start > end or any(start <= e and s <= end for s, e, _ in implicit):
                raise ValueError(f"invalid implicit range {source}")
            implicit.append((start, end, int(weight, 16)))
            continue
        source, payload = map(str.strip, body.split(";"))
        key = tuple(map(point, source.split()))
        if not key or key in entries or any(0xD800 <= cp <= 0xDFFF for cp in key):
            raise ValueError(f"invalid or repeated collation mapping at line {line_number}")
        matches = CE_RE.findall(payload)
        if not matches or CE_RE.sub("", payload).strip():
            raise ValueError(f"invalid collation elements at line {line_number}")
        elements = tuple(
            Element(
                int(primary, 16) * 4, int(secondary, 16) * 4, int(tertiary, 16) * 4,
                flag == "*", 2 if int(tertiary, 16) in UPPER_TERTIARY else 0,
            )
            for flag, primary, secondary, tertiary in matches
        )
        entries[key] = elements
    if not entries or (not implicit and not cldr):
        raise ValueError("empty UCA mapping or implicit range set")
    return entries, sorted(implicit)


def merge_cldr_root(ducet, cldr_root):
    if set(ducet) != set(cldr_root):
        raise ValueError("CLDR root repertory differs from the DUCET")
    merged = {}
    for key, sequence in ducet.items():
        cldr_sequence = [item for item in cldr_root[key] if any(item.levels())]
        cldr_iter = iter(cldr_sequence)
        updated = []
        for element in sequence:
            if not any(element.levels()):
                updated.append(element)
                continue
            counterpart = next(cldr_iter, None)
            if counterpart is None or element.levels() != counterpart.levels():
                raise ValueError(f"CLDR root changes nonzero DUCET weights for {key!r}")
            updated.append(replace(element, cldr_variable=counterpart.variable))
        if next(cldr_iter, None) is not None:
            raise ValueError(f"CLDR root inserts nonzero weights for {key!r}")
        merged[key] = tuple(updated)
    return merged


def tailored_case(ducet, key, weights):
    primary = [element for cp in key for element in ducet[(cp,)] if element.primary]
    primary_count = sum(bool(element.primary) for element in weights)
    case = [element.case for element in primary]
    if len(case) > primary_count:
        if primary_count == 0:
            raise ValueError("tailored primary removed while computing case")
        endings = set(case[primary_count - 1:])
        case[primary_count - 1:] = [endings.pop() if len(endings) == 1 else 1]
    case.extend([0] * (primary_count - len(case)))
    primary_case = iter(case)
    output = []
    for element in weights:
        if element.primary:
            assign = next(primary_case)
        else:
            assign = 2 if element.secondary == 0 and element.tertiary else 0
        output.append(replace(element, case=assign))
    return tuple(output)


def parse_cldr_collation(text, language, name):
    root = ET.fromstring(text)
    identity = root.find("identity/language")
    if identity is None or identity.get("type") != language:
        raise ValueError(f"invalid CLDR identification for {language}")
    records = [
        child
        for child in root.findall("collations/collation")
        if child.get("type") == name and not child.get("alt") and not child.get("draft")
    ]
    if len(records) != 1:
        raise ValueError(f"missing stable CLDR collation {language}/{name}")
    children = list(records[0])
    if len(children) != 1 or children[0].tag != "cr" or not children[0].text:
        raise ValueError(f"unsupported CLDR collation payload for {language}/{name}")
    return children[0].text


def tailor(ducet, rules, canonical):
    # These CLDR 49 profiles use reset, primary, secondary and tertiary
    # relations only. Unknown LDML syntax is a generator error.
    result = {}

    def scalars(text):
        if not text or any(c in "[]&<>=/'|*\\" or c.isspace() for c in text):
            raise ValueError(f"unsupported CLDR tailoring operand {text!r}")
        result = tuple(cp for char in text for cp in canonical.get(ord(char), (ord(char),)))
        if any(cp in canonical or 0xAC00 <= cp <= 0xD7A3 for cp in result):
            raise ValueError(f"CLDR operand has an unsupported nested canonical expansion: {text!r}")
        return result

    for source in rules.splitlines():
        line = source.split("#", 1)[0].strip()
        if not line:
            continue
        fields = RELATION_RE.split(line)
        if not line.startswith("&") or len(fields) < 3:
            raise ValueError(f"unsupported CLDR collation line {line!r}")
        anchor = scalars(fields[0][1:])
        current = tuple(element for point in anchor for element in ducet[(point,)])
        for relation, value in zip(fields[1::2], fields[2::2], strict=True):
            key = scalars(value)
            if key in result:
                raise ValueError(f"duplicate CLDR tailoring for {value!r}")
            previous = current[-1]
            if previous.primary == 0 or (relation != "<" and previous.secondary == 0):
                raise ValueError("CLDR reset must end in a collation element strong enough for the relation")
            first = key[0]
            source = ducet[(first,)][0]
            first_tertiary = source.tertiary
            if relation == "<":
                last = Element(previous.primary + 1, 0x20 * 4, first_tertiary)
            elif relation == "<<":
                # A secondary relation after a multi-element reset must be
                # ordered after the complete reset's secondary sequence.
                last = Element(previous.primary, previous.secondary + 1, first_tertiary)
            else:
                incremented = previous.tertiary + 1
                last = Element(previous.primary, previous.secondary, max(incremented, first_tertiary))
            current = current[:-1] + (last,)
            result[key] = tailored_case(ducet, key, current)
    return result


def add_mappings(nodes, pool, pool_index, entries, root):
    for key, elements in sorted(entries.items()):
        current = root
        for scalar in key:
            if scalar not in nodes[current].children:
                nodes[current].children[scalar] = len(nodes)
                nodes.append(Node())
            current = nodes[current].children[scalar]
        sequence = tuple(element.encode() for element in elements)
        if not 0 < len(sequence) < 256:
            raise ValueError("collation element sequence exceeds token count")
        if sequence not in pool_index:
            if len(pool) >= 1 << 56:
                raise ValueError("collation element pool exceeds its packed span")
            pool_index[sequence] = (len(pool) << 8) | len(sequence)
            pool.extend(sequence)
        nodes[current].terminal = pool_index[sequence]


def render(entries, overlays, implicit, han, version, cldr_version, render_list):
    nodes = [Node() for _ in range(len(overlays) + 2)]
    pool, pool_index = [], {}
    add_mappings(nodes, pool, pool_index, entries, 1)
    for index, overlay in enumerate(overlays, 2):
        add_mappings(nodes, pool, pool_index, overlay, index)
    if len(nodes) >= 1 << 32:
        raise ValueError("collation trie exceeds U32 node indices")
    starts, keys, values = [0], [], []
    for node in nodes:
        for key, index in sorted(node.children.items()):
            keys.append(key)
            values.append(index)
        starts.append(len(keys))
    offsets = [node.terminal for node in nodes]
    digit_group = entries[(0x30,)][0].primary
    output = [
        "# Generated by scripts/unicode_data.py. Do not edit.",
        f"# Unicode {version} DUCET and CLDR {cldr_version} profile collation elements.",
        "InternalCollationData :: [].{",
        f'    cldr_version : Str\n    cldr_version = "{cldr_version}"',
        f"    numeric_primary : U32\n    numeric_primary = {digit_group - 1}",
        "    child : U32, U32 -> U32",
        "    child = |state, code| {",
        "        if state == 0 {",
        "            return 0",
        "        }",
        "        at = state.to_u64()",
        f"        if state < {len(overlays) + 2} and code < 128 {{",
        "            return ascii_children.get(at * 128 + code.to_u64()) ?? 0",
        "        }",
        "        var $low = starts.get(at) ?? return 0",
        "        var $high = starts.get(at + 1) ?? return 0",
        "        while $low < $high {",
        "            mid = $low + ($high - $low) // 2",
        "            candidate = keys.get(mid) ?? return 0",
        "            if code < candidate {",
        "                $high = mid",
        "            } else if code > candidate {",
        "                $low = mid + 1",
        "            } else {",
        "                return destinations.get(mid) ?? 0",
        "            }",
        "        }",
        "        0",
        "    }",
        "    terminal : U32 -> U64",
        "    terminal = |state| terminals.get(state.to_u64()) ?? 0",
        "    element : U64 -> Try(U64, [OutOfBounds])",
        "    element = |at| elements.get(at)",
        "    implicit : U32, Bool -> { first : U32, second : U32 }",
        "    implicit = |code, assigned| {",
    ]
    bases = {}
    for start, _, base in implicit:
        bases[base] = min(start, bases.get(base, start))
    for start, end, base in implicit:
        output.extend([
            f"        if assigned and {start} <= code and code <= {end} {{",
            f"            return {{ first: {base} * 4, second: ((code - {bases[base]}).bitwise_or(0x8000)) * 4 }}",
            "        }",
        ])
    for start, end in han:
        base = 0xFB40 if 0x4E00 <= start <= 0x9FFF or 0xF900 <= start <= 0xFAFF else 0xFB80
        output.extend([
            f"        if {start} <= code and code <= {end} {{",
            f"            return two_units(code, {base})",
            "        }",
        ])
    output += [
        "        two_units(code, 0xFBC0)",
        "    }", "}", "",
        "two_units = |code, base| {",
        "    { first: (base + code.shr_wrap(15)) * 4, second: code.bitwise_and(0x7FFF).bitwise_or(0x8000) * 4 }",
        "}", "",
    ]
    for name, t, values in (
        (
            "ascii_children", "U32",
            (nodes[index].children.get(cp, 0) for index in range(len(overlays) + 2) for cp in range(128)),
        ),
        ("starts", "U64", starts), ("keys", "U32", keys),
        ("destinations", "U32", values), ("terminals", "U64", offsets),
        ("elements", "U64", pool),
    ):
        output.extend([f"{name} : List({t})", f"{name} = {render_list(values)}", ""])
    return "\n".join(output)


def parse_short_tests(text, version):
    for name in ("UCA", "UCD"):
        if f"# {name} Version: {version}" not in text.splitlines()[:7]:
            raise ValueError(f"unexpected {name} collation-test version")
    for line in text.splitlines():
        entry = line.split("#", 1)[0].strip()
        if entry:
            codes = tuple(map(point, entry.split()))
            if not any(0xD800 <= cp <= 0xDFFF for cp in codes):
                yield codes
