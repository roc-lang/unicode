#!/usr/bin/env python3
"""Opt-in Unicode 17 Case differential test against ICU4J 78.3.

The normal offline suite never calls this script. Supply ICU4J_JAR (or
--icu4j-jar) for a local artifact, or pass --download to fetch the pinned jar
into .roc-unicode-tmp/case-icu/ after its SHA-256 is verified.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import subprocess
import sys
import urllib.request
from pathlib import Path

import unicode_data


ROOT = Path(__file__).resolve().parents[1]
TMP = ROOT / ".roc-unicode-tmp" / "case-icu"
JAR_NAME = "icu4j-78.3.jar"
JAR_URL = "https://repo.maven.apache.org/maven2/com/ibm/icu/icu4j/78.3/icu4j-78.3.jar"
JAR_SHA256 = "e962c1758d9659ea1e1fbab99c58683f654d304e1126ace19aaabfe39e0edb25"
EXPECTED_ICU_PREFIX = "78.3"
EXPECTED_UNICODE_PREFIX = "17.0"
# ICU's titlecase API is not Unicode Core R3 for an isolated U+0345: it leaves
# the combining ypogegrammeni unchanged, while R3 title-maps its first cased
# scalar. The oracle has no switch for R3, so this one scalar/title combination
# is reported as an intentional non-comparison rather than accepted as equal.
ICU_NON_R3_TITLE_SOURCES = frozenset({0x0345})
ICU_NON_R3_TITLE_FOCUSED = frozenset({13})


class Failure(RuntimeError):
    pass


ROC_EMITTER = r'''app [run!] {
	pf: platform "../../tests/platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Case
import unicode.Scalar

run! : Str => Str
run! = |input| {
	lines = input.split_on("\n").drop_if(|line| line == "")
	lines.fold("", |output, line| {
		row = run_record(line)
		if output == "" row else "${output}\n${row}"
	})
}

run_record = |line| match line.split_on("\t") {
	[id, operation, profile, scalars] => {
		source = parse_source(scalars) ?? return "${id}\tERR\tbad source"
		match invoke(operation, profile, source) {
			Err(_) => "${id}\tERR\tCase error"
			Ok(result) => "${id}\tOK\t${wire(Case.result_text(result))}"
		}
	}
	_ => "0\tERR\tbad record"
}

parse_source = |text| {
	if text == "_" {
		Ok("")
	} else {
		parts = text.split_on(",").map(|item| U32.from_str("0x${item}"))
		values = keep_oks(parts)?
		chars = keep_oks(values.map(|value| scalar_to_str(value) |> Try.map_err(|_| BadNumStr)))?
		Ok(Str.join_with(chars, ""))
	}
}

scalar_to_str = |value| {
	scalar = Scalar.from_u32(value) |> Try.map_err(|_| {})?
	Scalar.to_str(scalar) |> Try.map_err(|_| {})
}

keep_oks = |items| items.fold(
	Ok([]),
	|state, item| {
		values = state?
		value = item?
		Ok(values.append(value))
	},
)

invoke = |operation, profile, source| {
	limits = Case.unlimited_limits
	match (operation, profile) {
		("lower", "default") => Case.to_lower(source, Case.unicode_default, limits)
		("lower", "turkic") => Case.to_lower(source, Case.turkic, limits)
		("lower", "lithuanian") => Case.to_lower(source, Case.lithuanian, limits)
		("upper", "default") => Case.to_upper(source, Case.unicode_default, limits)
		("upper", "turkic") => Case.to_upper(source, Case.turkic, limits)
		("upper", "lithuanian") => Case.to_upper(source, Case.lithuanian, limits)
		("title", "default") => Case.to_title(source, Case.unicode_default, limits)
		("title", "turkic") => Case.to_title(source, Case.turkic, limits)
		("title", "lithuanian") => Case.to_title(source, Case.lithuanian, limits)
		("fold", "full") => Case.fold(source, Case.full, limits)
		_ => Case.to_lower(source, Case.unicode_default, limits)
	}
}

wire = |text| {
	encoded = Str.join_with(text.to_utf8().map(|byte| byte.to_str()), ",")
	if encoded == "" "_" else encoded
}
'''


JAVA_ORACLE = r'''import com.ibm.icu.lang.UCharacter;
import com.ibm.icu.util.VersionInfo;
import com.ibm.icu.text.BreakIterator;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

public final class CaseIcuOracle {
  static String locale(String profile) { return profile.equals("turkic") ? "tr" : profile.equals("lithuanian") ? "lt" : ""; }
  static Locale toLocale(String profile) { return profile.equals("turkic") ? Locale.forLanguageTag("tr") : profile.equals("lithuanian") ? Locale.forLanguageTag("lt") : Locale.ROOT; }
  static String decode(String text) {
    if (text.equals("_")) return "";
    StringBuilder out = new StringBuilder();
    for (String part : text.split(",")) out.appendCodePoint(Integer.parseInt(part, 16));
    return out.toString();
  }
  static String wire(String text) {
    byte[] bytes = text.getBytes(StandardCharsets.UTF_8);
    if (bytes.length == 0) return "_";
    StringBuilder out = new StringBuilder();
    for (int i = 0; i < bytes.length; i++) { if (i != 0) out.append(','); out.append(Byte.toUnsignedInt(bytes[i])); }
    return out.toString();
  }
  static String apply(String operation, String profile, String source) {
    Locale locale = toLocale(profile);
    if (operation.equals("lower")) return UCharacter.toLowerCase(locale, source);
    if (operation.equals("upper")) return UCharacter.toUpperCase(locale, source);
    if (operation.equals("title")) return UCharacter.toTitleCase(locale, source, BreakIterator.getWordInstance(locale), 0);
    if (operation.equals("fold") && profile.equals("full")) return UCharacter.foldCase(source, UCharacter.FOLD_CASE_DEFAULT);
    throw new IllegalArgumentException("unsupported operation/profile");
  }
  public static void main(String[] args) throws Exception {
    if (args.length == 1 && args[0].equals("--version")) {
      System.out.println("ICU=" + VersionInfo.ICU_VERSION + " UNICODE=" + UCharacter.getUnicodeVersion()); return;
    }
    try (java.io.BufferedReader in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in, StandardCharsets.UTF_8))) {
      for (String line; (line = in.readLine()) != null;) {
        if (line.isEmpty()) continue;
        String[] fields = line.split("\\t", -1);
        if (fields.length != 4) throw new IllegalArgumentException("bad record");
        System.out.println(fields[0] + "\tOK\t" + wire(apply(fields[1], fields[2], decode(fields[3]))));
      }
    }
  }
}
'''


def run(args: list[str], *, input_text: str | None = None, cwd: Path = ROOT, announce: bool = True) -> subprocess.CompletedProcess[str]:
    if announce:
        print("+", " ".join(args))
    completed = subprocess.run(args, cwd=cwd, input=input_text, text=True, encoding="utf-8", errors="strict", stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if completed.returncode:
        raise Failure(f"command failed ({completed.returncode}): {' '.join(args)}\n{(completed.stderr or completed.stdout).strip()}")
    return completed


def checked_jar(path: Path) -> Path:
    path = path.resolve()
    if not path.is_file():
        raise Failure(f"ICU4J jar not found: {path}")
    actual = hashlib.sha256(path.read_bytes()).hexdigest()
    if actual != JAR_SHA256:
        raise Failure(f"ICU4J SHA-256 mismatch for {path}: expected {JAR_SHA256}, got {actual}")
    return path


def verify_pinned_roc(roc: str) -> None:
    observed = run([roc, "version"]).stdout.strip()
    pinned = (ROOT / ".roc-version").read_text(encoding="utf-8").strip()
    pinned_revision = pinned.rsplit("-", 1)[-1]
    if pinned not in observed and pinned_revision not in observed:
        raise Failure(f"repository requires {pinned}, got {observed!r}")


def resolve_jar(args: argparse.Namespace) -> Path:
    configured = args.icu4j_jar or os.environ.get("ICU4J_JAR")
    if configured:
        return checked_jar(Path(configured))
    cache = TMP / JAR_NAME
    if cache.is_file():
        return checked_jar(cache)
    if not args.download:
        raise Failure("set ICU4J_JAR/--icu4j-jar, or pass --download to fetch the pinned ICU4J 78.3 jar")
    TMP.mkdir(parents=True, exist_ok=True)
    print(f"+ download {JAR_URL}")
    with urllib.request.urlopen(JAR_URL, timeout=60) as response:
        payload = response.read()
    cache.write_bytes(payload)
    return checked_jar(cache)


def scalar_wire(scalars: tuple[int, ...]) -> str:
    return "_" if not scalars else ",".join(f"{scalar:X}" for scalar in scalars)


def corpus() -> tuple[list[tuple[str, str, str, tuple[int, ...]]], int]:
    manifest = unicode_data.load_manifest()
    canonical = unicode_data.load_canonical_properties(manifest)
    data = unicode_data.load_case_data(manifest, canonical)
    sources = {item.source for group in (data.simple_lower, data.simple_upper, data.simple_title) for item in group}
    sources.update(item.source for item in data.special)
    sources.update(item.source for item in data.folding)
    focused = (
        "I", "İ", "I\u0307", "I\u0323\u0307", "I\u0301", "J\u0300", "\u012E\u0301",
        "i\u0307", "\u0391\u03A3", "\u0391\u03A3\u0301", "\u0391\u0345", "foo bar", "foo's bar",
        "123ABC's XYZ", "\u0301ABC", "\U00010400\U00010428",
    )
    records: list[tuple[str, str, str, tuple[int, ...]]] = []
    skipped_title = 0
    for scalar in sorted(sources):
        text = (scalar,)
        for operation in ("lower", "upper", "title"):
            for profile in ("default", "turkic", "lithuanian"):
                if operation == "title" and scalar in ICU_NON_R3_TITLE_SOURCES:
                    skipped_title += 1
                    continue
                records.append((f"scalar-{scalar:06X}-{operation}-{profile}", operation, profile, text))
        records.append((f"scalar-{scalar:06X}-fold-full", "fold", "full", text))
    for index, text in enumerate(focused):
        scalars = tuple(map(ord, text))
        for operation in ("lower", "upper", "title"):
            for profile in ("default", "turkic", "lithuanian"):
                if operation == "title" and index in ICU_NON_R3_TITLE_FOCUSED:
                    skipped_title += 1
                    continue
                records.append((f"focused-{index}-{operation}-{profile}", operation, profile, scalars))
        records.append((f"focused-{index}-fold-full", "fold", "full", scalars))
    return records, skipped_title


def protocol(records: list[tuple[str, str, str, tuple[int, ...]]]) -> str:
    return "\n".join(f"{case_id}\t{operation}\t{profile}\t{scalar_wire(scalars)}" for case_id, operation, profile, scalars in records) + "\n"


def batches(items: list[tuple[str, str, str, tuple[int, ...]]], size: int = 250) -> list[list[tuple[str, str, str, tuple[int, ...]]]]:
    return [items[index : index + size] for index in range(0, len(items), size)]


def parse_output(label: str, text: str, expected: int) -> dict[str, bytes]:
    rows: dict[str, bytes] = {}
    for line in text.splitlines():
        fields = line.split("\t")
        if len(fields) != 3 or fields[1] != "OK":
            raise Failure(f"{label} emitted malformed row: {line!r}")
        if fields[0] in rows:
            raise Failure(f"{label} emitted duplicate case id {fields[0]}")
        rows[fields[0]] = b"" if fields[2] == "_" else bytes(int(item) for item in fields[2].split(","))
    if len(rows) != expected:
        raise Failure(f"{label} emitted {len(rows)} rows, expected {expected}")
    return rows


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--icu4j-jar", type=Path)
    parser.add_argument("--download", action="store_true")
    parser.add_argument("--roc", default="roc")
    parser.add_argument("--java", default="java")
    parser.add_argument("--javac", default="javac")
    args = parser.parse_args(argv)
    try:
        verify_pinned_roc(args.roc)
        jar = resolve_jar(args)
        TMP.mkdir(parents=True, exist_ok=True)
        java_source = TMP / "CaseIcuOracle.java"
        roc_source = TMP / "case-icu.roc"
        java_source.write_text(JAVA_ORACLE, encoding="utf-8")
        roc_source.write_text(ROC_EMITTER, encoding="utf-8")
        run([args.javac, "-cp", str(jar), str(java_source)], cwd=TMP)
        version = run([args.java, "-cp", f"{jar}{os.pathsep}{TMP}", "CaseIcuOracle", "--version"], cwd=TMP).stdout.strip()
        if f"ICU={EXPECTED_ICU_PREFIX}" not in version or f"UNICODE={EXPECTED_UNICODE_PREFIX}" not in version:
            raise Failure(f"ICU4J version mismatch: expected ICU {EXPECTED_ICU_PREFIX}/Unicode {EXPECTED_UNICODE_PREFIX}, got {version!r}")
        run(["zig", "build", "--build-file", "tests/platform/build.zig", "native", "-Doptimize=ReleaseFast"])
        emitter = TMP / ("case-icu" + (".exe" if os.name == "nt" else ""))
        run([args.roc, "build", str(roc_source), "--opt=speed", f"--output={emitter}", "--no-cache"])
        records, skipped_title = corpus()
        roc_rows: dict[str, bytes] = {}
        icu_rows: dict[str, bytes] = {}
        for batch in batches(records):
            input_text = protocol(batch)
            roc_rows.update(parse_output("Roc", run([str(emitter)], input_text=input_text, cwd=TMP, announce=False).stdout, len(batch)))
            icu_rows.update(parse_output("ICU4J", run([args.java, "-cp", f"{jar}{os.pathsep}{TMP}", "CaseIcuOracle"], input_text=input_text, cwd=TMP, announce=False).stdout, len(batch)))
        if len(roc_rows) != len(records) or len(icu_rows) != len(records):
            raise Failure("differential batch IDs did not remain unique")
        for case_id, expected in icu_rows.items():
            actual = roc_rows.get(case_id)
            if actual != expected:
                raise Failure(f"ICU4J mismatch {case_id}: roc={actual!r} icu={expected!r}")
        print(f"PASS case-icu: {len(records)} comparisons ({len({item[3] for item in records})} deterministic sources; {skipped_title} explicit ICU non-R3 title skips; title uses ICU word breaks)")
        return 0
    except (Failure, unicode_data.DataError, OSError, ValueError, subprocess.TimeoutExpired) as err:
        print(f"FAIL case-icu: {err}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
