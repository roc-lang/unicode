app [run!] {
	pf: platform "../platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Collation
import unicode.Locale

run! : Str => Str
run! = |_| ""

limits : Collation.Limits
limits = {
	normalization: { max_input_bytes: 256, max_intermediate_scalars: 256, max_output_bytes: 1024 },
	max_elements: 1024,
	max_numeric_digits: 128,
	max_key_bytes: 16384,
}

# UCA §7.2. Cyrillic И + breve forms a root contraction. U+0327 (202)
# can be skipped for U+0306 (230), whereas U+0301 (230) blocks U+0306.
expect {
	root = Collation.collator(Locale.root, { strength: Primary })?
	Collation.compare(root, "И\u(0327)\u(0306)", "Й", limits) == Ok(Eq) and
		Collation.compare(root, "И\u(0301)\u(0306)", "И", limits) == Ok(Eq) and
			Collation.compare(root, "И\u(0301)\u(0306)", "Й", limits) == Ok(Lt)
}

# The skipped cedilla is still present at the secondary level. Canonically
# equivalent inputs get identical keys even under the Identical strength.
expect {
	options : Collation.Options
	options = { strength: Identical }
	root = Collation.collator(Locale.root, options)?
	Collation.sort_key(root, "И\u(0327)\u(0306)", limits) == Collation.sort_key(root, "Й\u(0327)", limits) and
		Collation.compare(root, "Й", "Й\u(0327)", limits) == Ok(Lt) and
			Collation.compare(root, "a\u(0000)", "a", limits) == Ok(Gt) and
				Collation.compare(root, "\u(FFFE)", "\u(0000)\u(FFFE)", limits) == Ok(Lt)
}

# CLDR Spanish n/ñ and traditional ch/ll, plus German phonebook expansions.
expect {
	options : Collation.Options
	options = { strength: Primary }
	root = Collation.collator(Locale.root, options)?
	spanish = Collation.collator(Locale.parse("es")?, options)?
	traditional = Collation.collator(Locale.parse("es-u-co-trad")?, options)?
	de = Collation.collator(Locale.parse("de")?, options)?
	phonebook = Collation.collator(Locale.parse("de-u-co-phonebk")?, options)?
	Collation.compare(root, "n", "ñ", limits) == Ok(Eq) and
		Collation.compare(spanish, "n", "ñ", limits) == Ok(Lt) and
			Collation.compare(spanish, "ñ", "o", limits) == Ok(Lt) and
				Collation.compare(spanish, "cz", "ch", limits) == Ok(Gt) and
					Collation.compare(traditional, "cz", "ch", limits) == Ok(Lt) and
						Collation.compare(traditional, "lz", "ll", limits) == Ok(Lt) and
							Collation.compare(de, "ä", "ad", limits) == Ok(Lt) and
								Collation.compare(phonebook, "ä", "ad", limits) == Ok(Gt)
}

# A variable and its following ignorable disappear through tertiary. The
# shifted quaternary retains the variable. CLDR retains symbols like U+2620.
expect {
	options : Collation.Options
	options = { alternate: Shifted }
	root = Collation.collator(Locale.root, options)?
	fourth = Collation.collator(Locale.root, { alternate: Shifted, strength: Quaternary })?
	spanish = Collation.collator(Locale.parse("es")?, options)?
	Collation.compare(root, "a \u(0300)b", "ab", limits) == Ok(Eq) and
		Collation.compare(root, "a\u(0300)b", "ab", limits) == Ok(Gt) and
			Collation.compare(fourth, "a b", "ab", limits) == Ok(Lt) and
				Collation.compare(root, "a☠b", "ab", limits) == Ok(Eq) and
					Collation.compare(spanish, "a☠b", "ab", limits) != Ok(Eq)
}

# Different scripts and leading zeroes can spell the same numeric value.
# The count/ordinal encoding also handles values beyond machine integers.
expect {
	options : Collation.Options
	options = { digit_order: ByNumericValue }
	root = Collation.collator(Locale.root, options)?
	standard = Collation.collator(Locale.root, {})?
	Collation.compare(standard, "file9", "file10", limits) == Ok(Gt) and
		Collation.compare(root, "file9", "file10", limits) == Ok(Lt) and
			Collation.compare(root, "file09", "file٩", limits) == Ok(Eq) and
				Collation.compare(root, "99999999999999999999", "100000000000000000000", limits) == Ok(Lt) and
					Collation.compare(root, "file⑨", "file9", limits) == Ok(Gt) and
						Collation.sort_key(root, "001", { ..limits, max_numeric_digits: 2 }) ==
							Err(LimitExceeded({ resource: NumericDigits, limit: 2, required: 3 }))
}

expect {
	root = Collation.collator(Locale.root, {})?
	upper = Collation.collator(Locale.root, { case_first: UpperFirst })?
	lower = Collation.collator(Locale.root, { case_first: LowerFirst })?
	traditional = Collation.collator(Locale.parse("es-u-co-trad")?, { case_first: UpperFirst })?
	Collation.compare(root, "a", "A", limits) == Ok(Lt) and
		Collation.compare(upper, "A", "a", limits) == Ok(Lt) and
			Collation.compare(lower, "a", "A", limits) == Ok(Lt) and
				Collation.compare(traditional, "CH", "Ch", limits) == Ok(Lt) and
					Collation.compare(traditional, "Ch", "ch", limits) == Ok(Lt)
}

expect {
	root = Collation.collator(Locale.root, { strength: Primary })?
	per_text = { ..limits, max_key_bytes: 4 }
	sort_limits = { per_text, max_items: 3, max_total_key_bytes: 12 }
	Collation.sort(root, ["b", "A", "a"], sort_limits) == Ok(["A", "a", "b"]) and
		Collation.sort(root, ["b", "A", "a"], { ..sort_limits, max_items: 2 }) ==
			Err(LimitExceeded({ resource: SortItems, limit: 2, required: 3 })) and
			Collation.sort(root, ["b", "A", "a"], { ..sort_limits, max_total_key_bytes: 11 }) ==
				Err(LimitExceeded({ resource: SortKeyBytes, limit: 11, required: 12 })) and
				Collation.sort_key(root, "a", { ..limits, max_elements: 0 }) ==
					Err(LimitExceeded({ resource: Elements, limit: 0, required: 1 })) and
					Collation.sort_key(root, "a", { ..limits, max_key_bytes: 3 }) ==
						Err(LimitExceeded({ resource: KeyBytes, limit: 3, required: 4 }))
}

expect {
	unsupported = "es-u-co-search"
	locale = Locale.parse(unsupported)?
	Collation.collator(locale, {}).map_ok(|_| {}) == Err(UnsupportedLocale(unsupported))
}

# Differential fixture recorded with ICU4J 79.1, Unicode 18.0.0, CLDR 49.
# The ICU4J Maven artifact's SHA-256 is
# 28b292e60d0d00c496bae88a15b2615f89f781be13670aeac9dc90f189570e07.
# Every row names the locale and settings actually used by the reference.
# Numeric cases stay within ICU's 254-significant-digit segment limit.
expect {
	differential_limits = { ..limits, max_numeric_digits: 254 }
	fixtures = [("de-standard-umlaut-primary", "de", Primary, Default, NonIgnorable, ByCollationWeight, "\u(00E4)", "a", Eq), ("de-phonebook-expands-a", "de-u-co-phonebk", Primary, Default, NonIgnorable, ByCollationWeight, "\u(00E4)", "ae", Eq), ("de-phonebook-expands-o", "de-u-co-phonebk", Primary, Default, NonIgnorable, ByCollationWeight, "\u(00D6)", "oe", Eq), ("de-phonebook-expands-u", "de-u-co-phonebk", Primary, Default, NonIgnorable, ByCollationWeight, "\u(00FC)", "ue", Eq), ("de-phonebook-distinguishes-expansion", "de-u-co-phonebk", Secondary, Default, NonIgnorable, ByCollationWeight, "ae", "\u(00E4)", Lt), ("de-phonebook-prefix-order", "de-u-co-phonebk", Primary, Default, NonIgnorable, ByCollationWeight, "ad", "\u(00E4)", Lt), ("de-phonebook-nfd", "de-u-co-phonebk", Identical, Default, NonIgnorable, ByCollationWeight, "\u(00E4)", "a\u(0308)", Eq), ("de-phonebook-discontiguous", "de-u-co-phonebk", Primary, Default, NonIgnorable, ByCollationWeight, "a\u(0327)\u(0308)", "ae", Eq), ("de-phonebook-cgj-block", "de-u-co-phonebk", Primary, Default, NonIgnorable, ByCollationWeight, "a\u(034F)\u(0308)", "ae", Lt), ("de-phonebook-default-case", "de-u-co-phonebk", Tertiary, Default, NonIgnorable, ByCollationWeight, "\u(00E4)", "\u(00C4)", Lt), ("de-phonebook-upper-first", "de-u-co-phonebk", Tertiary, UpperFirst, NonIgnorable, ByCollationWeight, "\u(00E4)", "\u(00C4)", Gt), ("de-phonebook-lower-first", "de-u-co-phonebk", Tertiary, LowerFirst, NonIgnorable, ByCollationWeight, "\u(00E4)", "\u(00C4)", Lt), ("es-n-tilde", "es", Primary, Default, NonIgnorable, ByCollationWeight, "nz", "\u(00F1)", Lt), ("es-n-tilde-before-o", "es", Primary, Default, NonIgnorable, ByCollationWeight, "\u(00F1)z", "o", Lt), ("es-n-tilde-uppercase", "es", Primary, Default, NonIgnorable, ByCollationWeight, "\u(00D1)", "\u(00F1)", Eq), ("es-nfd", "es", Identical, Default, NonIgnorable, ByCollationWeight, "\u(00D1)", "N\u(0303)", Eq), ("es-standard-ch", "es", Primary, Default, NonIgnorable, ByCollationWeight, "ch", "cz", Lt), ("es-traditional-ch", "es-u-co-trad", Primary, Default, NonIgnorable, ByCollationWeight, "ch", "cz", Gt), ("es-traditional-ch-before-d", "es-u-co-trad", Primary, Default, NonIgnorable, ByCollationWeight, "chz", "d", Lt), ("es-traditional-case-mixed-default", "es-u-co-trad", Tertiary, Default, NonIgnorable, ByCollationWeight, "Ch", "CH", Lt), ("es-traditional-case-mixed-upper-first", "es-u-co-trad", Tertiary, UpperFirst, NonIgnorable, ByCollationWeight, "Ch", "CH", Gt), ("es-traditional-case-mixed-lower-first", "es-u-co-trad", Tertiary, LowerFirst, NonIgnorable, ByCollationWeight, "Ch", "CH", Lt), ("es-traditional-reverse-mixed-no-contraction", "es-u-co-trad", Primary, Default, NonIgnorable, ByCollationWeight, "cH", "ch", Lt), ("es-traditional-cgj-block", "es-u-co-trad", Primary, Default, NonIgnorable, ByCollationWeight, "c\u(034F)h", "ch", Lt), ("es-traditional-hyphen-blocks-shifted", "es-u-co-trad", Primary, Default, Shifted, ByCollationWeight, "c-h", "ch", Lt), ("es-traditional-ll", "es-u-co-trad", Primary, Default, NonIgnorable, ByCollationWeight, "ll", "lz", Gt), ("cldr-ignore-hyphen-when-shifted", "de", Tertiary, Default, Shifted, ByCollationWeight, "a-b", "ab", Eq), ("cldr-hyphen-quaternary", "de", Quaternary, Default, Shifted, ByCollationWeight, "a-b", "ab", Lt), ("cldr-heart-stays-nonvariable", "de", Primary, Default, Shifted, ByCollationWeight, "a\u(2665)b", "ab", Lt), ("cldr-embedded-nul", "de", Tertiary, Default, NonIgnorable, ByCollationWeight, "a\u(0000)b", "ab", Eq), ("cldr-numeric", "de", Tertiary, Default, NonIgnorable, ByNumericValue, "file2", "file10", Lt), ("cldr-numeric-arabic-indic", "de", Tertiary, Default, NonIgnorable, ByNumericValue, "file\u(0662)", "file02", Eq), ("cldr-numeric-254-digits", "de", Tertiary, Default, NonIgnorable, ByNumericValue, Str.join_with(List.repeat("9", 253), ""), Str.concat("1", Str.join_with(List.repeat("0", 253), "")), Lt), ("cldr-fffe-identical", "de", Identical, Default, NonIgnorable, ByCollationWeight, "a\u(00AD)\u(FFFE)", "a\u(FFFE)\u(00AD)", Gt)]
	fixtures.all(
		|(name, identifier, strength, case_first, alternate, digit_order, left, right, expected)| {
			locale = Locale.parse(identifier) ?? crash "invalid differential locale ${name}"
			instance = Collation.collator(locale, { strength, alternate, digit_order, case_first }) ?? crash "differential locale rejected for ${name}"
			actual = Collation.compare(instance, left, right, differential_limits)
			if actual != Ok(expected) {
				crash "${name}: expected ${Str.inspect(expected)} and got ${Str.inspect(actual)}"
			}
			Bool.True
		},
	)
}
