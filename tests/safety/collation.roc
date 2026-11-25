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
	root = Collation.collator(Locale.root, { ..Collation.default_options, strength: Primary })?
	Collation.compare(root, "И\u(0327)\u(0306)", "Й", limits) == Ok(Eq) and
		Collation.compare(root, "И\u(0301)\u(0306)", "И", limits) == Ok(Eq) and
			Collation.compare(root, "И\u(0301)\u(0306)", "Й", limits) == Ok(Lt)
}

# The skipped cedilla is still present at the secondary level. Canonically
# equivalent inputs get identical keys even under the Identical strength.
expect {
	options = { ..Collation.default_options, strength: Identical }
	root = Collation.collator(Locale.root, options)?
	Collation.sort_key(root, "И\u(0327)\u(0306)", limits) == Collation.sort_key(root, "Й\u(0327)", limits) and
		Collation.compare(root, "Й", "Й\u(0327)", limits) == Ok(Lt) and
			Collation.compare(root, "a\u(0000)", "a", limits) == Ok(Gt) and
				Collation.compare(root, "\u(FFFE)", "\u(0000)\u(FFFE)", limits) == Ok(Lt)
}

# CLDR Spanish n/ñ and traditional ch/ll, plus German phonebook expansions.
expect {
	options = { ..Collation.default_options, strength: Primary }
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
	options = { ..Collation.default_options, alternate: Shifted }
	root = Collation.collator(Locale.root, options)?
	fourth = Collation.collator(Locale.root, { ..options, strength: Quaternary })?
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
	options = { ..Collation.default_options, numeric: Bool.True }
	root = Collation.collator(Locale.root, options)?
	Collation.compare(root, "file9", "file10", limits) == Ok(Lt) and
		Collation.compare(root, "file09", "file٩", limits) == Ok(Eq) and
			Collation.compare(root, "99999999999999999999", "100000000000000000000", limits) == Ok(Lt) and
				Collation.compare(root, "file⑨", "file9", limits) == Ok(Gt) and
					Collation.sort_key(root, "001", { ..limits, max_numeric_digits: 2 }) ==
						Err(LimitExceeded({ resource: NumericDigits, limit: 2, required: 3 }))
}

expect {
	open = Collation.default_options
	root = Collation.collator(Locale.root, open)?
	upper = Collation.collator(Locale.root, { ..open, case_first: UpperFirst })?
	lower = Collation.collator(Locale.root, { ..open, case_first: LowerFirst })?
	traditional = Collation.collator(Locale.parse("es-u-co-trad")?, { ..open, case_first: UpperFirst })?
	Collation.compare(root, "a", "A", limits) == Ok(Lt) and
		Collation.compare(upper, "A", "a", limits) == Ok(Lt) and
			Collation.compare(lower, "a", "A", limits) == Ok(Lt) and
				Collation.compare(traditional, "CH", "Ch", limits) == Ok(Lt) and
					Collation.compare(traditional, "Ch", "ch", limits) == Ok(Lt)
}

expect {
	root = Collation.collator(Locale.root, { ..Collation.default_options, strength: Primary })?
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
	Collation.collator(locale, Collation.default_options).map_ok(|_| {}) == Err(UnsupportedLocale(unsupported))
}
