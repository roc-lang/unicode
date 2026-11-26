import FuzzSupport
import fuzz.Fuzz
import unicode.Collation
import unicode.Locale
import unicode.Normalization

limits : Collation.Limits
limits = {
	normalization: { max_input_bytes: 16384, max_intermediate_scalars: 4096, max_output_bytes: 16384 },
	max_elements: 32768,
	max_numeric_digits: 4096,
	max_key_bytes: 262144,
}

key_order : List(U8), List(U8) -> [Lt, Eq, Gt]
key_order = |left, right| {
	var $index = 0
	for byte in left {
		match right.get($index) {
			Err(_) => return Gt
			Ok(other) if byte < other => return Lt
			Ok(other) if byte > other => return Gt
			Ok(_) => {
				$index = $index + 1
			}
		}
	}
	if ($index < right.len()) Lt else Eq
}

check : Collation.Collator, Str, Str -> {}
check = |collator, left, right| {
	decomposed = Normalization.normalize(left, NFD, limits.normalization) ?? crash "NFD failed within the test budget"
	composed = Normalization.normalize(left, NFC, limits.normalization) ?? crash "NFC failed within the test budget"
	left_key = Collation.sort_key(collator, decomposed, limits) ?? crash "collation key failed within the test budget"
	right_key = Collation.sort_key(collator, right, limits) ?? crash "collation key failed within the test budget"
	if Collation.sort_key(collator, left, limits) != Ok(left_key) or Collation.sort_key(collator, composed, limits) != Ok(left_key) {
		crash "canonical equivalents produced different collation keys"
	}
	expected = key_order(left_key, right_key)
	reversed = match expected {
		Lt => Gt
		Eq => Eq
		Gt => Lt
	}
	if Collation.compare(collator, composed, right, limits) != Ok(expected) or Collation.compare(collator, right, decomposed, limits) != Ok(reversed) {
		crash "the comparison order disagreed with lexicographic key order"
	}
	# The two spellings of left always tie. A tie with right preserves its
	# position between them. A strict order moves right past both spellings.
	sorted = match expected {
		Lt => [decomposed, composed, right]
		Eq => [decomposed, right, composed]
		Gt => [right, decomposed, composed]
	}
	sort_limits = { per_text: limits, max_items: 3, max_total_key_bytes: 3 * limits.max_key_bytes }
	if Collation.sort(collator, [decomposed, right, composed], sort_limits) != Ok(sorted) {
		crash "the stable sort disagreed with collation key order"
	}
}

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	identifier = match input.config.shr_wrap(5) {
		1 => "de"
		2 => "de-u-co-phonebk"
		3 => "es"
		4 => "es-u-co-trad"
		_ => "und"
	}
	locale = Locale.parse(identifier) ?? crash "invalid test locale"
	strength = match input.config.bitwise_and(7) {
		0 => Primary
		1 => Secondary
		2 => Tertiary
		3 => Quaternary
		_ => Identical
	}
	options : Collation.Options
	options = {
		strength,
		numeric: input.config.bitwise_and(8) != 0,
		alternate: if (input.config.bitwise_and(16) == 0) NonIgnorable else Shifted,
	}
	collator = Collation.collator(locale, options) ?? crash "collation rejected a supported test profile"
	parts = FuzzSupport.source_parts(input.code_points)
	cut = (parts.len() + 1) // 2
	check(collator, Str.join_with(parts.take_first(cut), ""), Str.join_with(parts.drop_first(cut), ""))
	Fuzz.keep
}

CollationTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-collation",
		examples: [
			Fuzz.saved({ name: "stable-canonical-tie", revision: "1", bytes: [4, 0xC5, 0, 0, 0xC5, 0, 0] }),
		],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
