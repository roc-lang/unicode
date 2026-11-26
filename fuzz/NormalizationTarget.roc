import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Normalization
import unicode.Scalar

# The plan has at most 128 scalars. These maxima leave room for every
# decomposition and identifier fold in the pinned Unicode data, including
# the same calls on already-transformed output.
generous : Normalization.Limits
generous = { max_input_bytes: 16384, max_intermediate_scalars: 4096, max_output_bytes: 16384 }

normalize : Str, Normalization.Form -> Str
normalize = |text, form| Normalization.normalize(text, form, generous) ?? crash "normalization exceeded the test budget or faulted"

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	parts = FuzzSupport.source_parts(input.code_points)
	source = Str.join_with(parts, "")
	nfc = normalize(source, NFC)
	nfd = normalize(source, NFD)
	nfkc = normalize(source, NFKC)
	nfkd = normalize(source, NFKD)
	if normalize(nfc, NFD) != nfd or normalize(nfd, NFC) != nfc {
		crash "the canonical forms did not preserve canonical equivalence"
	}
	if normalize(nfkc, NFKD) != nfkd or normalize(nfkd, NFC) != nfkc or normalize(nfd, NFKD) != nfkd {
		crash "the compatibility forms did not preserve compatibility equivalence"
	}
	if Normalization.canonically_equivalent(source, nfc, generous) != Ok(Bool.True) {
		crash "the canonical equivalence API disagreed with normalization"
	}

	for (form, normalized) in [(NFC, nfc), (NFD, nfd), (NFKC, nfkc), (NFKD, nfkd)] {
		check_form(source, form, normalized)
	}

	(form, normalized, decomposed) = match input.config.bitwise_and(3) {
		0 => (NFC, nfc, nfd)
		1 => (NFD, nfd, nfd)
		2 => (NFKC, nfkc, nfkd)
		_ => (NFKD, nfkd, nfkd)
	}
	cut = input.config.shr_wrap(2).to_u64() % (parts.len() + 1)
	left = normalize(Str.join_with(parts.take_first(cut), ""), form)
	right = normalize(Str.join_with(parts.drop_first(cut), ""), form)
	if normalize(Str.concat(left, right), form) != normalized {
		crash "normalizing each side first changed the normalized concatenation"
	}
	check_limits(source, form, normalized, decomposed, cut)

	folded = Normalization.nfkc_case_fold(source, generous) ?? crash "identifier fold exceeded the test budget or faulted"
	if Normalization.nfkc_case_fold(folded, generous) != Ok(folded) or normalize(folded, NFC) != folded {
		crash "identifier fold did not produce an idempotent NFC result"
	}
	Fuzz.keep
}

check_form : Str, Normalization.Form, Str -> {}
check_form = |source, form, normalized| {
	if normalize(normalized, form) != normalized {
		crash "normalization was not idempotent"
	}
	same = source == normalized
	if Normalization.is_normalized(source, form, generous) != Ok(same) {
		crash "is_normalized disagreed with the transformation"
	}
	match Normalization.quick_check(source, form) {
		Yes if !same => crash "quick_check said Yes for text changed by normalization"
		No if same => crash "quick_check said No for text already in the selected form"
		Maybe if form == NFD or form == NFKD => crash "a decomposition form returned Maybe"
		_ => {}
	}
}

check_limits : Str, Normalization.Form, Str, Str, U64 -> {}
check_limits = |source, form, normalized, decomposed, selector| {
	input_bytes = source.count_utf8_bytes()
	output_bytes = normalized.count_utf8_bytes()
	tape = Scalar.iter(decomposed).fold(0.U64, |count, _| count + 1)
	exact : Normalization.Limits
	exact = { max_input_bytes: input_bytes, max_intermediate_scalars: tape, max_output_bytes: output_bytes }
	if Normalization.normalize(source, form, exact) != Ok(normalized) {
		crash "normalization changed its result at exact input, intermediate, or output capacity"
	}

	if input_bytes > 0 {
		budget = undersized(input_bytes, selector)
		got = Normalization.normalize(source, form, { ..exact, max_input_bytes: budget })
		if got != Err(LimitExceeded({ resource: InputBytes, limit: budget, required: input_bytes })) {
			crash "normalization did not report the expected input-byte limit"
		}
	}
	if tape > 0 {
		budget = undersized(tape, selector)
		got = Normalization.normalize(source, form, { ..exact, max_intermediate_scalars: budget })
		if got != Err(LimitExceeded({ resource: IntermediateScalars, limit: budget, required: budget + 1 })) {
			crash "normalization did not report the first scalar beyond its intermediate limit"
		}
	}
	if output_bytes > 0 {
		budget = undersized(output_bytes, selector)
		var $required = output_bytes
		for located in Scalar.iter(normalized) {
			end = ByteRange.end(located.byte_range)
			if end > budget {
				$required = end
				break
			}
		}
		got = Normalization.normalize(source, form, { ..exact, max_output_bytes: budget })
		if got != Err(LimitExceeded({ resource: OutputBytes, limit: budget, required: $required })) {
			crash "normalization did not report the first scalar beyond its output byte limit"
		}
	}
	{}
}

undersized : U64, U64 -> U64
undersized = |exact, selector| if (selector == 0) exact - 1 else selector % exact

NormalizationTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-normalization",
		examples: [
			Fuzz.saved({ name: "compose-across-cut", revision: "1", bytes: [4, 0x41, 0, 0, 0x0A, 3, 0, 1, 3, 0] }),
			Fuzz.saved({ name: "compatibility-expansion", revision: "1", bytes: [3, 3, 0xF3, 0] }),
		],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
