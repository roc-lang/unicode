app [run!] {
	pf: platform "../platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Bidi
import unicode.Grapheme
import unicode.LineBreak
import unicode.ScriptItemization
import unicode.TextPosition
import unicode.TextRange
import unicode.Word

run! : Str => Str
run! = |_| ""

expect {
	zero = { max_input_bytes: 0.U64, max_items: 0.U64 }
	empty = Grapheme.ranges_limited("", zero)
	limited = Grapheme.ranges_limited("a", zero)
	empty == Ok([]) and limited == Err(LimitExceeded({ resource: InputBytes, limit: 0, required: 1 }))
}

expect {
	source = "áb"
	exact = { max_input_bytes: 4.U64, max_items: 2.U64 }
	one = { ..exact, max_items: 1.U64 }
	short = { ..exact, max_input_bytes: 3.U64 }
	Grapheme.slices_limited(source, exact) == Ok(["á", "b"]) and
		Grapheme.split_limited(source, exact) == Ok(["á", "b"]) and
			Grapheme.split_limited(source, one) == Err(LimitExceeded({ resource: OutputItems, limit: 1, required: 2 })) and
				Grapheme.slices_limited(source, short) == Err(LimitExceeded({ resource: InputBytes, limit: 3, required: 4 }))
}

expect {
	source = "a b"
	exact = { max_input_bytes: 3.U64, max_items: 3.U64 }
	short = { ..exact, max_items: 2.U64 }
	zero = { ..exact, max_items: 0.U64 }
	Word.slices_limited(source, exact) == Ok(["a", " ", "b"]) and
		Word.owned_limited(source, exact) == Ok(["a", " ", "b"]) and
			Word.owned_limited(source, short) == Err(LimitExceeded({ resource: OutputItems, limit: 2, required: 3 })) and
				Word.ranges_limited(source, zero) == Err(LimitExceeded({ resource: OutputItems, limit: 0, required: 1 }))
}

expect {
	source = "a"
	exact = { max_input_bytes: 1.U64, max_items: 1.U64 }
	zero = { ..exact, max_items: 0.U64 }
	boundaries = { ..exact, max_items: 2.U64 }
	LineBreak.boundaries_limited(source, UnicodeDefault, boundaries) == Ok(LineBreak.boundaries(source)) and
		LineBreak.boundaries_limited(source, UnicodeDefault, exact) == Err(LimitExceeded({ resource: OutputItems, limit: 1, required: 2 })) and
			LineBreak.opportunities_limited(source, PreserveGraphemes, exact) == Ok(LineBreak.opportunities_with(source, PreserveGraphemes)) and
				LineBreak.opportunities_limited(source, PreserveGraphemes, zero) == Err(LimitExceeded({ resource: OutputItems, limit: 0, required: 1 }))
}

expect {
	source = "a"
	policy = ScriptItemization.default
	exact = { max_input_bytes: 1.U64, max_items: 1.U64 }
	zero = { ..exact, max_items: 0.U64 }
	ScriptItemization.runs_limited(source, policy, exact) == Ok(ScriptItemization.runs(source, policy)) and
		ScriptItemization.runs_limited(source, policy, zero) == Err(LimitExceeded({ resource: OutputItems, limit: 0, required: 1 }))
}

expect {
	source = "a\ra\r\n\r"
	limits = { max_input_bytes: 6.U64, max_items: 3.U64 }
	short = { ..limits, max_items: 2.U64 }
	ranges = Bidi.paragraph_ranges(source)
	ends = ranges.map(|range| TextPosition.byte_offset(TextRange.end(range)))
	ends == [2, 5, 6] and
		Bidi.paragraph_ranges_limited(source, limits) == Ok(ranges) and
			Bidi.paragraph_ranges_limited(source, short) == Err(LimitExceeded({ resource: OutputItems, limit: 2, required: 3 }))
}
