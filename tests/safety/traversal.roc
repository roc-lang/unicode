app [run!] {
	pf: platform "../platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Bidi
import unicode.ByteRange
import unicode.CodePoint
import unicode.Grapheme
import unicode.LineBreak
import unicode.Scalar
import unicode.TextPosition
import unicode.TextRange
import unicode.Utf8
import unicode.Word

run! = |input| input

Fact : (U32, U64, U64, U64)

Observation : [
	Decoded(List(Fact), U64, U64),
	Rejected(List(Fact), U64, Utf8.DecodeError),
	InvalidTerminalState,
]

append_fact : List(Fact), Scalar.LocatedScalar -> List(Fact)
append_fact = |facts, located| {
	facts.append((
		Scalar.to_u32(located.scalar),
		ByteRange.start(located.byte_range),
		ByteRange.end(located.byte_range),
		located.scalar_index,
	))
}

emit : List(Fact), Scalar.LocatedScalar -> [Continue(List(Fact)), Stop(List(Fact))]
emit = |facts, located| Continue(append_fact(facts, located))

observe : List(List(U8)) -> Observation
observe = |chunks| {
	var $cursor = Utf8.Cursor.init({})
	var $facts = []
	var $accepted = 0.U64
	for chunk in chunks {
		match Utf8.Cursor.push($cursor, chunk, $facts, emit) {
			Stopped(_) => return InvalidTerminalState
			Failed({ cursor, state, consumed, error: Malformed(error) }) => {
				if !faulted_and_sealed(cursor) {
					return InvalidTerminalState
				}
				return Rejected(state, $accepted + consumed, error)
			}
			Failed(_) => return InvalidTerminalState
			Pushed({ cursor, state, consumed }) => {
				if consumed != chunk.len() {
					return InvalidTerminalState
				}
				$cursor = cursor
				$facts = state
				$accepted = $accepted + consumed
			}
		}
	}
	match Utf8.Cursor.finish($cursor) {
		End({ cursor, byte_offset, scalar_count }) => {
			match (Utf8.Cursor.finish(cursor), Utf8.Cursor.push(cursor, [], [], emit)) {
				(Failed({ error: AlreadyFinished, .. }), Failed({ error: AlreadyFinished, consumed: 0, state: [], .. })) =>
					Decoded($facts, byte_offset, scalar_count)
				_ => InvalidTerminalState
			}
		}
		Failed({ cursor, error: Malformed(error) }) => {
			if faulted_and_sealed(cursor) {
				Rejected($facts, $accepted, error)
			} else {
				InvalidTerminalState
			}
		}
		Failed(_) => InvalidTerminalState
	}
}

faulted_and_sealed : Utf8.Cursor -> Bool
faulted_and_sealed = |cursor| {
	match (Utf8.Cursor.finish(cursor), Utf8.Cursor.push(cursor, [0x41], [], emit)) {
		(Failed({ error: AlreadyFailed, .. }), Failed({ error: AlreadyFailed, consumed: 0, state: [], .. })) => Bool.True
		_ => Bool.False
	}
}

expect {
	# A valid prefix meets an invalid sequence at every possible chunk cut.
	# The expected offset is the first offending byte, or logical EOF.
	prefix = [0x41, 0xCE, 0xB2]
	facts = [(0x41.U32, 0.U64, 1.U64, 0.U64), (0x3B2, 1, 3, 1)]
	cases : List((List(U8), Utf8.Problem, U64))
	cases = [
		([0x80], InvalidStartByte, 0),
		([0xBF], InvalidStartByte, 0),
		([0xF5], InvalidStartByte, 0),
		([0xFF], InvalidStartByte, 0),
		([0xC0, 0x80], OverlongEncoding, 0),
		([0xC1, 0xBF], OverlongEncoding, 0),
		([0xE0, 0x82, 0x80], OverlongEncoding, 1),
		([0xE0, 0x9F, 0xBF], OverlongEncoding, 1),
		([0xF0, 0x8F, 0xBF, 0xBF], OverlongEncoding, 1),
		([0xED, 0xA0, 0x80], EncodesSurrogateHalf, 1),
		([0xED, 0xBF, 0xBF], EncodesSurrogateHalf, 1),
		([0xF4, 0x90, 0x80, 0x80], CodePointTooLarge, 1),
		([0xC2, 0x41], ExpectedContinuation, 1),
		([0xE0, 0xA0, 0xC2], ExpectedContinuation, 2),
		([0xF4, 0x8F, 0xBF, 0x80 - 1], ExpectedContinuation, 3),
		([0xC2], UnexpectedEndOfSequence, 1),
		([0xE0], UnexpectedEndOfSequence, 1),
		([0xE0, 0xA0], UnexpectedEndOfSequence, 2),
		([0xF0], UnexpectedEndOfSequence, 1),
		([0xF0, 0x90], UnexpectedEndOfSequence, 2),
		([0xF0, 0x90, 0x80], UnexpectedEndOfSequence, 3),
	]
	cases.all(
		|(suffix, problem, relative_offset)| {
			bytes = prefix.concat(suffix)
			offset = prefix.len() + relative_offset
			expected = Rejected(facts, offset, { problem, sequence_start: prefix.len(), offset })
			var $cut = 0.U64
			var $equal = match Str.from_utf8(bytes) {
				Err(_) => Bool.True
				Ok(_) => Bool.False
			}
			while $cut <= bytes.len() and $equal {
				$equal = observe([bytes.take_first($cut), [], bytes.drop_first($cut)]) == expected
				$cut = $cut + 1
			}
			$equal
		},
	)
}

expect {
	# Smallest and largest scalar encodings of each width and both sides of
	# the surrogate gap. Verify exact bytes before consulting Roc's validator.
	cases : List((U32, List(U8)))
	cases = [
		(0x0000, [0x00]),
		(0x007F, [0x7F]),
		(0x0080, [0xC2, 0x80]),
		(0x07FF, [0xDF, 0xBF]),
		(0x0800, [0xE0, 0xA0, 0x80]),
		(0xD7FF, [0xED, 0x9F, 0xBF]),
		(0xE000, [0xEE, 0x80, 0x80]),
		(0xFFFF, [0xEF, 0xBF, 0xBF]),
		(0x10000, [0xF0, 0x90, 0x80, 0x80]),
		(0x10FFFF, [0xF4, 0x8F, 0xBF, 0xBF]),
	]
	cases.all(
		|(cp, bytes)| {
			width = bytes.len()
			match (Scalar.from_u32(cp), Str.from_utf8(bytes)) {
				(Ok(scalar), Ok(text)) => {
					short = Scalar.append_utf8([0x61], scalar, width)
					exact = Scalar.append_utf8([0x61], scalar, width + 1)
					exact_empty = Scalar.append_utf8([], scalar, width)
					zero = Scalar.append_utf8([], scalar, 0)
					Scalar.utf8_len(scalar).to_u64() == width and
						Scalar.to_utf8(scalar) == bytes and Scalar.to_str(scalar) == Ok(text) and
							observe(bytes.map(|byte| [byte])) == Decoded([(cp, 0, width, 0)], width, 1) and
								short == Err(OutputLimitExceeded({ limit: width, required: width + 1 })) and
									exact == Ok([0x61].concat(bytes)) and exact_empty == Ok(bytes) and
										zero == Err(OutputLimitExceeded({ limit: 0, required: width }))
				}
				_ => Bool.False
			}
		},
	)
}

expect {
	# Every full-domain surrogate classification must stay outside Scalar.
	var $cp = 0xD800.U32
	var $excluded = Bool.True
	while $cp <= 0xDFFF and $excluded {
		$excluded = match (CodePoint.from_u32($cp), Scalar.from_u32($cp)) {
			(Ok(code_point), Err(InvalidScalar)) => match Scalar.from_code_point(code_point) {
				Err(Surrogate) => Bool.True
				Ok(_) => Bool.False
			}
			_ => Bool.False
		}
		$cp = $cp + 1
	}
	$excluded
}

expect {
	# Stop ahead of malformed bytes. Resume from consumed with absolute
	# coordinates and the original irrevocable prefix.
	chunk = [0xC2, 0x80, 0xE0, 0x41]
	stop = |facts, located| Stop(append_fact(facts, located))
	match Utf8.Cursor.push(Utf8.Cursor.init({}), chunk, [], stop) {
		Stopped({ cursor, state: [(0x80, 0, 2, 0)] as facts, consumed: 2 }) =>
			match Utf8.Cursor.push(cursor, chunk.drop_first(2), facts, emit) {
				Failed({ cursor: terminal, state, consumed: 1, error: Malformed(error) }) =>
					state == facts and error == { problem: ExpectedContinuation, sequence_start: 2, offset: 3 } and
						faulted_and_sealed(terminal)
				_ => Bool.False
			}
		_ => Bool.False
	}
}

expect {
	match (ByteRange.from_bounds(2, 1), ByteRange.from_bounds(1, 2), ByteRange.from_bounds(0, U64.highest)) {
		(Err(EndBeforeStart), Ok(in_scalar), Ok(past_end)) =>
			ByteRange.slice(in_scalar, "β") == Err(NotScalarBoundary) and
				ByteRange.slice(past_end, "β") == Err(OutOfBounds)
		_ => Bool.False
	}
}

repeat = |unit, count| Str.join_with(List.repeat(unit, count), "")

expect {
	# Each family exercises inline, SIMD and heap boundaries before a long run.
	sizes = [1, 7, 8, 15, 16, 23, 24, 31, 32, 128, 1024, 4096]
	sizes.all(
		|count| {
			ascii = repeat("a", count)
			marks = Str.concat("a", repeat("\u(0308)", count))
			zwj = Str.concat("👩", repeat("\u(200D)👩", count))
			indicators = repeat("🇦", count)
			prepend = Str.concat(repeat("\u(0600)", count), "a")
			hangul = Str.concat(repeat("\u(1100)", count), "\u(1161)\u(11A8)")
			indic = Str.concat(repeat("क्", count), "क")
			spaces = Str.concat(repeat(" ", count), "a")
			Grapheme.ranges(ascii).len() == count and
				Grapheme.ranges(marks).len() == 1 and
					Grapheme.ranges(zwj).len() == 1 and
						Grapheme.ranges(indicators).len() == (count + 1) / 2 and
							Grapheme.ranges(prepend).len() == 1 and
								Grapheme.ranges(hangul).len() == 1 and
									Grapheme.ranges(indic).len() == 1 and
										Word.ranges(marks).len() == 1 and
											LineBreak.opportunities(spaces).len() == 2
		},
	)
}

expect {
	# CR reads exactly enough to tell CRLF apart from a lone CR. A non-LF
	# lookahead is reused with its original byte and scalar coordinates.
	source = "β\r😀\r\n\u(0085)\u(2028)a"
	ranges = Bidi.iter_paragraph_ranges(source).fold(
		[],
		|result, range| {
			start = TextRange.start(range)
			end = TextRange.end(range)
			result.append((
				TextPosition.byte_offset(start),
				TextPosition.byte_offset(end),
				TextPosition.scalar_offset(start),
				TextPosition.scalar_offset(end),
			))
		},
	)
	ranges == [(0, 3, 0, 2), (3, 9, 2, 5), (9, 11, 5, 6), (11, 15, 6, 8)]
}
