import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Scalar
import unicode.Utf8

Item : { value : U32, start : U64, end : U64, index : U64 }

Terminal : [
	Complete({ byte_offset : U64, scalar_count : U64, sealed : Bool }),
	MalformedInput({ problem : U8, offset : U64, sequence_start : U64, sealed : Bool }),
	UnexpectedFailure({ kind : U8, at : U64, sealed : Bool }),
]

Trace : { items : List(Item), terminal : Terminal }

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	whole = decode_whole(bytes)
	chunked = decode_byte_chunks(bytes)
	stopped = decode_with_stops(bytes)
	planned = decode_planned(bytes)

	if whole != chunked {
		crash "Utf8.Cursor whole and byte-chunk decoding disagreed"
	}
	if whole != stopped or whole != planned {
		crash "Utf8.Cursor Continue and Stop/resume decoding disagreed"
	}
	validate_trace(bytes, whole)
	Fuzz.keep
}

decode_whole : List(U8) -> Trace
decode_whole = |bytes| {
	pushed = Utf8.Cursor.push(Utf8.Cursor.init({}), bytes, [], append_item)
	match pushed {
		Pushed(next) => finish_trace(next.cursor, next.state)
		Stopped(_) => crash "Continue callback unexpectedly stopped UTF-8 decoding"
		Failed(failure) => failed_trace(failure.cursor, failure.state, failure.error)
	}
}

decode_byte_chunks : List(U8) -> Trace
decode_byte_chunks = |bytes| {
	var $cursor = Utf8.Cursor.init({})
	var $items = []
	for byte in bytes {
		match Utf8.Cursor.push($cursor, [byte], $items, append_item) {
			Pushed(next) => {
				$cursor = next.cursor
				$items = next.state
			}
			Stopped(_) => crash "Continue callback unexpectedly stopped chunked UTF-8 decoding"
			Failed(failure) => return failed_trace(failure.cursor, failure.state, failure.error)
		}
	}
	finish_trace($cursor, $items)
}

decode_with_stops : List(U8) -> Trace
decode_with_stops = |bytes| {
	var $cursor = Utf8.Cursor.init({})
	var $remaining = bytes
	var $items = []
	while Bool.True {
		match Utf8.Cursor.push($cursor, $remaining, $items, |items, located| Stop(items.append(item_shape(located)))) {
			Pushed(next) => return finish_trace(next.cursor, next.state)
			Stopped(next) => {
				if next.consumed == 0 {
					crash "Utf8.Cursor stopped without consuming a scalar"
				}
				$cursor = next.cursor
				$items = next.state
				$remaining = $remaining.drop_first(next.consumed)
				if $remaining.is_empty() {
					return finish_trace($cursor, $items)
				}
			}
			Failed(failure) => return failed_trace(failure.cursor, failure.state, failure.error)
		}
	}
}

## Derive chunk sizes and a stop point from raw bytes without stripping any
## byte from the source. Resume mid-chunk with exactly the unconsumed suffix.
decode_planned : List(U8) -> Trace
decode_planned = |bytes| {
	var $cursor = Utf8.Cursor.init({})
	var $items = []
	var $remaining = bytes
	while !$remaining.is_empty() {
		marker = $remaining.get(0) ?? crash "a nonempty raw chunk has no first byte"
		chunk = $remaining.take_first(1 + marker.to_u64() % 11)
		$remaining = $remaining.drop_first(chunk.len())
		if marker.bitwise_and(1) == 0 {
			match Utf8.Cursor.push($cursor, [], $items, append_item) {
				Pushed(next) if next.consumed == 0 and next.state == $items => {
					$cursor = next.cursor
				}
				_ => crash "an empty UTF-8 chunk changed the cursor's output"
			}
		}
		stop_after = $items.len() + 1 + marker.to_u64() % 3
		var $suffix = chunk
		while !$suffix.is_empty() {
			match Utf8.Cursor.push(
				$cursor,
				$suffix,
				$items,
				|items, located| {
					next = items.append(item_shape(located))
					if (next.len() == stop_after) Stop(next) else Continue(next)
				},
			) {
				Pushed(next) => {
					if next.consumed != $suffix.len() {
						crash "UTF-8 push did not consume a complete chunk"
					}
					$cursor = next.cursor
					$items = next.state
					$suffix = []
				}
				Stopped(next) => {
					if next.consumed == 0 or next.consumed > $suffix.len() or next.state.len() != stop_after {
						crash "UTF-8 stop reported inconsistent consumed bytes or events"
					}
					$cursor = next.cursor
					$items = next.state
					$suffix = $suffix.drop_first(next.consumed)
				}
				Failed(failure) => return failed_trace(failure.cursor, failure.state, failure.error)
			}
		}
	}
	finish_trace($cursor, $items)
}

append_item = |items, located| Continue(items.append(item_shape(located)))

item_shape = |located| {
	range = located.byte_range
	{
		value: Scalar.to_u32(located.scalar),
		start: ByteRange.start(range),
		end: ByteRange.end(range),
		index: located.scalar_index,
	}
}

finish_trace : Utf8.Cursor, List(Item) -> Trace
finish_trace = |cursor, items| {
	match Utf8.Cursor.finish(cursor) {
		End(done) => {
			sealed_finish = match Utf8.Cursor.finish(done.cursor) {
				Failed({ error: AlreadyFinished, .. }) => Bool.True
				_ => Bool.False
			}
			sealed_push = match Utf8.Cursor.push(done.cursor, [], {}, |state, _| Continue(state)) {
				Failed({ error: AlreadyFinished, consumed: 0, .. }) => Bool.True
				_ => Bool.False
			}
			{
				items,
				terminal: Complete({
					byte_offset: done.byte_offset,
					scalar_count: done.scalar_count,
					sealed: sealed_finish and sealed_push,
				}),
			}
		}
		Failed(failure) => failed_trace(failure.cursor, items, failure.error)
	}
}

failed_trace : Utf8.Cursor, List(Item), Utf8.Cursor.Error -> Trace
failed_trace = |cursor, items, error| {
	sealed_finish = match Utf8.Cursor.finish(cursor) {
		Failed({ error: AlreadyFailed, .. }) => Bool.True
		_ => Bool.False
	}
	sealed_push = match Utf8.Cursor.push(cursor, [], {}, |state, _| Continue(state)) {
		Failed({ error: AlreadyFailed, consumed: 0, .. }) => Bool.True
		_ => Bool.False
	}
	sealed = sealed_finish and sealed_push
	terminal = match error {
		Malformed(details) => MalformedInput({
			problem: problem_code(details.problem),
			offset: details.offset,
			sequence_start: details.sequence_start,
			sealed,
		})
		OffsetOverflow({ at }) => UnexpectedFailure({ kind: 1, at, sealed })
		ScalarIndexOverflow({ at }) => UnexpectedFailure({ kind: 2, at, sealed })
		AlreadyFinished => UnexpectedFailure({ kind: 3, at: 0, sealed })
		AlreadyFailed => UnexpectedFailure({ kind: 4, at: 0, sealed })
		InternalFault => UnexpectedFailure({ kind: 5, at: 0, sealed })
	}
	{ items, terminal }
}

problem_code = |problem| match problem {
	InvalidStartByte => 1
	UnexpectedEndOfSequence => 2
	ExpectedContinuation => 3
	OverlongEncoding => 4
	EncodesSurrogateHalf => 5
	CodePointTooLarge => 6
}

validate_trace : List(U8), Trace -> {}
validate_trace = |bytes, trace| {
	var $expected_start = 0
	var $expected_index = 0
	for item in trace.items {
		if item.start != $expected_start or item.end <= item.start or item.index != $expected_index {
			crash "Utf8.Cursor emitted discontinuous ranges or scalar indices"
		}
		scalar = Scalar.from_u32(item.value) ?? crash "Utf8.Cursor emitted a non-scalar value"
		encoded = Scalar.to_utf8(scalar)
		selected = bytes.drop_first(item.start).take_first(item.end - item.start)
		if encoded != selected {
			crash "Utf8.Cursor scalar did not re-encode to its source range"
		}
		$expected_start = item.end
		$expected_index = item.index + 1
	}

	match trace.terminal {
		Complete(done) => {
			if !done.sealed or done.byte_offset != bytes.len() or done.scalar_count != trace.items.len() or $expected_start != bytes.len() {
				crash "successful Utf8.Cursor completion reported inconsistent coordinates"
			}
			match Str.from_utf8(bytes) {
				Err(_) => crash "Utf8.Cursor accepted bytes rejected by Str.from_utf8"
				Ok(_) => {}
			}
		}
		MalformedInput(details) => {
			if !details.sealed or details.sequence_start != $expected_start or details.sequence_start > details.offset or details.offset > bytes.len() {
				crash "malformed UTF-8 reported inconsistent offsets or terminal state"
			}
			match Str.from_utf8(bytes) {
				Ok(_) => crash "Utf8.Cursor rejected bytes accepted by Str.from_utf8"
				Err(_) => {}
			}
		}
		UnexpectedFailure(_) => crash "finite fuzz input reached an unexpected Utf8.Cursor failure"
	}
	{}
}

Utf8Target :: [].{
	target = Fuzz.target_with({
		name: "unicode-utf8",
		examples: [
			Fuzz.saved({ name: "utf8-widths", revision: "1", bytes: [0x41, 0xC2, 0x80, 0xE0, 0xA0, 0x80, 0xF4, 0x8F, 0xBF, 0xBF] }),
			Fuzz.saved({ name: "partial-then-ascii", revision: "1", bytes: [0xF0, 0x9F, 0x41] }),
		],
		generator: Fuzz.raw_bytes,
		test,
		show: FuzzSupport.show_bytes,
	})
}
