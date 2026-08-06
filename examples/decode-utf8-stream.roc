app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	unicode: "../package/main.roc",
}

import CliArgs
import pf.IOErr exposing [IOErr]
import pf.OsStr exposing [OsStr]
import pf.Stderr
import pf.Stdout
import unicode.ByteRange
import unicode.Scalar
import unicode.Utf8

## Parse comma-separated hexadecimal bytes. A single dash represents an empty
## network chunk, which must be a decoder no-op rather than end of text.
parse_chunk : Str -> Try(List(U8), [InvalidHexChunk(Str)])
parse_chunk = |argument| {
	if argument == "-" {
		Ok([])
	} else {
		var $bytes = []
		for part in argument.split_on(",") {
			byte = U8.from_str("0x${part}") ?? return Err(InvalidHexChunk(argument))
			$bytes = $bytes.append(byte)
		}
		Ok($bytes)
	}
}

problem_name = |problem| match problem {
	InvalidStartByte => "InvalidStartByte"
	UnexpectedEndOfSequence => "UnexpectedEndOfSequence"
	ExpectedContinuation => "ExpectedContinuation"
	OverlongEncoding => "OverlongEncoding"
	EncodesSurrogateHalf => "EncodesSurrogateHalf"
	CodePointTooLarge => "CodePointTooLarge"
}

cursor_error_message = |error| match error {
	Malformed(details) => {
		"${problem_name(details.problem)} at byte ${details.offset.to_str()} (sequence start ${details.sequence_start.to_str()})"
	}
	OffsetOverflow({ at }) => "OffsetOverflow at byte ${at.to_str()}"
	ScalarIndexOverflow({ at }) => "ScalarIndexOverflow at scalar ${at.to_str()}"
	AlreadyFinished => "AlreadyFinished"
	AlreadyFailed => "AlreadyFailed"
	InternalFault => "InternalFault"
}

format_scalar = |located| {
	range = located.byte_range
	"scalar ${located.scalar_index.to_str()}: value=${Scalar.to_u32(located.scalar).to_str()} bytes=${ByteRange.start(range).to_str()}..${ByteRange.end(range).to_str()}"
}

## Consume one transport chunk under one-scalar backpressure. `Stop` leaves the
## suffix with the caller; `consumed` is therefore used to resume until the
## entire chunk has been accepted. The decoder never retains that suffix.
push_chunk = |initial_cursor, chunk, initial_state| {
	var $cursor = initial_cursor
	var $remaining = chunk
	var $state = initial_state
	while Bool.True {
		pushed = Utf8.Cursor.push(
			$cursor,
			$remaining,
			$state,
			|state, located| Stop({
				count: state.count + 1,
				lines: state.lines.append(format_scalar(located)),
			}),
		)
		match pushed {
			Failed({ error, .. }) => return Err(DecodeFailed(cursor_error_message(error)))
			Pushed(next) => return Ok({ cursor: next.cursor, state: next.state })
			Stopped(next) => {
				if next.consumed == 0 {
					return Err(DecodeFailed("decoder stopped without consuming a scalar"))
				}
				$cursor = next.cursor
				$state = next.state
				$remaining = $remaining.drop_first(next.consumed)
				if $remaining.is_empty() {
					return Ok({ cursor: $cursor, state: $state })
				}
			}
		}
	}
}

decode_chunks : List(Str) -> Try(Str, [BadChunk(Str), DecodeFailed(Str)])
decode_chunks = |arguments| {
	var $cursor = Utf8.Cursor.init({})
	var $lines = []
	var $count = 0.U64

	for argument in arguments {
		chunk = parse_chunk(argument) ? |_| BadChunk(argument)
		next = push_chunk($cursor, chunk, { count: $count, lines: $lines })?
		$cursor = next.cursor
		$lines = next.state.lines
		$count = next.state.count
	}

	finished = match Utf8.Cursor.finish($cursor) {
		Failed({ error, .. }) => return Err(DecodeFailed(cursor_error_message(error)))
		End(value) => value
	}
	scalars = $lines.fold(
		"",
		|current, line| {
			if current == "" {
				line
			} else {
				\\${current}
				\\${line}
			}
		},
	)
	summary = "complete: ${finished.byte_offset.to_str()} bytes, ${finished.scalar_count.to_str()} scalars"
	Ok(
		if scalars == "" {
			summary
		} else {
			\\${scalars}
			\\${summary}
		},
	)
}

expect decode_chunks(["41", "C3,A9", "F0,9F,A6,98"]) == Ok(
	\\scalar 0: value=65 bytes=0..1
	\\scalar 1: value=233 bytes=1..3
	\\scalar 2: value=129432 bytes=3..7
	\\complete: 7 bytes, 3 scalars
	,
)
expect decode_chunks(["F0", "9F", "A6", "98"]) == Ok(
	\\scalar 0: value=129432 bytes=0..4
	\\complete: 4 bytes, 1 scalars
	,
)
expect decode_chunks(["E2,82"]) == Err(DecodeFailed("UnexpectedEndOfSequence at byte 2 (sequence start 0)"))
expect decode_chunks(["FF"]) == Err(DecodeFailed("InvalidStartByte at byte 0 (sequence start 0)"))
expect {
	finished = Utf8.Cursor.finish(Utf8.Cursor.init({}))
	match finished {
		Failed(_) => Bool.False
		End({ cursor, .. }) => match Utf8.Cursor.push(cursor, [], {}, |state, _| Continue(state)) {
			Failed({ error: AlreadyFinished, .. }) => Bool.True
			_ => Bool.False
		}
	}
}
expect {
	failed = Utf8.Cursor.push(Utf8.Cursor.init({}), [0xFF], {}, |state, _| Continue(state))
	match failed {
		Pushed(_) | Stopped(_) => Bool.False
		Failed({ cursor, .. }) => match Utf8.Cursor.push(cursor, [], {}, |state, _| Continue(state)) {
			Failed({ error: AlreadyFailed, .. }) => Bool.True
			_ => Bool.False
		}
	}
}

main! : List(OsStr) => Try({}, [Exit(I32), StderrErr(IOErr), StdoutErr(IOErr), ..])
main! = |os_args| {
	args = CliArgs.to_strs!(os_args)?
	chunks = args.drop_first(1)
	if chunks.is_empty() {
		Stderr.line!("usage: decode-utf8-stream HEX_BYTES [HEX_BYTES ...]")?
		Stderr.line!("       bytes are comma-separated; use - for an empty chunk")?
		return Err(Exit(2))
	}
	match decode_chunks(chunks) {
		Ok(report) => Stdout.line!(report)?
		Err(BadChunk(argument)) => {
			Stderr.line!("error: invalid hexadecimal byte chunk ${Str.inspect(argument)}")?
			return Err(Exit(2))
		}
		Err(DecodeFailed(message)) => {
			Stderr.line!("error: malformed UTF-8: ${message}")?
			return Err(Exit(1))
		}
	}
	Ok({})
}
