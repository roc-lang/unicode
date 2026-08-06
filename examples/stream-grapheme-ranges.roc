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
import unicode.Grapheme
import unicode.UnicodeVersion

format_range = |range| {
	"bytes=${ByteRange.start(range).to_str()}..${ByteRange.end(range).to_str()}"
}

cursor_error = |error| match error {
	AlreadyFinished => "AlreadyFinished"
	OffsetOverflow => "OffsetOverflow"
}

## Segment scalar-aligned text chunks without treating a chunk boundary as the
## end of text. Only absolute ranges are retained, so the cursor and this
## report do not keep any consumed chunk alive.
stream_ranges = |arguments| {
	var $cursor = Grapheme.Cursor.init({})
	var $ranges = []
	var $byte_count = 0.U64
	for argument in arguments {
		chunk = if argument == "-" "" else argument
		next = Grapheme.Cursor.push(
			$cursor,
			chunk,
			$ranges,
			|ranges, range| ranges.append(format_range(range)),
		) ? |error| CursorFailure(cursor_error(error))
		$cursor = next.cursor
		$ranges = next.state
		$byte_count = $byte_count + chunk.count_utf8_bytes()
	}
	finished = Grapheme.Cursor.finish(
		$cursor,
		$ranges,
		|ranges, range| ranges.append(format_range(range)),
	) ? |error| CursorFailure(cursor_error(error))
	Ok({ byte_count: $byte_count, ranges: finished.state })
}

render = |chunk_count, result| {
	header =
		\\unicode: ${UnicodeVersion.to_str(UnicodeVersion.current)}
		\\mode: scalar-aligned-chunks
		\\chunks: ${chunk_count.to_str()}
		\\bytes: ${result.byte_count.to_str()}
		\\graphemes: ${result.ranges.len().to_str()}
	result.ranges.fold(
		header,
		|current, range| {
			\\${current}
			\\${range}
		},
	)
}

expect stream_ranges(["e", "́"]) == Ok({ byte_count: 3, ranges: ["bytes=0..3"] })
expect stream_ranges(["👩", "‍", "🚀"]) == Ok({ byte_count: 11, ranges: ["bytes=0..11"] })
expect stream_ranges(["a", "-", "b"]) == Ok({ byte_count: 2, ranges: ["bytes=0..1", "bytes=1..2"] })
expect {
	first = Grapheme.Cursor.finish(Grapheme.Cursor.init({}), [], |ranges, range| ranges.append(range))
	match first {
		Err(_) => Bool.False
		Ok({ cursor, .. }) => match Grapheme.Cursor.finish(cursor, [], |ranges, range| ranges.append(range)) {
			Err(AlreadyFinished) => Bool.True
			_ => Bool.False
		}
	}
}
expect {
	first = Grapheme.Cursor.finish(Grapheme.Cursor.init({}), [], |ranges, range| ranges.append(range))
	match first {
		Err(_) => Bool.False
		Ok({ cursor, .. }) => match Grapheme.Cursor.push(cursor, "a", [], |ranges, range| ranges.append(range)) {
			Err(AlreadyFinished) => Bool.True
			_ => Bool.False
		}
	}
}

main! : List(OsStr) => Try({}, [Exit(I32), StderrErr(IOErr), StdoutErr(IOErr), ..])
main! = |os_args| {
	args = CliArgs.to_strs!(os_args)?
	chunks = args.drop_first(1)
	if chunks.is_empty() {
		Stderr.line!("usage: stream-grapheme-ranges CHUNK [CHUNK ...]")?
		Stderr.line!("       chunks must be scalar-aligned; use - for an empty chunk")?
		return Err(Exit(2))
	}
	match stream_ranges(chunks) {
		Ok(result) => Stdout.line!(render(chunks.len(), result))?
		Err(CursorFailure(message)) => {
			Stderr.line!("error: grapheme cursor failed: ${message}")?
			return Err(Exit(1))
		}
	}
	Ok({})
}
