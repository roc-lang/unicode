app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
	unicode: "../package/main.roc",
}

import pf.Stderr
import pf.Stdout
import unicode.ByteRange
import unicode.Scalar

default_text = "café ☕"

## Count Unicode scalar values in a valid Roc Str without allocating a list.
## Scalar.iter is total for Str and yields absolute UTF-8 byte coordinates.
scalar_summary : Str -> { byte_count : U64, scalar_count : U64, positions : List(Str) }
scalar_summary = |text| {
	var $scalar_count = 0.U64
	var $positions = []
	for located in Scalar.iter(text) {
		range = located.byte_range
		$positions = $positions.append(
			"${located.scalar_index.to_str()}: value=${Scalar.to_u32(located.scalar).to_str()} bytes=${ByteRange.start(range).to_str()}..${ByteRange.end(range).to_str()}",
		)
		$scalar_count = $scalar_count + 1
	}
	{
		byte_count: text.count_utf8_bytes(),
		scalar_count: $scalar_count,
		positions: $positions,
	}
}

report : Str -> Str
report = |text| {
	summary = scalar_summary(text)
	header =
		\\text: ${text}
		\\utf8-bytes: ${summary.byte_count.to_str()}
		\\unicode-scalars: ${summary.scalar_count.to_str()}
	summary.positions.fold(
		header,
		|current, position| {
			\\${current}
			\\${position}
		},
	)
}

expect scalar_summary("").scalar_count == 0
expect scalar_summary("abc").byte_count == 3
expect scalar_summary("aé🦘").positions == [
	"0: value=97 bytes=0..1",
	"1: value=233 bytes=1..3",
	"2: value=129432 bytes=3..7",
]

main! : List(Str) => Try({}, [Exit(I32), StderrErr(Str), StdoutErr(Str), ..])
main! = |args| {
	text = match args {
		[_app] => default_text
		[_app, provided] => provided
		_ => {
			Stderr.line!("usage: count-scalars [TEXT]")?
			return Err(Exit(2))
		}
	}
	Stdout.line!(report(text))?
	Ok({})
}
