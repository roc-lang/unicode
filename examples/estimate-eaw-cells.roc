app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
	# Use a release bundle URL in applications. The end-to-end suite rewrites
	# this local development dependency to the bundle served by the test driver.
	unicode: "../package/main.roc",
}

import pf.Stderr
import pf.Stdout
import unicode.EastAsianWidth
import unicode.Scalar

default_text = "Hello, 世界"

## An explicit application policy: Fullwidth and Wide scalars occupy two
## cells; every other scalar occupies one. East_Asian_Width is a Unicode fact,
## not a universal terminal or glyph width, so real terminal applications may
## need extra policy for emoji, grapheme clusters, locale, and fonts.
measure_cells : Str -> { cells : U32, scalars : U64 }
measure_cells = |text| {
	var $cells = 0.U32
	var $scalars = 0.U64
	for located in Scalar.iter(text) {
		$cells = $cells + match EastAsianWidth.of_scalar(located.scalar) {
			Fullwidth => 2
			Wide => 2
			_ => 1
		}
		$scalars = $scalars + 1
	}
	{ cells: $cells, scalars: $scalars }
}

report : Str -> Str
report = |text| {
	measured = measure_cells(text)
	Str.join_with(
		[
			"policy: Fullwidth/Wide=2, all other scalars=1",
			"text: ${text}",
			"scalars: ${measured.scalars.to_str()}",
			"cells: ${measured.cells.to_str()}",
		],
		"\n",
	)
}

expect measure_cells("ASCII") == { cells: 5, scalars: 5 }
expect measure_cells("世界") == { cells: 4, scalars: 2 }
expect measure_cells("ＡA") == { cells: 3, scalars: 2 }
expect measure_cells("é") == { cells: 2, scalars: 2 }

main! : List(Str) => Try({}, [Exit(I32), StderrErr(Str), StdoutErr(Str), ..])
main! = |args| {
	text = match args {
		[_app] => default_text
		[_app, provided] => provided
		_ => {
			Stderr.line!("usage: estimate-eaw-cells [TEXT]")?
			return Err(Exit(2))
		}
	}
	Stdout.line!(report(text))?
	Ok({})
}
