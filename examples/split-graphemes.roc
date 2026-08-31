app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	unicode: "../package/main.roc",
}

import CliArgs
import pf.IOErr exposing [IOErr]
import pf.OsStr exposing [OsStr]
import pf.Stderr
import pf.Stdout
import unicode.Grapheme
import unicode.UnicodeVersion

default_text = "🇦🇺🦘🪃"

## Return zero-copy extended grapheme cluster slices with byte coordinates.
## The slices retain the source backing storage, which is appropriate while
## producing this report; use Grapheme.split only when independent copies are
## required. Deriving offsets from each slice avoids segmenting the text twice.
segment : Str -> List({ end : U64, start : U64, text : Str })
segment = |source| {
	var $offset = 0.U64
	var $clusters = []
	for slice in Grapheme.slices(source) {
		end = $offset + slice.count_utf8_bytes()
		$clusters = $clusters.append({ start: $offset, end, text: slice })
		$offset = end
	}
	$clusters
}

report : Str -> Str
report = |source| {
	clusters = segment(source)
	lines = clusters.map(
		|cluster| {
			"bytes ${cluster.start.to_str()}..${cluster.end.to_str()}: ${Str.inspect(cluster.text)}"
		},
	)
	header =
		\\unicode: ${UnicodeVersion.to_str(UnicodeVersion.current)}
		\\text: ${source}
		\\graphemes: ${clusters.len().to_str()}
	lines.fold(
		header,
		|current, line| {
			\\${current}
			\\${line}
		},
	)
}

expect segment("") == []
expect segment("é") == [{ start: 0, end: 3, text: "é" }]
expect segment("🇦🇺🦘") == [
	{ start: 0, end: 8, text: "🇦🇺" },
	{ start: 8, end: 12, text: "🦘" },
]
expect Grapheme.slices("👩‍🚀") == ["👩‍🚀"]

main! : List(OsStr) => Try({}, [Exit(I32), StderrErr(IOErr), StdoutErr(IOErr), ..])
main! = |os_args| {
	args = CliArgs.to_strs!(os_args)?
	source = match args {
		[_app] => default_text
		[_app, provided] => provided
		_ => {
			Stderr.line!("usage: split-graphemes [TEXT]")?
			return Err(Exit(2))
		}
	}
	Stdout.line!(report(source))?
	Ok({})
}
