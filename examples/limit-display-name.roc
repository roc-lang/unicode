app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
	unicode: "../package/main.roc",
}

import pf.Stderr
import pf.Stdout
import unicode.ByteRange
import unicode.Grapheme

## Select a seamless prefix containing at most max_graphemes extended
## grapheme clusters. The iterator stops once the requested prefix is known,
## and the returned slice retains the source instead of copying it.
limit_display_name : Str, U64 -> Try({ slice : Str, truncated : Bool }, [UnexpectedRange])
limit_display_name = |source, max_graphemes| {
	end = Iter.take_first(Grapheme.iter_ranges(source), max_graphemes).fold(
		0.U64,
		|_, range| ByteRange.end(range),
	)
	range = ByteRange.from_bounds(0, end) ? |_| UnexpectedRange
	prefix = ByteRange.slice(range, source) ? |_| UnexpectedRange
	Ok({
		slice: prefix,
		truncated: end < source.count_utf8_bytes(),
	})
}

render : Str, U64 -> Try(Str, [UnexpectedRange])
render = |source, limit| {
	result = limit_display_name(source, limit)?
	suffix = if result.truncated "…" else ""
	Ok(
		\\input: ${source}
		\\limit: ${limit.to_str()} graphemes
		\\display: ${result.slice}${suffix}
		\\truncated: ${if result.truncated "yes" else "no"}
		,
	)
}

expect limit_display_name("", 3) == Ok({ slice: "", truncated: Bool.False })
expect limit_display_name("hello", 5) == Ok({ slice: "hello", truncated: Bool.False })
expect limit_display_name("hello", 3) == Ok({ slice: "hel", truncated: Bool.True })
expect limit_display_name("éclair", 1) == Ok({ slice: "é", truncated: Bool.True })
expect limit_display_name("👩🏽‍🚀 mission", 1) == Ok({ slice: "👩🏽‍🚀", truncated: Bool.True })
expect limit_display_name("🇦🇺 team", 1) == Ok({ slice: "🇦🇺", truncated: Bool.True })

main! : List(Str) => Try({}, [Exit(I32), StderrErr(Str), StdoutErr(Str), ..])
main! = |args| {
	(limit_text, source) = match args {
		[_app, limit_arg, display_name] => (limit_arg, display_name)
		_ => {
			Stderr.line!("usage: limit-display-name MAX_GRAPHEMES DISPLAY_NAME")?
			return Err(Exit(2))
		}
	}
	limit = match U64.from_str(limit_text) {
		Ok(value) => value
		Err(_) => {
			Stderr.line!("error: MAX_GRAPHEMES must be a non-negative integer")?
			return Err(Exit(2))
		}
	}
	match render(source, limit) {
		Ok(output) => Stdout.line!(output)?
		Err(_) => {
			Stderr.line!("error: package returned an unexpected non-scalar-aligned range")?
			return Err(Exit(1))
		}
	}
	Ok({})
}
