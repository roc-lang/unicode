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
import unicode.ScalarRange
import unicode.Script
import unicode.ScriptItemization
import unicode.TextRange

## Parse a short list of explicit application preferences. Script aliases use
## Unicode loose matching; this does not infer a language from the input.
parse_preferences : Str -> Try(List(Script.Value), [UnknownScriptAlias(Str)])
parse_preferences = |argument| {
	if argument == "-" {
		Ok([])
	} else {
		var $scripts = []
		for alias in argument.split_on(",") {
			script = Script.from_alias(alias) ?? return Err(UnknownScriptAlias(alias))
			$scripts = $scripts.append(script)
		}
		Ok($scripts)
	}
}

format_run = |run| {
	bytes = TextRange.byte_range(run.range)
	scalars = TextRange.scalar_range(run.range)
	"bytes=${ByteRange.start(bytes).to_str()}..${ByteRange.end(bytes).to_str()} scalars=${ScalarRange.start(scalars).to_str()}..${ScalarRange.end(scalars).to_str()} script=${Script.short_alias(run.script)} (${Script.long_alias(run.script)})"
}

format_cursor_error = |error| match error {
	AlreadyFinished => "AlreadyFinished"
	AlreadyFailed => "AlreadyFailed"
	OffsetOverflow => "OffsetOverflow"
	PendingUnitLimitExceeded(details) => {
		"PendingUnitLimitExceeded limit=${details.limit.to_str()} required=${details.required.to_str()} byte=${details.at_byte.to_str()} scalar=${details.at_scalar.to_str()}"
	}
}

complete_runs = |source, policy| ScriptItemization.runs(source, policy).map(format_run)

stream_runs : List(Str), ScriptItemization.ConservativeScxV1, U64 -> Try(List(Str), [CursorFailure(Str)])
stream_runs = |chunks, policy, max_pending_units| {
	var $cursor = ScriptItemization.cursor_init(policy, max_pending_units)
	var $runs = []
	for raw_chunk in chunks {
		chunk = if raw_chunk == "-" "" else raw_chunk
		match ScriptItemization.cursor_push($cursor, chunk, $runs, |runs, run| runs.append(format_run(run))) {
			Failed({ error, .. }) => return Err(CursorFailure(format_cursor_error(error)))
			Pushed(next) => {
				$cursor = next.cursor
				$runs = next.state
			}
		}
	}
	match ScriptItemization.cursor_finish($cursor, $runs, |runs, run| runs.append(format_run(run))) {
		Failed({ error, .. }) => Err(CursorFailure(format_cursor_error(error)))
		FinishedCursor(done) => Ok(done.state)
	}
}

render = |mode, preferred, runs| {
	names = preferred.map(Script.short_alias)
	header =
		\\mode: ${mode}
		\\policy: ${ScriptItemization.policy_revision}
		\\preferred-scripts: ${if names.is_empty() "none" else Str.join_with(names, ",")}
		\\runs: ${runs.len().to_str()}
	runs.fold(
		header,
		|current, run| {
			\\${current}
			\\${run}
		},
	)
}

expect complete_runs("abc", ScriptItemization.default) == [
	"bytes=0..3 scalars=0..3 script=Latn (Latin)",
]
expect complete_runs("aβ", ScriptItemization.default) == [
	"bytes=0..1 scalars=0..1 script=Latn (Latin)",
	"bytes=1..3 scalars=1..2 script=Grek (Greek)",
]
expect complete_runs("á", ScriptItemization.default) == [
	"bytes=0..3 scalars=0..2 script=Latn (Latin)",
]
expect complete_runs("ー", ScriptItemization.with_preferred([Hira])) == [
	"bytes=0..3 scalars=0..1 script=Hira (Hiragana)",
]
expect stream_runs(["a", "́", "β"], ScriptItemization.default, 8) == Ok(complete_runs("áβ", ScriptItemization.default))
expect stream_runs([" "], ScriptItemization.default, 0) == Err(CursorFailure("PendingUnitLimitExceeded limit=0 required=1 byte=0 scalar=0"))
expect {
	first = ScriptItemization.cursor_push(
		ScriptItemization.cursor_init(ScriptItemization.default, 0),
		"  ",
		[],
		|runs, run| runs.append(run),
	)
	match first {
		Pushed(_) => Bool.False
		Failed(failed) => match ScriptItemization.cursor_push(failed.cursor, "a", [], |runs, run| runs.append(run)) {
			Failed({ error: AlreadyFailed, .. }) => Bool.True
			_ => Bool.False
		}
	}
}

main! : List(OsStr) => Try({}, [Exit(I32), StderrErr(IOErr), StdoutErr(IOErr), ..])
main! = |os_args| {
	args = CliArgs.to_strs!(os_args)?
	request = match args {
		[_app, "complete", preferences, source] => Complete({ preferences, source })
		[_app, "chunks", max_pending, preferences, first, .. as rest] => {
			Chunks({ max_pending, preferences, chunks: [first].concat(rest) })
		}
		_ => {
			Stderr.line!("usage: font-script-runs complete PREFERRED_SCRIPTS TEXT")?
			Stderr.line!("       font-script-runs chunks MAX_PENDING PREFERRED_SCRIPTS CHUNK [CHUNK ...]")?
			Stderr.line!("       use - for no preferences or an empty chunk")?
			return Err(Exit(2))
		}
	}
	preferences_text = match request {
		Complete(value) => value.preferences
		Chunks(value) => value.preferences
	}
	preferred = match parse_preferences(preferences_text) {
		Ok(scripts) => scripts
		Err(UnknownScriptAlias(alias)) => {
			Stderr.line!("error: unrecognized Unicode Script alias ${Str.inspect(alias)}")?
			return Err(Exit(2))
		}
	}
	policy = ScriptItemization.with_preferred(preferred)
	match request {
		Complete({ source, .. }) => {
			Stdout.line!(render("complete-str", preferred, complete_runs(source, policy)))?
		}
		Chunks({ chunks, max_pending, .. }) => {
			limit = U64.from_str(max_pending) ?? {
				Stderr.line!("error: MAX_PENDING must be a non-negative integer")?
				return Err(Exit(2))
			}
			match stream_runs(chunks, policy, limit) {
				Ok(runs) => Stdout.line!(render("scalar-aligned-chunks", preferred, runs))?
				Err(CursorFailure(message)) => {
					Stderr.line!("error: script itemization failed: ${message}")?
					return Err(Exit(1))
				}
			}
		}
	}
	Ok({})
}
