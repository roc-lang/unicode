app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	unicode: "../package/main.roc",
}

import CliArgs
import pf.IOErr exposing [IOErr]
import pf.OsStr exposing [OsStr]
import pf.Stderr
import pf.Stdout
import unicode.LineBreak
import unicode.TextPosition

parse_profile = |name| match name {
	"unicode" => Ok(UnicodeDefault)
	"preserve-graphemes" => Ok(PreserveGraphemes)
	_ => Err(UnknownProfile(name))
}

profile_name = |profile| match profile {
	UnicodeDefault => "unicode"
	PreserveGraphemes => "preserve-graphemes"
}

profile_revision = |profile| match LineBreak.profile_revision(profile) {
	None => "none (exact Unicode default)"
	Some(PreserveGraphemesV1) => "PreserveGraphemesV1"
}

decision_name = |decision| match decision {
	Mandatory => "mandatory"
	Allowed => "allowed"
}

authority_name = |authority| match authority {
	NonTailorable => "non-tailorable"
	Tailorable => "tailorable"
}

format_opportunity = |opportunity| {
	position = opportunity.at
	"byte=${TextPosition.byte_offset(position).to_str()} scalar=${TextPosition.scalar_offset(position).to_str()} ${decision_name(opportunity.decision)} ${authority_name(opportunity.authority)}"
}

complete_opportunities = |source, profile| {
	LineBreak.opportunities_with(source, profile).map(format_opportunity)
}

chunk_opportunities : List(Str), LineBreak.Profile -> Try(List(Str), [CursorFailure(Str)])
chunk_opportunities = |chunks, profile| {
	var $cursor = LineBreak.Cursor.init_with(profile)
	var $lines = []
	for raw_chunk in chunks {
		chunk = if raw_chunk == "-" "" else raw_chunk
		match LineBreak.Cursor.push($cursor, chunk, $lines, |lines, event| lines.append(format_opportunity(event))) {
			Failed({ error, .. }) => return Err(CursorFailure(Str.inspect(error)))
			Pushed(next) => {
				$cursor = next.cursor
				$lines = next.state
			}
		}
	}
	match LineBreak.Cursor.finish($cursor, $lines, |lines, event| lines.append(format_opportunity(event))) {
		Failed({ error, .. }) => Err(CursorFailure(Str.inspect(error)))
		End(final) => Ok(final.state)
	}
}

render = |mode, profile, opportunities| {
	header =
		\\mode: ${mode}
		\\profile: ${profile_name(profile)}
		\\profile-revision: ${profile_revision(profile)}
		\\opportunities: ${opportunities.len().to_str()}
	opportunities.fold(
		header,
		|current, opportunity| {
			\\${current}
			\\${opportunity}
		},
	)
}

expect complete_opportunities("", UnicodeDefault) == [
	"byte=0 scalar=0 mandatory non-tailorable",
]
expect complete_opportunities("a b", UnicodeDefault) == [
	"byte=2 scalar=2 allowed tailorable",
	"byte=3 scalar=3 mandatory non-tailorable",
]
expect complete_opportunities("؀一", UnicodeDefault) == [
	"byte=2 scalar=1 allowed tailorable",
	"byte=5 scalar=2 mandatory non-tailorable",
]
expect complete_opportunities("؀一", PreserveGraphemes) == [
	"byte=5 scalar=2 mandatory non-tailorable",
]
expect chunk_opportunities(["a", " ", "b"], UnicodeDefault) == Ok(complete_opportunities("a b", UnicodeDefault))
expect {
	cursor = LineBreak.Cursor.init({})
	finished = LineBreak.Cursor.finish(cursor, [], |items, item| items.append(item))
	match finished {
		Failed(_) => Bool.False
		End(done) => match LineBreak.Cursor.finish(done.cursor, done.state, |items, item| items.append(item)) {
			Failed({ error: AlreadyFinished, .. }) => Bool.True
			_ => Bool.False
		}
	}
}

main! : List(OsStr) => Try({}, [Exit(I32), StderrErr(IOErr), StdoutErr(IOErr), ..])
main! = |os_args| {
	args = CliArgs.to_strs!(os_args)?
	request = match args {
		[_app, "complete", profile_text, source] => Complete({ profile_text, source })
		[_app, "chunks", profile_text, first, .. as rest] => Chunks({ profile_text, chunks: [first].concat(rest) })
		_ => {
			Stderr.line!("usage: line-break-opportunities complete PROFILE TEXT")?
			Stderr.line!("       line-break-opportunities chunks PROFILE CHUNK [CHUNK ...]")?
			Stderr.line!("       PROFILE is unicode or preserve-graphemes; use - for an empty chunk")?
			return Err(Exit(2))
		}
	}
	profile_text = match request {
		Complete(value) => value.profile_text
		Chunks(value) => value.profile_text
	}
	profile = match parse_profile(profile_text) {
		Ok(value) => value
		Err(UnknownProfile(name)) => {
			Stderr.line!("error: unknown profile ${Str.inspect(name)}; expected unicode or preserve-graphemes")?
			return Err(Exit(2))
		}
	}
	match request {
		Complete({ source, .. }) => {
			lines = complete_opportunities(source, profile)
			Stdout.line!(render("complete-str", profile, lines))?
		}
		Chunks({ chunks, .. }) => match chunk_opportunities(chunks, profile) {
			Ok(lines) => Stdout.line!(render("scalar-aligned-chunks", profile, lines))?
			Err(CursorFailure(message)) => {
				Stderr.line!("error: line-break cursor failed: ${message}")?
				return Err(Exit(1))
			}
		}
	}
	Ok({})
}
