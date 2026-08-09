app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	unicode: "../package/main.roc",
}

import CliArgs
import pf.IOErr exposing [IOErr]
import pf.OsStr exposing [OsStr]
import pf.Stderr
import pf.Stdout
import unicode.Bidi
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange

parse_base = |name| match name {
	"auto" => Ok(Auto)
	"ltr" => Ok(LeftToRight)
	"rtl" => Ok(RightToLeft)
	_ => Err(UnknownBase(name))
}

base_name = |base| match base {
	Auto => "auto"
	LeftToRight => "ltr"
	RightToLeft => "rtl"
}

level_name = |level| match level {
	Level(value) => value.to_str()
	RemovedByX9 => "x"
}

scalar_option_name = |value| match value {
	None => "none"
	Some(scalar) => Scalar.to_u32(scalar).to_str()
}

format_mirrors = |mirrors| {
	var output = []
	var at = 0.U64
	for mirror in mirrors {
		if mirror.needs_glyph {
			output = output.append("${at.to_str()}:${scalar_option_name(mirror.glyph)}")
		}
		at = at + 1
	}
	if output.is_empty() "none" else Str.join_with(output, ",")
}

render = |base, analysis, line| {
	paragraph = TextRange.scalar_range(Bidi.paragraph_range(analysis))
	levels = Bidi.line_levels(line).map(level_name)
	visual = Bidi.visual_to_logical(line).map(U64.to_str)
	logical_to_visual = Bidi.logical_to_visual(line).map(
		|position| match position {
			Some(value) => value.to_str()
			None => "x"
		},
	)
	\\requested-base: ${base_name(base)}
	\\paragraph-level: ${Bidi.paragraph_level(analysis).to_str()}
	\\scalar-range: ${ScalarRange.start(paragraph).to_str()}..${ScalarRange.end(paragraph).to_str()}
	\\line-levels: ${Str.join_with(levels, ",")}
	\\visual-to-logical: ${Str.join_with(visual, ",")}
	\\logical-to-visual: ${Str.join_with(logical_to_visual, ",")}
	\\mirrored-glyphs: ${format_mirrors(Bidi.line_mirroring(line))}
}

main! : List(OsStr) => Try({}, [Exit(I32), StderrErr(IOErr), StdoutErr(IOErr), ..])
main! = |os_args| {
	args = CliArgs.to_strs!(os_args)?
	match args {
		[_app, base_text, source] => {
			base = match parse_base(base_text) {
				Ok(value) => value
				Err(UnknownBase(name)) => {
					Stderr.line!("error: BASE must be auto, ltr, or rtl; got ${Str.inspect(name)}")?
					return Err(Exit(2))
				}
			}
			analysis = match Bidi.analyze_paragraph(source, base, Bidi.default_limits) {
				Ok(value) => value
				Err(error) => {
					Stderr.line!("error: bidi analysis failed: ${Str.inspect(error)}")?
					return Err(Exit(1))
				}
			}
			line_range = TextRange.scalar_range(Bidi.paragraph_range(analysis))
			line = match Bidi.reorder_line(analysis, line_range) {
				Ok(value) => value
				Err(error) => {
					Stderr.line!("error: bidi line reordering failed: ${Str.inspect(error)}")?
					return Err(Exit(1))
				}
			}
			Stdout.line!(render(base, analysis, line))?
			Ok({})
		}
		_ => {
			Stderr.line!("usage: bidi-analysis BASE TEXT")?
			Stderr.line!("       BASE is auto, ltr, or rtl; TEXT is one P1 paragraph")?
			Err(Exit(2))
		}
	}
}
