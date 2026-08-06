app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
	unicode: "../package/main.roc",
}

import pf.Stderr
import pf.Stdout
import unicode.BidiClass
import unicode.ByteRange
import unicode.CanonicalCombiningClass
import unicode.EastAsianWidth
import unicode.Emoji
import unicode.GeneralCategory
import unicode.IndicPositionalCategory
import unicode.IndicSyllabicCategory
import unicode.JoiningGroup
import unicode.JoiningType
import unicode.Property
import unicode.Scalar
import unicode.Script
import unicode.UnicodeVersion
import unicode.VerticalOrientation

yes_no = |value| if value "Y" else "N"

script_set_names = |set| {
	names = Script.walk(set, [], |items, script| items.append(Script.short_alias(script)))
	Str.join_with(names, "+")
}

mirror_name = |mapping| match mapping {
	None => "none"
	Some(scalar) => Scalar.to_u32(scalar).to_str()
}

bracket_name = |mapping| match mapping {
	None => "none"
	Some({ scalar, kind }) => {
		kind_name = match kind {
			Open => "open"
			Close => "close"
		}
		"${Scalar.to_u32(scalar).to_str()}:${kind_name}"
	}
}

format_entry = |entry| {
	located = entry.located
	row = entry.row
	scalar = located.scalar
	bytes = located.byte_range
	emoji = Property.Row.emoji(row)
	script = Script.of_scalar(scalar)
	extensions = Script.extensions_of_scalar(scalar)
	\\${located.scalar_index.to_str()} value=${Scalar.to_u32(scalar).to_str()} bytes=${ByteRange.start(bytes).to_str()}..${ByteRange.end(bytes).to_str()}
	\\  gc=${GeneralCategory.short(Property.Row.general_category(row))} ccc=${CanonicalCombiningClass.to_u8(Property.Row.canonical_combining_class(row)).to_str()} eaw=${EastAsianWidth.short(Property.Row.east_asian_width(row))}
	\\  script=${Script.short_alias(script)} scx=${script_set_names(extensions)} bidi=${BidiClass.short(Property.Row.bidi_class(row))} mirrored=${yes_no(Property.Row.bidi_mirrored(row))} mirror=${mirror_name(Property.Row.bidi_mirroring_glyph(row))} bracket=${bracket_name(Property.Row.bidi_paired_bracket(row))}
	\\  joining=${JoiningType.short(Property.Row.joining_type(row))}/${JoiningGroup.short(Property.Row.joining_group(row))} indic=${IndicSyllabicCategory.short(Property.Row.indic_syllabic_category(row))}/${IndicPositionalCategory.short(Property.Row.indic_positional_category(row))} vertical=${VerticalOrientation.short(Property.Row.vertical_orientation(row))}
	\\  default-ignorable=${yes_no(Property.Row.default_ignorable(row))} variation-selector=${yes_no(Property.Row.variation_selector(row))} emoji=${yes_no(emoji.emoji)} emoji-presentation=${yes_no(emoji.emoji_presentation)} modifier=${yes_no(emoji.emoji_modifier)} modifier-base=${yes_no(emoji.emoji_modifier_base)} component=${yes_no(emoji.emoji_component)} extended-pictographic=${yes_no(emoji.extended_pictographic)}
}

variation_lines = |entries| {
	var $previous = None
	var $lines = []
	for entry in entries {
		current = entry.located.scalar
		match $previous {
			None => {}
			Some(base) => match Emoji.variation_presentation(base, current) {
				None => {}
				Some(presentation) => {
					name = match presentation {
						Text => "text"
						Emoji => "emoji"
					}
					$lines = $lines.append(
						"  base=${Scalar.to_u32(base).to_str()} selector=${Scalar.to_u32(current).to_str()} presentation=${name}",
					)
				}
			}
		}
		$previous = Some(current)
	}
	$lines
}

## Inspect immutable Unicode facts using one fused Property.Row lookup per
## scalar. These facts are inputs to shaping and layout; this application does
## not reorder bidi text, choose fonts, shape glyphs, or infer language.
report = |source| {
	entries = Property.fold(source, [], |items, entry| items.append(entry))
	lines = entries.map(format_entry)
	variations = variation_lines(entries)
	header =
		\\unicode: ${UnicodeVersion.to_str(UnicodeVersion.current)}
		\\scalars: ${lines.len().to_str()}
		\\variation-requests: ${variations.len().to_str()}
	with_scalars = lines.fold(
		header,
		|current, line| {
			\\${current}
			\\${line}
		},
	)
	variations.fold(
		with_scalars,
		|current, variation| {
			\\${current}
			\\${variation}
		},
	)
}

expect report("") ==
	\\unicode: 17.0.0
	\\scalars: 0
	\\variation-requests: 0
expect report("A") ==
	\\unicode: 17.0.0
	\\scalars: 1
	\\variation-requests: 0
	\\0 value=65 bytes=0..1
	\\  gc=Lu ccc=0 eaw=Na
	\\  script=Latn scx=Latn bidi=L mirrored=N mirror=none bracket=none
	\\  joining=U/No_Joining_Group indic=Other/NA vertical=R
	\\  default-ignorable=N variation-selector=N emoji=N emoji-presentation=N modifier=N modifier-base=N component=N extended-pictographic=N
expect report("́").contains("gc=Mn ccc=230 eaw=A")
expect report("(").contains("mirrored=Y mirror=41 bracket=41:open")
expect report("🦘").contains("emoji=Y emoji-presentation=Y")
expect {
	heart = Scalar.from_u32(0x2665) ?? ...
	vs15 = Scalar.from_u32(0xFE0E) ?? ...
	vs16 = Scalar.from_u32(0xFE0F) ?? ...
	letter = Scalar.from_u32(0x41) ?? ...
	Emoji.variation_presentation(heart, vs15) == Some(Text)
		and Emoji.variation_presentation(heart, vs16) == Some(Emoji)
			and Emoji.variation_presentation(letter, vs16) == None
}

main! : List(Str) => Try({}, [Exit(I32), StderrErr(Str), StdoutErr(Str), ..])
main! = |args| {
	source = match args {
		[_app, text] => text
		_ => {
			Stderr.line!("usage: shaping-facts TEXT")?
			return Err(Exit(2))
		}
	}
	Stdout.line!(report(source))?
	Ok({})
}
