import InternalEAW
import InternalBidiProperties
import InternalEmoji
import InternalGraphemeData

## Stable, test-private numeric encodings of generated Unicode properties.
## This module is exposed only by test-main.roc, never by the public package.
TestProperties :: {}.{
	gcb : U32 -> U8
	gcb = |u32| {
		match InternalGraphemeData.lookup(u32).gcb {
			Other => 0
			CR => 1
			LF => 2
			Control => 3
			Extend => 4
			ZWJ => 5
			RI => 6
			Prepend => 7
			SpacingMark => 8
			L => 9
			V => 10
			T => 11
			LV => 12
			LVT => 13
		}
	}

	eaw : U32 -> U8
	eaw = |u32| {
		match InternalEAW.east_asian_width_property(u32) {
			N => 0
			A => 1
			F => 2
			H => 3
			Na => 4
			W => 5
		}
	}

	emoji : U32 -> U8
	emoji = |u32| {
		(if InternalEmoji.is_emoji(u32) 1 else 0)
			+ (if InternalEmoji.is_presentation(u32) 2 else 0)
			+ (if InternalEmoji.is_modifier(u32) 4 else 0)
			+ (if InternalEmoji.is_base(u32) 8 else 0)
			+ (if InternalEmoji.is_component(u32) 16 else 0)
			+ (if InternalEmoji.is_pictographic(u32) 32 else 0)
	}

	## Test-private, source-stable encodings for the four UAX #9 property
	## projections. `0xFFFFFFFF` represents an absent scalar mapping.
	bidi_class : U32 -> U8
	bidi_class = |u32| {
		match InternalBidiProperties.lookup(u32) {
			L => 0
			AL => 1
			AN => 2
			B => 3
			BN => 4
			CS => 5
			EN => 6
			ES => 7
			ET => 8
			FSI => 9
			LRE => 10
			LRI => 11
			LRO => 12
			NSM => 13
			ON => 14
			PDF => 15
			PDI => 16
			R => 17
			RLE => 18
			RLI => 19
			RLO => 20
			S => 21
			WS => 22
		}
	}

	bidi_mirrored : U32 -> U8
	bidi_mirrored = |u32| if InternalBidiProperties.is_mirrored(u32) 1 else 0

	bidi_mirroring_glyph : U32 -> U32
	bidi_mirroring_glyph = |u32| match InternalBidiProperties.mirroring_glyph(u32) {
		Some(value) => value
		None => 0xFFFFFFFF
	}

	bidi_bracket_target : U32 -> U32
	bidi_bracket_target = |u32| match InternalBidiProperties.paired_bracket(u32) {
		Some(pair) => pair.scalar
		None => 0xFFFFFFFF
	}

	bidi_bracket_type : U32 -> U8
	bidi_bracket_type = |u32| match InternalBidiProperties.paired_bracket(u32) {
		Some(pair) => match pair.kind {
			Open => 1
			Close => 2
		}
		None => 0
	}
}
