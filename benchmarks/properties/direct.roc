app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.BidiClass
import unicode.BidiProperties
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
import unicode.VerticalOrientation

## Benchmark the independent narrow views when a caller asks for the complete
## bounded property family one field at a time.
run! : Str => Str
run! = |source| {
    var checksum = 0.U64
    for located in Scalar.iter(source) {
        scalar = located.scalar
        emoji = Emoji.of_scalar(scalar)
        checksum = checksum
            + GeneralCategory.short(GeneralCategory.of_scalar(scalar)).count_utf8_bytes()
            + CanonicalCombiningClass.to_u8(CanonicalCombiningClass.of_scalar(scalar)).to_u64()
            + EastAsianWidth.short(EastAsianWidth.of_scalar(scalar)).count_utf8_bytes()
            + BidiClass.short(BidiClass.of_scalar(scalar)).count_utf8_bytes()
            + bool_u64(BidiProperties.is_mirrored(scalar))
            + option_u64(BidiProperties.mirroring_glyph(scalar))
            + option_u64(BidiProperties.paired_bracket(scalar))
            + JoiningType.short(JoiningType.of_scalar(scalar)).count_utf8_bytes()
            + JoiningGroup.short(JoiningGroup.of_scalar(scalar)).count_utf8_bytes()
            + IndicSyllabicCategory.short(IndicSyllabicCategory.of_scalar(scalar)).count_utf8_bytes()
            + IndicPositionalCategory.short(IndicPositionalCategory.of_scalar(scalar)).count_utf8_bytes()
            + bool_u64(Property.is_default_ignorable(scalar))
            + bool_u64(Property.is_variation_selector(scalar))
            + VerticalOrientation.short(VerticalOrientation.of_scalar(scalar)).count_utf8_bytes()
            + emoji_bits(emoji)
    }
    checksum.to_str()
}

bool_u64 = |value| if value 1.U64 else 0.U64
option_u64 = |value| match value { Some(_) => 1.U64, None => 0.U64 }
emoji_bits = |value| bool_u64(value.emoji) + bool_u64(value.emoji_presentation) * 2 + bool_u64(value.emoji_modifier) * 4 + bool_u64(value.emoji_modifier_base) * 8 + bool_u64(value.emoji_component) * 16 + bool_u64(value.extended_pictographic) * 32
