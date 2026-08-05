app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.BidiClass
import unicode.CanonicalCombiningClass
import unicode.EastAsianWidth
import unicode.GeneralCategory
import unicode.IndicPositionalCategory
import unicode.IndicSyllabicCategory
import unicode.JoiningGroup
import unicode.JoiningType
import unicode.Property
import unicode.VerticalOrientation

## Benchmark one fused row-id lookup per scalar, lazy column access, and the
## single-decode complete-string fold.
run! : Str => Str
run! = |source| {
    checksum = Property.fold(source, 0.U64, |sum, entry| {
        row = entry.row
        emoji = Property.Row.emoji(row)
        sum
            + GeneralCategory.short(Property.Row.general_category(row)).count_utf8_bytes()
            + CanonicalCombiningClass.to_u8(Property.Row.canonical_combining_class(row)).to_u64()
            + EastAsianWidth.short(Property.Row.east_asian_width(row)).count_utf8_bytes()
            + BidiClass.short(Property.Row.bidi_class(row)).count_utf8_bytes()
            + bool_u64(Property.Row.bidi_mirrored(row))
            + option_u64(Property.Row.bidi_mirroring_glyph(row))
            + option_u64(Property.Row.bidi_paired_bracket(row))
            + JoiningType.short(Property.Row.joining_type(row)).count_utf8_bytes()
            + JoiningGroup.short(Property.Row.joining_group(row)).count_utf8_bytes()
            + IndicSyllabicCategory.short(Property.Row.indic_syllabic_category(row)).count_utf8_bytes()
            + IndicPositionalCategory.short(Property.Row.indic_positional_category(row)).count_utf8_bytes()
            + bool_u64(Property.Row.default_ignorable(row))
            + bool_u64(Property.Row.variation_selector(row))
            + VerticalOrientation.short(Property.Row.vertical_orientation(row)).count_utf8_bytes()
            + emoji_bits(emoji)
    })
    checksum.to_str()
}

bool_u64 = |value| if value 1.U64 else 0.U64
option_u64 = |value| match value { Some(_) => 1.U64, None => 0.U64 }
emoji_bits = |value| bool_u64(value.emoji) + bool_u64(value.emoji_presentation) * 2 + bool_u64(value.emoji_modifier) * 4 + bool_u64(value.emoji_modifier_base) * 8 + bool_u64(value.emoji_component) * 16 + bool_u64(value.extended_pictographic) * 32
