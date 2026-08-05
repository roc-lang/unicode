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

run! : Str => Str
run! = |_| {
    var code_point = 0.U64
    var scalar_count = 0.U64
    while code_point <= 0x10FFFF {
        match Scalar.from_u32(code_point.to_u32_wrap()) {
            Err(_) => {}
            Ok(scalar) => {
                if Bool.not(direct_row_match(scalar)) {
                    return "FAIL\tdirect/Row mismatch at U+${code_point.to_str()}"
                }
                scalar_count = scalar_count + 1
            }
        }
        code_point = code_point + 1
    }
    "PASS\tdirect-row\t${scalar_count.to_str()}"
}

direct_row_match = |scalar| {
    row = Property.of_scalar(scalar)
    if GeneralCategory.short(GeneralCategory.of_scalar(scalar)) != GeneralCategory.short(Property.Row.general_category(row)) { return Bool.False }
    if CanonicalCombiningClass.to_u8(CanonicalCombiningClass.of_scalar(scalar)) != CanonicalCombiningClass.to_u8(Property.Row.canonical_combining_class(row)) { return Bool.False }
    if EastAsianWidth.short(EastAsianWidth.of_scalar(scalar)) != EastAsianWidth.short(Property.Row.east_asian_width(row)) { return Bool.False }
    if BidiClass.short(BidiClass.of_scalar(scalar)) != BidiClass.short(Property.Row.bidi_class(row)) { return Bool.False }
    if BidiProperties.is_mirrored(scalar) != Property.Row.bidi_mirrored(row) { return Bool.False }
    if Bool.not(scalar_option_eq(BidiProperties.mirroring_glyph(scalar), Property.Row.bidi_mirroring_glyph(row))) { return Bool.False }
    if Bool.not(bracket_option_eq(BidiProperties.paired_bracket(scalar), Property.Row.bidi_paired_bracket(row))) { return Bool.False }
    if JoiningType.short(JoiningType.of_scalar(scalar)) != JoiningType.short(Property.Row.joining_type(row)) { return Bool.False }
    if JoiningGroup.short(JoiningGroup.of_scalar(scalar)) != JoiningGroup.short(Property.Row.joining_group(row)) { return Bool.False }
    if IndicSyllabicCategory.short(IndicSyllabicCategory.of_scalar(scalar)) != IndicSyllabicCategory.short(Property.Row.indic_syllabic_category(row)) { return Bool.False }
    if IndicPositionalCategory.short(IndicPositionalCategory.of_scalar(scalar)) != IndicPositionalCategory.short(Property.Row.indic_positional_category(row)) { return Bool.False }
    if Property.is_default_ignorable(scalar) != Property.Row.default_ignorable(row) { return Bool.False }
    if Property.is_variation_selector(scalar) != Property.Row.variation_selector(row) { return Bool.False }
    if VerticalOrientation.short(VerticalOrientation.of_scalar(scalar)) != VerticalOrientation.short(Property.Row.vertical_orientation(row)) { return Bool.False }
    emoji_eq(Emoji.of_scalar(scalar), Property.Row.emoji(row))
}

scalar_option_eq = |left, right| match (left, right) {
    (None, None) => Bool.True
    (Some(left_scalar), Some(right_scalar)) => Scalar.is_eq(left_scalar, right_scalar)
    _ => Bool.False
}

bracket_option_eq = |left, right| match (left, right) {
    (None, None) => Bool.True
    (Some(left_pair), Some(right_pair)) => Scalar.is_eq(left_pair.scalar, right_pair.scalar) and left_pair.kind == right_pair.kind
    _ => Bool.False
}

emoji_eq = |left, right| {
    left.emoji == right.emoji
        and left.emoji_presentation == right.emoji_presentation
        and left.emoji_modifier == right.emoji_modifier
        and left.emoji_modifier_base == right.emoji_modifier_base
        and left.emoji_component == right.emoji_component
        and left.extended_pictographic == right.extended_pictographic
}
