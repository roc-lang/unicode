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
import unicode.Scalar
import Signature

## Benchmark one fused row-id lookup per scalar, lazy column access, and the
## single-decode complete-string fold.
run! : Str => Str
run! = |source| {
    checksum = Property.fold(source, 0xCBF29CE484222325.U64, |initial, entry| {
        row = entry.row
        emoji = Property.Row.emoji(row)
        scalar = entry.located.scalar
        var state = Signature.mix(initial, Scalar.to_u32(scalar).to_u64())
        state = Signature.mix_str(state, GeneralCategory.short(Property.Row.general_category(row)))
        state = Signature.mix(state, CanonicalCombiningClass.to_u8(Property.Row.canonical_combining_class(row)).to_u64())
        state = Signature.mix_str(state, EastAsianWidth.short(Property.Row.east_asian_width(row)))
        state = Signature.mix_str(state, BidiClass.short(Property.Row.bidi_class(row)))
        state = Signature.mix_bool(state, Property.Row.bidi_mirrored(row))
        state = mix_scalar_option(state, Property.Row.bidi_mirroring_glyph(row))
        state = mix_bracket_option(state, Property.Row.bidi_paired_bracket(row))
        state = Signature.mix_str(state, JoiningType.short(Property.Row.joining_type(row)))
        state = Signature.mix_str(state, JoiningGroup.short(Property.Row.joining_group(row)))
        state = Signature.mix_str(state, IndicSyllabicCategory.short(Property.Row.indic_syllabic_category(row)))
        state = Signature.mix_str(state, IndicPositionalCategory.short(Property.Row.indic_positional_category(row)))
        state = Signature.mix_bool(state, Property.Row.default_ignorable(row))
        state = Signature.mix_bool(state, Property.Row.variation_selector(row))
        state = Signature.mix_str(state, VerticalOrientation.short(Property.Row.vertical_orientation(row)))
        mix_emoji(state, emoji)
    })
    checksum.to_str()
}

mix_scalar_option = |state, value| match value {
    None => Signature.mix(state, 0)
    Some(scalar) => Signature.mix(Signature.mix(state, 1), Scalar.to_u32(scalar).to_u64())
}

mix_bracket_option = |state, value| match value {
    None => Signature.mix(state, 0)
    Some(pair) => {
        with_scalar = Signature.mix(Signature.mix(state, 1), Scalar.to_u32(pair.scalar).to_u64())
        Signature.mix(with_scalar, match pair.kind { Open => 1, Close => 2 })
    }
}

mix_emoji = |initial, value| {
    var state = initial
    state = Signature.mix_bool(state, value.emoji)
    state = Signature.mix_bool(state, value.emoji_presentation)
    state = Signature.mix_bool(state, value.emoji_modifier)
    state = Signature.mix_bool(state, value.emoji_modifier_base)
    state = Signature.mix_bool(state, value.emoji_component)
    Signature.mix_bool(state, value.extended_pictographic)
}
