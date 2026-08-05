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
import Signature

## Benchmark the independent narrow views when a caller asks for the complete
## bounded property family one field at a time.
run! : Str => Str
run! = |source| {
    var checksum = 0xCBF29CE484222325.U64
    for located in Scalar.iter(source) {
        scalar = located.scalar
        emoji = Emoji.of_scalar(scalar)
        checksum = Signature.mix(checksum, Scalar.to_u32(scalar).to_u64())
        checksum = Signature.mix_str(checksum, GeneralCategory.short(GeneralCategory.of_scalar(scalar)))
        checksum = Signature.mix(checksum, CanonicalCombiningClass.to_u8(CanonicalCombiningClass.of_scalar(scalar)).to_u64())
        checksum = Signature.mix_str(checksum, EastAsianWidth.short(EastAsianWidth.of_scalar(scalar)))
        checksum = Signature.mix_str(checksum, BidiClass.short(BidiClass.of_scalar(scalar)))
        checksum = Signature.mix_bool(checksum, BidiProperties.is_mirrored(scalar))
        checksum = mix_scalar_option(checksum, BidiProperties.mirroring_glyph(scalar))
        checksum = mix_bracket_option(checksum, BidiProperties.paired_bracket(scalar))
        checksum = Signature.mix_str(checksum, JoiningType.short(JoiningType.of_scalar(scalar)))
        checksum = Signature.mix_str(checksum, JoiningGroup.short(JoiningGroup.of_scalar(scalar)))
        checksum = Signature.mix_str(checksum, IndicSyllabicCategory.short(IndicSyllabicCategory.of_scalar(scalar)))
        checksum = Signature.mix_str(checksum, IndicPositionalCategory.short(IndicPositionalCategory.of_scalar(scalar)))
        checksum = Signature.mix_bool(checksum, Property.is_default_ignorable(scalar))
        checksum = Signature.mix_bool(checksum, Property.is_variation_selector(scalar))
        checksum = Signature.mix_str(checksum, VerticalOrientation.short(VerticalOrientation.of_scalar(scalar)))
        checksum = mix_emoji(checksum, emoji)
    }
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
