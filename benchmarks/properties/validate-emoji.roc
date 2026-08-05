app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.Emoji
import unicode.Scalar

run! : Str => Str
run! = |input| {
    salt = input.count_utf8_bytes().to_u32_wrap()
    face = runtime_scalar(0x1F600, salt)
    skin = runtime_scalar(0x1F3FD, salt)
    heart = runtime_scalar(0x2764, salt)
    vs15 = runtime_scalar(0xFE0E, salt)
    vs16 = runtime_scalar(0xFE0F, salt)
    letter = runtime_scalar(0x0041, salt)
    if emoji_bits(Emoji.of_scalar(face)) != 35 { return "FAIL\temoji-face" }
    if emoji_bits(Emoji.of_scalar(skin)) != 23 { return "FAIL\temoji-modifier" }
    if emoji_bits(Emoji.of_scalar(heart)) != 33 { return "FAIL\temoji-heart" }
    if Emoji.variation_presentation(heart, vs15) != Some(Text) { return "FAIL\tvariation-text" }
    if Emoji.variation_presentation(heart, vs16) != Some(Emoji) { return "FAIL\tvariation-emoji" }
    if Emoji.variation_presentation(letter, vs16) != None { return "FAIL\tvariation-negative" }
    "PASS\temoji-values"
}

runtime_scalar = |code_point, salt| Scalar.from_u32(code_point.bitwise_xor(salt).bitwise_xor(salt)) ?? ...

emoji_bits = |value| {
    bit(value.emoji) + bit(value.emoji_presentation) * 2 + bit(value.emoji_modifier) * 4 + bit(value.emoji_modifier_base) * 8 + bit(value.emoji_component) * 16 + bit(value.extended_pictographic) * 32
}

bit = |value| if value 1.U64 else 0.U64
