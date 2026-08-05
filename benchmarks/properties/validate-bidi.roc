app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.BidiClass
import unicode.BidiProperties
import unicode.Scalar

run! : Str => Str
run! = |input| {
    salt = input.count_utf8_bytes().to_u32_wrap()
    open = runtime_scalar(0x0028, salt)
    close = runtime_scalar(0x0029, salt)
    absent = runtime_scalar(0x2140, salt)
    if Bool.not(BidiProperties.is_mirrored(open)) { return "FAIL\tmirrored" }
    if Bool.not(scalar_option_is(BidiProperties.mirroring_glyph(open), 0x0029)) { return "FAIL\tmirror-target" }
    if Bool.not(bracket_option_is(BidiProperties.paired_bracket(open), 0x0029, Open)) { return "FAIL\topen-bracket" }
    if Bool.not(bracket_option_is(BidiProperties.paired_bracket(close), 0x0028, Close)) { return "FAIL\tclose-bracket" }
    if Bool.not(BidiProperties.is_mirrored(absent)) { return "FAIL\tmirrored-without-map" }
    if BidiProperties.mirroring_glyph(absent) != None { return "FAIL\tmissing-mirror" }
    if BidiProperties.paired_bracket(absent) != None { return "FAIL\tmissing-bracket" }
    if Bool.not(defaults_match(salt)) { return "FAIL\tbidi-defaults" }
    "PASS\tbidi-values"
}

defaults_match = |salt| {
    expected = [
        (0x0378.U32, "L"), (0x0590, "R"), (0x070E, "AL"), (0x07FB, "R"),
        (0x086B, "AL"), (0x20C2, "ET"), (0xFB37, "R"), (0xFE75, "AL"),
        (0x10806, "R"), (0x10D28, "AL"), (0x10D66, "R"), (0x10EC0, "AL"),
        (0x10F28, "R"), (0x10F5A, "AL"), (0x10F8A, "R"), (0x1E8C5, "R"),
        (0x1EC70, "AL"), (0x1ECC0, "R"), (0x1ED00, "AL"), (0x1ED50, "R"),
        (0x1EE04, "AL"), (0x1EF00, "R"),
    ]
    for (code_point, bidi) in expected {
        scalar = runtime_scalar(code_point, salt)
        if BidiClass.short(BidiClass.of_scalar(scalar)) != bidi { return Bool.False }
    }
    Bool.True
}

runtime_scalar = |code_point, salt| Scalar.from_u32(code_point.bitwise_xor(salt).bitwise_xor(salt)) ?? ...

scalar_option_is = |value, expected| match value { Some(mapped) => Scalar.to_u32(mapped) == expected, None => Bool.False }
bracket_option_is = |value, expected, kind| match value { Some(pair) => Scalar.to_u32(pair.scalar) == expected and pair.kind == kind, None => Bool.False }
