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
import unicode.Scalar
import unicode.VerticalOrientation

run! : Str => Str
run! = |input| {
    salt = input.count_utf8_bytes().to_u32_wrap()
    acute = runtime_scalar(0x0301, salt)
    virama = runtime_scalar(0x094D, salt)
    alef = runtime_scalar(0x0627, salt)
    ka = runtime_scalar(0x0915, salt)
    zwsp = runtime_scalar(0x200B, salt)
    vs16 = runtime_scalar(0xFE0F, salt)
    ideograph = runtime_scalar(0x4E00, salt)
    if GeneralCategory.short(GeneralCategory.of_scalar(acute)) != "Mn" { return "FAIL\tgc" }
    if CanonicalCombiningClass.to_u8(CanonicalCombiningClass.of_scalar(acute)) != 230 { return "FAIL\tccc-230" }
    if CanonicalCombiningClass.to_u8(CanonicalCombiningClass.of_scalar(virama)) != 9 { return "FAIL\tccc-9" }
    if EastAsianWidth.short(EastAsianWidth.of_scalar(acute)) != "A" { return "FAIL\teaw" }
    if BidiClass.short(BidiClass.of_scalar(acute)) != "NSM" { return "FAIL\tbidi" }
    if JoiningType.short(JoiningType.of_scalar(alef)) != "R" { return "FAIL\tjt" }
    if JoiningGroup.short(JoiningGroup.of_scalar(alef)) != "Alef" { return "FAIL\tjg" }
    if IndicSyllabicCategory.short(IndicSyllabicCategory.of_scalar(ka)) != "Consonant" { return "FAIL\tinsc-consonant" }
    if IndicSyllabicCategory.short(IndicSyllabicCategory.of_scalar(virama)) != "Virama" { return "FAIL\tinsc-virama" }
    if IndicPositionalCategory.short(IndicPositionalCategory.of_scalar(virama)) != "Bottom" { return "FAIL\tinpc" }
    if Bool.not(Property.is_default_ignorable(zwsp)) { return "FAIL\tdi-zwsp" }
    if Bool.not(Property.is_default_ignorable(vs16)) { return "FAIL\tdi-vs16" }
    if Property.is_variation_selector(zwsp) { return "FAIL\tvs-negative" }
    if Bool.not(Property.is_variation_selector(vs16)) { return "FAIL\tvs-positive" }
    if VerticalOrientation.short(VerticalOrientation.of_scalar(ideograph)) != "U" { return "FAIL\tvo" }
    "PASS\tdense-values"
}

runtime_scalar = |code_point, salt| Scalar.from_u32(code_point.bitwise_xor(salt).bitwise_xor(salt)) ?? ...
