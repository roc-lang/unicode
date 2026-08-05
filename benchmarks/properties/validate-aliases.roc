app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.BidiClass
import unicode.BidiProperties
import unicode.CanonicalCombiningClass
import unicode.EastAsianWidth
import unicode.GeneralCategory
import unicode.IndicPositionalCategory
import unicode.IndicSyllabicCategory
import unicode.JoiningGroup
import unicode.JoiningType
import unicode.Property
import unicode.VerticalOrientation

run! : Str => Str
run! = |input| {
    if Bool.not(gc_alias(input)) { return "FAIL\tgc alias" }
    if Bool.not(ccc_alias(input)) { return "FAIL\tccc alias" }
    if Bool.not(eaw_alias(input)) { return "FAIL\teaw alias" }
    if Bool.not(bidi_aliases(input)) { return "FAIL\tbidi alias" }
    if Bool.not(shaping_aliases(input)) { return "FAIL\tshaping alias" }
    if Bool.not(binary_alias(input)) { return "FAIL\tbinary alias" }
    "PASS\tloose-aliases"
}

gc_alias = |input| match GeneralCategory.parse(runtime_str("is\tUppercase - Letter", input)) { Ok(value) => GeneralCategory.short(value) == "Lu", Err(_) => Bool.False }
ccc_alias = |input| match CanonicalCombiningClass.parse(runtime_str("is Above", input)) { Ok(value) => CanonicalCombiningClass.to_u8(value) == 230, Err(_) => Bool.False }
eaw_alias = |input| match EastAsianWidth.parse(runtime_str("is wide", input)) { Ok(value) => EastAsianWidth.short(value) == "W", Err(_) => Bool.False }

bidi_aliases = |input| {
    class_ok = match BidiClass.parse(runtime_str("is\tRight To Left", input)) { Ok(value) => BidiClass.short(value) == "R", Err(_) => Bool.False }
    bracket_ok = match BidiProperties.parse_paired_bracket_type(runtime_str("is open", input)) { Ok(Open) => Bool.True, _ => Bool.False }
    class_ok and bracket_ok
}

shaping_aliases = |input| {
    jt_ok = match JoiningType.parse(runtime_str("is-dual joining", input)) { Ok(value) => JoiningType.short(value) == "D", Err(_) => Bool.False }
    jg_ok = match JoiningGroup.parse(runtime_str("is No Joining Group", input)) { Ok(value) => JoiningGroup.short(value) == "No_Joining_Group", Err(_) => Bool.False }
    insc_ok = match IndicSyllabicCategory.parse(runtime_str("is consonant", input)) { Ok(value) => IndicSyllabicCategory.short(value) == "Consonant", Err(_) => Bool.False }
    inpc_ok = match IndicPositionalCategory.parse(runtime_str("is bottom", input)) { Ok(value) => IndicPositionalCategory.short(value) == "Bottom", Err(_) => Bool.False }
    vo_ok = match VerticalOrientation.parse(runtime_str("is transformed rotated", input)) { Ok(value) => VerticalOrientation.short(value) == "Tr", Err(_) => Bool.False }
    jt_ok and jg_ok and insc_ok and inpc_ok and vo_ok
}

binary_alias = |input| {
    true_ok = match Property.parse_binary(runtime_str("is\ttrue", input)) { Ok(Bool.True) => Bool.True, _ => Bool.False }
    exact_is_ok = match Property.parse_binary(runtime_str("IS", input)) { Err(_) => Bool.True, _ => Bool.False }
    true_ok and exact_is_ok
}

runtime_str = |value, input| value.concat(input).drop_last_bytes(input.count_utf8_bytes()) ?? value
