## The purpose of this file is to generate the InternalGBP.roc file.
##
## This file will read the test data from `data/GraphemeBreakProperty-15.1.0.txt`
## parse it and then generate the implementation for each of the GBP properties.
app [main!] {
    pf: platform "../../basic-cli/platform/main.roc",
    parser: "../../roc-parser/package/main.roc",
}

import pf.Stdout
import "data/GraphemeBreakProperty-15.1.0.txt" as file : Str
import Helpers exposing [CPMeta, PropertyMap]

main! = |_args| Stdout.line!(template)

GBPProp : [CR, LF, Control, Extend, ZWJ, RI, Prepend, SpacingMark, L, V, T, LV, LVT, Other]
GBPMeta : { from_bytes : List U8, property : GBPProp, to_str : Str }

list_meta : List GBPMeta
list_meta =
    # NOTE ordering matters here, e.g. L after LV and LVT
    # to match on longest first
    [
        { from_bytes: Str.to_utf8("CR"), property: CR, to_str: "CR" },
        { from_bytes: Str.to_utf8("Control"), property: Control, to_str: "Control" },
        { from_bytes: Str.to_utf8("Extend"), property: Extend, to_str: "Extend" },
        { from_bytes: Str.to_utf8("ZWJ"), property: ZWJ, to_str: "ZWJ" },
        { from_bytes: Str.to_utf8("Regional_Indicator"), property: RI, to_str: "RI" },
        { from_bytes: Str.to_utf8("Prepend"), property: Prepend, to_str: "Prepend" },
        { from_bytes: Str.to_utf8("SpacingMark"), property: SpacingMark, to_str: "SpacingMark" },
        { from_bytes: Str.to_utf8("V"), property: V, to_str: "V" },
        { from_bytes: Str.to_utf8("T"), property: T, to_str: "T" },
        { from_bytes: Str.to_utf8("LF"), property: LF, to_str: "LF" },
        { from_bytes: Str.to_utf8("LVT"), property: LVT, to_str: "LVT" },
        { from_bytes: Str.to_utf8("LV"), property: LV, to_str: "LV" },
        { from_bytes: Str.to_utf8("L"), property: L, to_str: "L" },
        { from_bytes: Str.to_utf8("Other"), property: Other, to_str: "Other" },
    ]

template =
    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    module [GBP, from_cp, is_extend, is_zwj]
    import InternalCP exposing [CP, to_u32, from_u32_unchecked]

    ${prop_def_template}
    ${is_func_template}

    ${from_cp_template}
    ${tests_template}
    """

prop_def_template : Str
prop_def_template =

    prop_strs =
        list_meta
        |> List.map(.to_str)
        |> List.map(|str| "${str}")
        |> Str.join_with(", ")

    """
    GBP : [${prop_strs}]
    """

is_func_template : Str
is_func_template =

    help : Str, GBPProp -> Str
    help = |name, current|
        exp =
            cps_for_property(current)
            |> List.map(Helpers.meta_to_expression)
            |> Str.join_with(" or ")

        """

        ${name} : U32 -> Bool
        ${name} = |u32| ${exp}
        """

    # For each GBPProp define a function that returns true if the given code point has that property
    list_meta
    |> List.keep_oks(
        |{ property }|
            when property is
                CR -> help("is_cr", CR) |> Ok
                LF -> help("is_lf", LF) |> Ok
                Control -> help("is_control", Control) |> Ok
                Extend -> help("is_extend", Extend) |> Ok
                ZWJ -> help("is_zwj", ZWJ) |> Ok
                RI -> help("is_ri", RI) |> Ok
                Prepend -> help("is_prepend", Prepend) |> Ok
                SpacingMark -> help("is_spacing_mark", SpacingMark) |> Ok
                L -> help("is_l", L) |> Ok
                V -> help("is_v", V) |> Ok
                T -> help("is_t", T) |> Ok
                LV -> help("is_lv", LV) |> Ok
                LVT -> help("is_lvt", LVT) |> Ok
                Other -> Err(NotUsed),
    )
    |> Str.join_with("\n")

from_cp_template : Str
from_cp_template =
    """
    from_cp : CP -> GBP
    from_cp = |cp|
        u32 = to_u32(cp)

        ${is_xtemp(list_meta, "")}
    """

tests_template : Str
tests_template =
    [
        ("000D", "CR"),
        ("000A", "LF"),
        ("200D", "ZWJ"),
        ("17BF", "SpacingMark"),
        ("A960", "L"),
        ("D7C6", "V"),
        ("11A8", "T"),
        ("AC00", "LV"),
        ("AC04", "LVT"),
        ("B93D", "LVT"),
        ("0041", "Other"),
        ("D7CD", "T"),
        ("1160", "V"),
        ("D7C6", "V"),
        ("1E2AE", "Extend"),
        ("FFF0", "Control"),
        ("1D17A", "Control"),
        ("06DD", "Prepend"),
    ]
    |> List.map(unicode_hex_to_test)
    |> Str.join_with("\n\n")

# HELPERS

parse_prop_part : Str -> Result GBPProp [ParsingError]
parse_prop_part = |str|
    when Str.split_on(str, "#") is
        [prop_str, ..] -> gbp_prop_parser(Str.trim(prop_str))
        _ -> Err(ParsingError)

expect parse_prop_part(" Prepend # Cf   [6] ARABIC NUMBER SIGN..ARABIC NUMBER MARK ABOVE") == Ok(Prepend)
expect parse_prop_part(" CR # Cc       <control-000D>") == Ok(CR)
expect parse_prop_part(" Regional_Indicator # So  [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z") == Ok(RI)

# Parse the file to map between code points and properties
file_map : List (PropertyMap GBPProp)
file_map = Helpers.property_map_from_file(file, parse_prop_part)

# Make a helper that returns a list of code points for the given property
cps_for_property : GBPProp -> List CPMeta
cps_for_property = |current|
    Helpers.filter_property_map(
        file_map,
        |{ cp, prop }| if prop == current then Ok(cp) else Err(NotNeeded),
    )

# For each property, generate a function that returns true if the given code
# point has that property
is_xtemp : List GBPMeta, Str -> Str
is_xtemp = |props, buf|
    when List.first(props) is
        Err(ListWasEmpty) ->
            "${buf}\n        Other\n"

        Ok(prop) ->
            when prop.property is
                CR ->
                    next = if_x_str("is_cr", "CR")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                LF ->
                    next = if_x_str("is_lf", "LF")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Control ->
                    next = if_x_str("is_control", "Control")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Extend ->
                    next = if_x_str("is_extend", "Extend")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                ZWJ ->
                    next = if_x_str("is_zwj", "ZWJ")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                RI ->
                    next = if_x_str("is_ri", "RI")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Prepend ->
                    next = if_x_str("is_prepend", "Prepend")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                SpacingMark ->
                    next = if_x_str("is_spacing_mark", "SpacingMark")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                L ->
                    next = if_x_str("is_l", "L")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                V ->
                    next = if_x_str("is_v", "V")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                T ->
                    next = if_x_str("is_t", "T")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                LV ->
                    next = if_x_str("is_lv", "LV")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                LVT ->
                    next = if_x_str("is_lvt", "LVT")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Other ->
                    is_xtemp(List.drop_first(props, 1), buf)

if_x_str : Str, Str -> Str
if_x_str = |func_str, str|
    "if ${func_str}(u32) then\n        ${str}\n    else "

# Helper to manually generate a test
unicode_hex_to_test : (Str, Str) -> Str
unicode_hex_to_test = |(hex, gbp_expected)|
    u32 = hex |> Str.to_utf8 |> Helpers.hex_bytes_to_u32

    """
    expect # test U+${hex} gives ${gbp_expected}
        gbp = from_cp(from_u32_unchecked(${Num.to_str(u32)}))
        gbp == ${gbp_expected}
    """

gbp_prop_parser : Str -> Result GBPProp [ParsingError]
gbp_prop_parser = |input|
    starts_with_prop : GBPMeta -> Result GBPProp [NonPropSequence]
    starts_with_prop = |prop|
        if input |> Str.to_utf8 |> List.starts_with(prop.from_bytes) then
            Ok(prop.property)
        else
            Err(NonPropSequence)

    # see which properties match
    matches : List GBPProp
    matches = list_meta |> List.keep_oks(starts_with_prop)

    when matches is
        # take the longest match
        [a, ..] -> Ok(a)
        _ -> Err(ParsingError)

expect gbp_prop_parser("L") == Ok(L)
expect gbp_prop_parser("LF") == Ok(LF)
expect gbp_prop_parser("LV") == Ok(LV)
expect gbp_prop_parser("LVT") == Ok(LVT)
expect gbp_prop_parser("Other") == Ok(Other)
expect gbp_prop_parser("# ===") == Err(ParsingError)
