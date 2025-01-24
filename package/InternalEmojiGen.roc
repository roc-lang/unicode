## The purpose of this file is to generate the InternalEmoji.roc file.
##
## This file will read the test data from `data/emoji-data.txt`
## parse it and then generate the implementation for each of the Emoji properties.
app [main!] {
    pf: platform "../../basic-cli/platform/main.roc",
    parser: "../../roc-parser/package/main.roc",
}

import pf.Stdout
import "data/emoji-data.txt" as file : Str
import Helpers exposing [CPMeta, PropertyMap]

main! = |_args| Stdout.line!(template)

EMOJIProp : [Emoji, Presentation, Modifier, Base, Component, Pictographic]
EMOJIMeta : { from_bytes : List U8, property : EMOJIProp, to_str : Str }

list_meta : List EMOJIMeta
list_meta =
    # NOTE ordering matters here to match on longest first
    [
        { from_bytes: Str.to_utf8("Extended_Pictographic"), property: Pictographic, to_str: "Pictographic" },
        { from_bytes: Str.to_utf8("Emoji_Modifier_Base"), property: Base, to_str: "Base" },
        { from_bytes: Str.to_utf8("Emoji_Modifier"), property: Modifier, to_str: "Modifier" },
        { from_bytes: Str.to_utf8("Emoji_Presentation"), property: Presentation, to_str: "Presentation" },
        { from_bytes: Str.to_utf8("Emoji_Component"), property: Component, to_str: "Component" },
        { from_bytes: Str.to_utf8("Emoji"), property: Emoji, to_str: "Emoji" },
    ]

template =
    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    module [EMOJI, from_cp, is_pictographic]
    import InternalCP exposing [CP, to_u32]

    ${prop_def_template}
    ${is_func_template}

    ${from_cp_template}
    """

prop_def_template : Str
prop_def_template =

    prop_strs =
        list_meta
        |> List.map(.to_str)
        |> List.map(|str| "${str}")
        |> Str.join_with(", ")

    """
    EMOJI : [${prop_strs}]
    """

is_func_template : Str
is_func_template =

    help : Str, EMOJIProp -> Str
    help = |name, current|
        exp =
            cps_for_property(current)
            |> List.map(Helpers.meta_to_expression)
            |> Str.join_with(" or ")

        """

        ${name} : U32 -> Bool
        ${name} = |u32| ${exp}
        """

    # For each EMOJIProp define a function that returns true if the given code point has that property
    list_meta
    |> List.keep_oks(
        |{ property }|
            when property is
                Emoji -> help("is_emoji", Emoji) |> Ok
                Presentation -> help("is_presentation", Presentation) |> Ok
                Modifier -> help("is_modifier", Modifier) |> Ok
                Base -> help("is_base", Base) |> Ok
                Component -> help("is_component", Component) |> Ok
                Pictographic -> help("is_pictographic", Pictographic) |> Ok,
    )
    |> Str.join_with("\n")

from_cp_template : Str
from_cp_template =
    """
    from_cp : CP -> Result EMOJI [NonEmojiCodePoint]
    from_cp = |cp|

        u32 = to_u32(cp)

        ${is_xtemp(list_meta, "")}
    """

# HELPERS

# Parse the file to map between code points and properties
file_map : List (PropertyMap EMOJIProp)
file_map = Helpers.property_map_from_file(file, parse_prop_part)

# Make a helper that returns a list of code points for the given property
cps_for_property : EMOJIProp -> List CPMeta
cps_for_property = |current|
    Helpers.filter_property_map(
        file_map,
        |{ cp, prop }| if prop == current then Ok(cp) else Err(NotNeeded),
    )

parse_prop_part : Str -> Result EMOJIProp [ParsingError]
parse_prop_part = |str|
    when Str.split_on(str, "#") is
        [prop_str, ..] -> emoji_prop_parser(Str.trim(prop_str))
        _ -> Err(ParsingError)

expect parse_prop_part(" Emoji                ") == Ok(Emoji)

emoji_prop_parser : Str -> Result EMOJIProp [ParsingError]
emoji_prop_parser = |input|

    starts_with_prop : EMOJIMeta -> Result EMOJIProp [NonPropSequence]
    starts_with_prop = |prop|
        if input |> Str.to_utf8 |> List.starts_with(prop.from_bytes) then
            Ok(prop.property)
        else
            Err(NonPropSequence)

    # see which properties match
    matches : List EMOJIProp
    matches = list_meta |> List.keep_oks(starts_with_prop)

    when matches is
        # take the longest match
        [a, ..] -> Ok(a)
        _ -> Err(ParsingError)

expect emoji_prop_parser("Extended_Pictographic") == Ok(Pictographic)
expect emoji_prop_parser("Emoji_Modifier_Base") == Ok(Base)
expect emoji_prop_parser("# ===") == Err(ParsingError)

# For each property, generate a function that returns true if the given code
# point has that property
is_xtemp : List EMOJIMeta, Str -> Str
is_xtemp = |props, buf|
    when List.first(props) is
        Err(ListWasEmpty) ->
            "${buf}\n        Err(NonEmojiCodePoint)\n"

        Ok(prop) ->
            when prop.property is
                Emoji ->
                    next = if_x_str("is_emoji", "Emoji")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Presentation ->
                    next = if_x_str("is_presentation", "Presentation")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Modifier ->
                    next = if_x_str("is_modifier", "Modifier")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Base ->
                    next = if_x_str("is_base", "Base")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Component ->
                    next = if_x_str("is_component", "Component")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

                Pictographic ->
                    next = if_x_str("is_pictographic", "Pictographic")
                    is_xtemp(List.drop_first(props, 1), "${buf}${next}")

if_x_str : Str, Str -> Str
if_x_str = |func_str, str|
    "if ${func_str}(u32) then\n        Ok(${str})\n    else "
