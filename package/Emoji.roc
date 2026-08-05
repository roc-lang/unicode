import InternalEmojiData
import InternalEmojiVariations
import InternalPropertyRuns
import InternalLooseAlias
import Scalar
import TextRange

## The six bounded Unicode Emoji scalar properties from `emoji-data.txt`, plus
## valid VS15/VS16 presentation requests from
## `emoji-variation-sequences.txt`. This is not an RGI or ZWJ sequence parser.
Emoji :: [].{
    Properties : InternalEmojiData.Properties
    Presentation : [Text, Emoji]
    PropertyName : { short : Str, long : Str }
    Run : { range : TextRange, value : Properties }
    AliasError : [UnknownAlias]

    emoji_property : PropertyName
    emoji_property = { short: "Emoji", long: "Emoji" }

    presentation_property : PropertyName
    presentation_property = { short: "EPres", long: "Emoji_Presentation" }

    modifier_property : PropertyName
    modifier_property = { short: "EMod", long: "Emoji_Modifier" }

    modifier_base_property : PropertyName
    modifier_base_property = { short: "EBase", long: "Emoji_Modifier_Base" }

    component_property : PropertyName
    component_property = { short: "EComp", long: "Emoji_Component" }

    extended_pictographic_property : PropertyName
    extended_pictographic_property = { short: "ExtPict", long: "Extended_Pictographic" }

    of_scalar : Scalar -> Properties
    of_scalar = |scalar| InternalEmojiData.lookup(Scalar.to_u32(scalar))

    is_emoji : Scalar -> Bool
    is_emoji = |scalar| Emoji.of_scalar(scalar).emoji

    has_default_emoji_presentation : Scalar -> Bool
    has_default_emoji_presentation = |scalar| Emoji.of_scalar(scalar).emoji_presentation

    is_modifier : Scalar -> Bool
    is_modifier = |scalar| Emoji.of_scalar(scalar).emoji_modifier

    is_modifier_base : Scalar -> Bool
    is_modifier_base = |scalar| Emoji.of_scalar(scalar).emoji_modifier_base

    is_component : Scalar -> Bool
    is_component = |scalar| Emoji.of_scalar(scalar).emoji_component

    is_extended_pictographic : Scalar -> Bool
    is_extended_pictographic = |scalar| Emoji.of_scalar(scalar).extended_pictographic

    ## Return a presentation only when the exact base/selector pair is listed
    ## by Unicode. Arbitrary variation-selector pairs return `None`.
    variation_presentation : Scalar, Scalar -> [Some(Presentation), None]
    variation_presentation = |base, selector| {
        InternalEmojiVariations.lookup(Scalar.to_u32(base), Scalar.to_u32(selector))
    }

    binary_short : Bool -> Str
    binary_short = |value| if value "Y" else "N"

    binary_long : Bool -> Str
    binary_long = |value| if value "Yes" else "No"

    binary_alias_count : Bool -> U8
    binary_alias_count = |_| 4

    binary_alias_at : Bool, U8 -> [Some(Str), None]
    binary_alias_at = |value, index| {
        if value {
            match index { 0 => Some("Y"), 1 => Some("Yes"), 2 => Some("T"), 3 => Some("True"), _ => None }
        } else {
            match index { 0 => Some("N"), 1 => Some("No"), 2 => Some("F"), 3 => Some("False"), _ => None }
        }
    }

    parse_binary : Str -> Try(Bool, AliasError)
    parse_binary = |name| {
        if InternalLooseAlias.matches(name, "Y") or InternalLooseAlias.matches(name, "Yes") or InternalLooseAlias.matches(name, "T") or InternalLooseAlias.matches(name, "True") { Ok(Bool.True) }
        else if InternalLooseAlias.matches(name, "N") or InternalLooseAlias.matches(name, "No") or InternalLooseAlias.matches(name, "F") or InternalLooseAlias.matches(name, "False") { Ok(Bool.False) }
        else { Err(UnknownAlias) }
    }

    fold_runs : Str, state, (state, Run -> state) -> state
    fold_runs = |source, initial, emit| {
        InternalPropertyRuns.fold(
            source,
            initial,
            InternalEmojiData.lookup,
            properties_eq,
            emit,
        )
    }

    iter_runs : Str -> Iter(Run)
    iter_runs = |source| {
        InternalPropertyRuns.iter(source, InternalEmojiData.lookup, properties_eq)
    }
}

properties_eq = |left, right| {
    left.emoji == right.emoji
        and left.emoji_presentation == right.emoji_presentation
        and left.emoji_modifier == right.emoji_modifier
        and left.emoji_modifier_base == right.emoji_modifier_base
        and left.emoji_component == right.emoji_component
        and left.extended_pictographic == right.extended_pictographic
}
