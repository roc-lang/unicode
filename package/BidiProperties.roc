import InternalBidiProperties
import InternalLooseAlias
import Scalar

## Scalar mappings and binary facts used by UAX #9, sourced from
## `UnicodeData.txt`, `BidiMirroring.txt`, and `BidiBrackets.txt`. These queries
## expose Unicode character data; they do not perform paragraph analysis,
## mirroring, or visual reordering.
BidiProperties :: [].{
    PropertyName : { short : Str, long : Str }
    PairedBracketType : [None, Open, Close]
    PairedBracket : { scalar : Scalar, kind : [Open, Close] }
    AliasError : [UnknownAlias]

    mirrored_property : PropertyName
    mirrored_property = { short: "Bidi_M", long: "Bidi_Mirrored" }

    mirroring_glyph_property : PropertyName
    mirroring_glyph_property = { short: "bmg", long: "Bidi_Mirroring_Glyph" }

    paired_bracket_property : PropertyName
    paired_bracket_property = { short: "bpb", long: "Bidi_Paired_Bracket" }

    paired_bracket_type_property : PropertyName
    paired_bracket_type_property = { short: "bpt", long: "Bidi_Paired_Bracket_Type" }

    is_mirrored : Scalar -> Bool
    is_mirrored = |scalar| InternalBidiProperties.is_mirrored(Scalar.to_u32(scalar))

    ## Return the optional character-based best-fit mirror mapping. Absence is
    ## meaningful even when `is_mirrored` is true.
    mirroring_glyph : Scalar -> [Some(Scalar), None]
    mirroring_glyph = |scalar| {
        match InternalBidiProperties.mirroring_glyph(Scalar.to_u32(scalar)) {
            None => None
            Some(target) => {
                # Generation rejects every non-scalar endpoint. Crashing on a
                # violated generated-data invariant is deliberate: absence is
                # a real Unicode answer and must not conceal corrupt data.
                mapped = match Scalar.from_u32(target) {
                    Ok(value) => value
                    Err(_) => { crash "generated bidi mirror target is not a Unicode scalar" }
                }
                Some(mapped)
            }
        }
    }

    ## Return the optional reciprocal bracket mapping and open/close type.
    paired_bracket : Scalar -> [Some(PairedBracket), None]
    paired_bracket = |scalar| {
        match InternalBidiProperties.paired_bracket(Scalar.to_u32(scalar)) {
            None => None
            Some(pair) => {
                # See `mirroring_glyph`: invalid generated targets are faults,
                # never semantically meaningful missing mappings.
                mapped = match Scalar.from_u32(pair.scalar) {
                    Ok(value) => value
                    Err(_) => { crash "generated bidi bracket target is not a Unicode scalar" }
                }
                Some({ scalar: mapped, kind: pair.kind })
            }
        }
    }

    paired_bracket_type : Scalar -> PairedBracketType
    paired_bracket_type = |scalar| {
        match BidiProperties.paired_bracket(scalar) {
            None => None
            Some(pair) => match pair.kind { Open => Open, Close => Close }
        }
    }

    paired_bracket_type_short : PairedBracketType -> Str
    paired_bracket_type_short = |kind| match kind { None => "n", Open => "o", Close => "c" }

    paired_bracket_type_long : PairedBracketType -> Str
    paired_bracket_type_long = |kind| match kind { None => "None", Open => "Open", Close => "Close" }

    parse_paired_bracket_type : Str -> Try(PairedBracketType, AliasError)
    parse_paired_bracket_type = |name| {
        if InternalLooseAlias.matches(name, "n") or InternalLooseAlias.matches(name, "None") {
            Ok(None)
        } else if InternalLooseAlias.matches(name, "o") or InternalLooseAlias.matches(name, "Open") {
            Ok(Open)
        } else if InternalLooseAlias.matches(name, "c") or InternalLooseAlias.matches(name, "Close") {
            Ok(Close)
        } else {
            Err(UnknownAlias)
        }
    }

    paired_bracket_type_alias_count : PairedBracketType -> U8
    paired_bracket_type_alias_count = |_| 2

    paired_bracket_type_alias_at : PairedBracketType, U8 -> [Some(Str), None]
    paired_bracket_type_alias_at = |kind, index| {
        if index == 0 Some(BidiProperties.paired_bracket_type_short(kind))
        else if index == 1 Some(BidiProperties.paired_bracket_type_long(kind))
        else None
    }

    mirrored_short : Bool -> Str
    mirrored_short = |value| if value "Y" else "N"

    mirrored_long : Bool -> Str
    mirrored_long = |value| if value "Yes" else "No"

    mirrored_alias_count : Bool -> U8
    mirrored_alias_count = |_| 4

    mirrored_alias_at : Bool, U8 -> [Some(Str), None]
    mirrored_alias_at = |value, index| {
        if value {
            match index { 0 => Some("Y"), 1 => Some("Yes"), 2 => Some("T"), 3 => Some("True"), _ => None }
        } else {
            match index { 0 => Some("N"), 1 => Some("No"), 2 => Some("F"), 3 => Some("False"), _ => None }
        }
    }

    parse_mirrored : Str -> Try(Bool, AliasError)
    parse_mirrored = |name| {
        if InternalLooseAlias.matches(name, "Y") or InternalLooseAlias.matches(name, "Yes") or InternalLooseAlias.matches(name, "T") or InternalLooseAlias.matches(name, "True") { Ok(Bool.True) }
        else if InternalLooseAlias.matches(name, "N") or InternalLooseAlias.matches(name, "No") or InternalLooseAlias.matches(name, "F") or InternalLooseAlias.matches(name, "False") { Ok(Bool.False) }
        else { Err(UnknownAlias) }
    }
}
