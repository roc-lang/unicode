import InternalEAW
import InternalLooseAlias
import InternalPropertyRuns
import Scalar
import TextRange

## The Unicode East_Asian_Width scalar property from `EastAsianWidth.txt`.
##
## This is a Unicode fact, not a terminal-column or glyph-advance policy.
EastAsianWidth :: [].{
    Value := [Fullwidth, Wide, Ambiguous, Halfwidth, Neutral, Narrow].{
        is_eq : _
    }
    PropertyName : { short : Str, long : Str }
    Run : { range : TextRange, value : Value }
    AliasError : [UnknownAlias]

    property_name : PropertyName
    property_name = { short: "ea", long: "East_Asian_Width" }

    ## Look up a scalar's East_Asian_Width value.
    ##
    ## This is bounded constant time, does not allocate, and accepts `Scalar`
    ## so surrogate code points cannot receive unassigned-scalar defaults.
    of_scalar : Scalar -> Value
    of_scalar = |scalar| of_u32(Scalar.to_u32(scalar))

    short : Value -> Str
    short = |value| match value { Ambiguous => "A", Fullwidth => "F", Halfwidth => "H", Neutral => "N", Narrow => "Na", Wide => "W" }

    long : Value -> Str
    long = |value| match value { Ambiguous => "Ambiguous", Fullwidth => "Fullwidth", Halfwidth => "Halfwidth", Neutral => "Neutral", Narrow => "Narrow", Wide => "Wide" }

    alias_count : Value -> U8
    alias_count = |_| 2

    alias_at : Value, U8 -> [Some(Str), None]
    alias_at = |value, index| if index == 0 Some(EastAsianWidth.short(value)) else if index == 1 Some(EastAsianWidth.long(value)) else None

    parse : Str -> Try(Value, AliasError)
    parse = |name| {
        if alias_matches(name, "A", "Ambiguous") { Ok(Ambiguous) }
        else if alias_matches(name, "F", "Fullwidth") { Ok(Fullwidth) }
        else if alias_matches(name, "H", "Halfwidth") { Ok(Halfwidth) }
        else if alias_matches(name, "N", "Neutral") { Ok(Neutral) }
        else if alias_matches(name, "Na", "Narrow") { Ok(Narrow) }
        else if alias_matches(name, "W", "Wide") { Ok(Wide) }
        else { Err(UnknownAlias) }
    }

    fold_runs : Str, state, (state, Run -> state) -> state
    fold_runs = |source, initial, emit| InternalPropertyRuns.fold(source, initial, of_u32, |left, right| left == right, emit)

    iter_runs : Str -> Iter(Run)
    iter_runs = |source| InternalPropertyRuns.iter(source, of_u32, |left, right| left == right)
}

of_u32 = |value| {
    match InternalEAW.east_asian_width_property(value) {
        F => Fullwidth
        W => Wide
        A => Ambiguous
        H => Halfwidth
        N => Neutral
        Na => Narrow
    }
}

alias_matches = |name, short, long| InternalLooseAlias.matches(name, short) or InternalLooseAlias.matches(name, long)
