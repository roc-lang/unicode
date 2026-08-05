import InternalEAW
import Scalar

## The Unicode East_Asian_Width scalar property.
##
## This is a Unicode fact, not a terminal-column or glyph-advance policy.
EastAsianWidth :: [].{
    Value := [Fullwidth, Wide, Ambiguous, Halfwidth, Neutral, Narrow].{
        is_eq : _
    }

    ## Look up a scalar's East_Asian_Width value.
    ##
    ## This is bounded constant time, does not allocate, and accepts `Scalar`
    ## so surrogate code points cannot receive unassigned-scalar defaults.
    of_scalar : Scalar -> Value
    of_scalar = |scalar| {
        match InternalEAW.east_asian_width_property(Scalar.to_u32(scalar)) {
            F => Fullwidth
            W => Wide
            A => Ambiguous
            H => Halfwidth
            N => Neutral
            Na => Narrow
        }
    }
}
