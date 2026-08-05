import InternalJoiningGroup
import InternalPropertyRuns
import Scalar
import TextRange

## Unicode `Joining_Group` (`jg`) from `ArabicShaping.txt`. It names abstract
## joining-shape groups and does not prescribe a font glyph.
JoiningGroup :: [].{
    Value : InternalJoiningGroup.Value
    PropertyName : { short : Str, long : Str }
    Run : { range : TextRange, value : Value }
    AliasError : [UnknownAlias]

    property_name : PropertyName
    property_name = InternalJoiningGroup.property_name

    of_scalar : Scalar -> Value
    of_scalar = |scalar| InternalJoiningGroup.lookup(Scalar.to_u32(scalar))

    short : Value -> Str
    short = InternalJoiningGroup.short

    long : Value -> Str
    long = InternalJoiningGroup.long

    alias_count : Value -> U8
    alias_count = InternalJoiningGroup.alias_count

    alias_at : Value, U8 -> [Some(Str), None]
    alias_at = InternalJoiningGroup.alias_at

    parse : Str -> Try(Value, AliasError)
    parse = |name| {
        match InternalJoiningGroup.parse(name) {
            Some(value) => Ok(value)
            None => Err(UnknownAlias)
        }
    }

    fold_runs : Str, state, (state, Run -> state) -> state
    fold_runs = |source, initial, emit| InternalPropertyRuns.fold(source, initial, InternalJoiningGroup.lookup, |left, right| left == right, emit)

    iter_runs : Str -> Iter(Run)
    iter_runs = |source| InternalPropertyRuns.iter(source, InternalJoiningGroup.lookup, |left, right| left == right)
}
