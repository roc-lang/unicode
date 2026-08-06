import InternalIndicSyllabicCategory
import InternalPropertyRuns
import Scalar
import TextRange

## Unicode `Indic_Syllabic_Category` (`InSC`) from
## `IndicSyllabicCategory.txt`. It describes structural character roles used
## by Indic/USE processing; it is not a shaping state machine.
IndicSyllabicCategory :: [].{
    Value : InternalIndicSyllabicCategory.Value
    PropertyName : { short : Str, long : Str }
    Run : { range : TextRange, value : Value }
    AliasError : [UnknownAlias]

    property_name : PropertyName
    property_name = InternalIndicSyllabicCategory.property_name

    of_scalar : Scalar -> Value
    of_scalar = |scalar| InternalIndicSyllabicCategory.lookup(Scalar.to_u32(scalar))

    short : Value -> Str
    short = InternalIndicSyllabicCategory.short

    long : Value -> Str
    long = InternalIndicSyllabicCategory.long

    alias_count : Value -> U8
    alias_count = InternalIndicSyllabicCategory.alias_count

    alias_at : Value, U8 -> [Some(Str), None]
    alias_at = InternalIndicSyllabicCategory.alias_at

    parse : Str -> Try(Value, AliasError)
    parse = |name| {
        match InternalIndicSyllabicCategory.parse(name) {
            Some(value) => Ok(value)
            None => Err(UnknownAlias)
        }
    }

    fold_runs : Str, state, (state, Run -> state) -> state
    fold_runs = |source, initial, emit| InternalPropertyRuns.fold(source, initial, InternalIndicSyllabicCategory.lookup, |left, right| left == right, emit)

    iter_runs : Str -> Iter(Run)
    iter_runs = |source| InternalPropertyRuns.iter(source, InternalIndicSyllabicCategory.lookup, |left, right| left == right)
}
