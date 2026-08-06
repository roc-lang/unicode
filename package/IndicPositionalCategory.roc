import InternalIndicPositionalCategory
import InternalPropertyRuns
import Scalar
import TextRange

## Unicode `Indic_Positional_Category` (`InPC`) from
## `IndicPositionalCategory.txt`. Its notional positions classify characters;
## they do not prescribe final glyph placement.
IndicPositionalCategory :: [].{
	Value : InternalIndicPositionalCategory.Value
	PropertyName : { short : Str, long : Str }
	Run : { range : TextRange, value : Value }
	AliasError : [UnknownAlias]

	property_name : PropertyName
	property_name = InternalIndicPositionalCategory.property_name

	of_scalar : Scalar -> Value
	of_scalar = |scalar| InternalIndicPositionalCategory.lookup(Scalar.to_u32(scalar))

	short : Value -> Str
	short = InternalIndicPositionalCategory.short

	long : Value -> Str
	long = InternalIndicPositionalCategory.long

	alias_count : Value -> U8
	alias_count = InternalIndicPositionalCategory.alias_count

	alias_at : Value, U8 -> [Some(Str), None]
	alias_at = InternalIndicPositionalCategory.alias_at

	parse : Str -> Try(Value, AliasError)
	parse = |name| {
		match InternalIndicPositionalCategory.parse(name) {
			Some(value) => Ok(value)
			None => Err(UnknownAlias)
		}
	}

	fold_runs : Str, state, (state, Run -> state) -> state
	fold_runs = |source, initial, emit| InternalPropertyRuns.fold(source, initial, InternalIndicPositionalCategory.lookup, |left, right| left == right, emit)

	iter_runs : Str -> Iter(Run)
	iter_runs = |source| InternalPropertyRuns.iter(source, InternalIndicPositionalCategory.lookup, |left, right| left == right)
}
