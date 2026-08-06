import InternalPropertyRuns
import InternalVerticalOrientation
import Scalar
import TextRange

## Unicode `Vertical_Orientation` (`vo`) from `VerticalOrientation.txt` and
## UAX #50. It provides fallback orientation before font-specific layout.
VerticalOrientation :: [].{
	Value : InternalVerticalOrientation.Value
	PropertyName : { short : Str, long : Str }
	Run : { range : TextRange, value : Value }
	AliasError : [UnknownAlias]

	property_name : PropertyName
	property_name = InternalVerticalOrientation.property_name

	of_scalar : Scalar -> Value
	of_scalar = |scalar| InternalVerticalOrientation.lookup(Scalar.to_u32(scalar))

	short : Value -> Str
	short = InternalVerticalOrientation.short

	long : Value -> Str
	long = InternalVerticalOrientation.long

	alias_count : Value -> U8
	alias_count = InternalVerticalOrientation.alias_count

	alias_at : Value, U8 -> [Some(Str), None]
	alias_at = InternalVerticalOrientation.alias_at

	parse : Str -> Try(Value, AliasError)
	parse = |name| {
		match InternalVerticalOrientation.parse(name) {
			Some(value) => Ok(value)
			None => Err(UnknownAlias)
		}
	}

	fold_runs : Str, state, (state, Run -> state) -> state
	fold_runs = |source, initial, emit| InternalPropertyRuns.fold(source, initial, InternalVerticalOrientation.lookup, |left, right| left == right, emit)

	iter_runs : Str -> Iter(Run)
	iter_runs = |source| InternalPropertyRuns.iter(source, InternalVerticalOrientation.lookup, |left, right| left == right)
}
