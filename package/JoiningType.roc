import InternalJoiningType
import InternalPropertyRuns
import Scalar
import TextRange

## Unicode `Joining_Type` (`jt`) from `DerivedJoiningType.txt`. It describes
## cursive joining capability; it does not select or position glyphs.
JoiningType :: [].{
	Value : InternalJoiningType.Value
	PropertyName : { short : Str, long : Str }
	Run : { range : TextRange, value : Value }
	AliasError : [UnknownAlias]

	property_name : PropertyName
	property_name = InternalJoiningType.property_name

	of_scalar : Scalar -> Value
	of_scalar = |scalar| InternalJoiningType.lookup(Scalar.to_u32(scalar))

	short : Value -> Str
	short = InternalJoiningType.short

	long : Value -> Str
	long = InternalJoiningType.long

	alias_count : Value -> U8
	alias_count = InternalJoiningType.alias_count

	alias_at : Value, U8 -> [Some(Str), None]
	alias_at = InternalJoiningType.alias_at

	parse : Str -> Try(Value, AliasError)
	parse = |name| {
		match InternalJoiningType.parse(name) {
			Some(value) => Ok(value)
			None => Err(UnknownAlias)
		}
	}

	fold_runs : Str, state, (state, Run -> state) -> state
	fold_runs = |source, initial, emit| InternalPropertyRuns.fold(source, initial, InternalJoiningType.lookup, |left, right| left == right, emit)

	iter_runs : Str -> Iter(Run)
	iter_runs = |source| InternalPropertyRuns.iter(source, InternalJoiningType.lookup, |left, right| left == right)
}
