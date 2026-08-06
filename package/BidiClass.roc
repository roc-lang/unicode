import InternalBidiProperties
import InternalPropertyRuns
import Scalar
import TextRange

## The Unicode `Bidi_Class` (`bc`) directional class from
## `DerivedBidiClass.txt`. This is input to UAX #9; this module does not run
## the bidirectional algorithm or reorder text.
BidiClass :: [].{
	Value : InternalBidiProperties.Value
	PropertyName : { short : Str, long : Str }
	Run : { range : TextRange, value : Value }
	AliasError : [UnknownAlias]

	property_name : PropertyName
	property_name = InternalBidiProperties.property_name

	of_scalar : Scalar -> Value
	of_scalar = |scalar| InternalBidiProperties.lookup(Scalar.to_u32(scalar))

	short : Value -> Str
	short = InternalBidiProperties.short

	long : Value -> Str
	long = InternalBidiProperties.long

	alias_count : Value -> U8
	alias_count = InternalBidiProperties.alias_count

	alias_at : Value, U8 -> [Some(Str), None]
	alias_at = InternalBidiProperties.alias_at

	parse : Str -> Try(Value, AliasError)
	parse = |name| {
		match InternalBidiProperties.parse(name) {
			Some(value) => Ok(value)
			None => Err(UnknownAlias)
		}
	}

	fold_runs : Str, state, (state, Run -> state) -> state
	fold_runs = |source, initial, emit| {
		InternalPropertyRuns.fold(source, initial, InternalBidiProperties.lookup, |left, right| left == right, emit)
	}

	iter_runs : Str -> Iter(Run)
	iter_runs = |source| {
		InternalPropertyRuns.iter(source, InternalBidiProperties.lookup, |left, right| left == right)
	}
}
