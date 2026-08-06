import InternalGeneralCategory
import InternalPropertyAliases
import InternalPropertyRuns
import Scalar
import TextRange

## The Unicode `General_Category` (`gc`) property from
## `DerivedGeneralCategory.txt`. It classifies scalar values as letters,
## marks, numbers, punctuation, symbols, separators, controls, private use,
## surrogates, or unassigned code points; it does not choose glyphs.
GeneralCategory :: [].{
	Value : InternalGeneralCategory.GeneralCategory
	PropertyName : { short : Str, long : Str }
	Run : { range : TextRange, value : Value }
	AliasError : [UnknownAlias]

	property_name : PropertyName
	property_name = InternalPropertyAliases.general_category_property

	of_scalar : Scalar -> Value
	of_scalar = |scalar| InternalGeneralCategory.lookup(Scalar.to_u32(scalar))

	short : Value -> Str
	short = |value| InternalPropertyAliases.general_category_short(value)

	long : Value -> Str
	long = |value| InternalPropertyAliases.general_category_long(value)

	alias_count : Value -> U8
	alias_count = |value| InternalPropertyAliases.general_category_alias_count(value)

	alias_at : Value, U8 -> [Some(Str), None]
	alias_at = |value, index| InternalPropertyAliases.general_category_alias_at(value, index)

	parse : Str -> Try(Value, AliasError)
	parse = |name| {
		match InternalPropertyAliases.general_category_parse(name) {
			Some(value) => Ok(value)
			None => Err(UnknownAlias)
		}
	}

	## Fold maximal adjacent runs in one forward decode of a complete `Str`.
	fold_runs : Str, state, (state, Run -> state) -> state
	fold_runs = |source, initial, emit| {
		InternalPropertyRuns.fold(
			source,
			initial,
			InternalGeneralCategory.lookup,
			|left, right| left == right,
			emit,
		)
	}

	## Lazily visit maximal adjacent runs. The iterator retains `source`.
	iter_runs : Str -> Iter(Run)
	iter_runs = |source| {
		InternalPropertyRuns.iter(
			source,
			InternalGeneralCategory.lookup,
			|left, right| left == right,
		)
	}
}
