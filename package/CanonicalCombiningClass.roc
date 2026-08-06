import InternalCanonicalCombiningClass
import InternalPropertyAliases
import InternalPropertyRuns
import Scalar
import TextRange

## Exact numeric Unicode `Canonical_Combining_Class` (`ccc`) values from
## `DerivedCombiningClass.txt`. The number is the canonical ordering weight;
## this is not a generic “is combining mark” flag and does not shape glyphs.
CanonicalCombiningClass :: { value : U8 }.{
	PropertyName : { short : Str, long : Str }
	Run : { range : TextRange, value : CanonicalCombiningClass }
	AliasError : [UnknownAlias]

	property_name : PropertyName
	property_name = InternalPropertyAliases.canonical_combining_class_property

	from_u8 : U8 -> CanonicalCombiningClass
	from_u8 = |value| { value: value }

	to_u8 : CanonicalCombiningClass -> U8
	to_u8 = |class| class.value

	of_scalar : Scalar -> CanonicalCombiningClass
	of_scalar = |scalar| { value: InternalCanonicalCombiningClass.lookup(Scalar.to_u32(scalar)) }

	short : CanonicalCombiningClass -> [Some(Str), None]
	short = |class| InternalPropertyAliases.canonical_combining_class_short(class.value)

	long : CanonicalCombiningClass -> [Some(Str), None]
	long = |class| InternalPropertyAliases.canonical_combining_class_long(class.value)

	alias_count : CanonicalCombiningClass -> U8
	alias_count = |class| InternalPropertyAliases.canonical_combining_class_alias_count(class.value)

	alias_at : CanonicalCombiningClass, U8 -> [Some(Str), None]
	alias_at = |class, index| InternalPropertyAliases.canonical_combining_class_alias_at(class.value, index)

	parse : Str -> Try(CanonicalCombiningClass, AliasError)
	parse = |name| {
		match InternalPropertyAliases.canonical_combining_class_parse(name) {
			Some(value) => Ok({ value: value })
			None => Err(UnknownAlias)
		}
	}

	is_eq : CanonicalCombiningClass, CanonicalCombiningClass -> Bool
	is_eq = |left, right| left.value == right.value

	fold_runs : Str, state, (state, Run -> state) -> state
	fold_runs = |source, initial, emit| {
		InternalPropertyRuns.fold(
			source,
			initial,
			InternalCanonicalCombiningClass.lookup,
			|left, right| left == right,
			|state, run| emit(state, { range: run.range, value: { value: run.value } }),
		)
	}

	iter_runs : Str -> Iter(Run)
	iter_runs = |source| {
		InternalPropertyRuns.iter(
			source,
			InternalCanonicalCombiningClass.lookup,
			|left, right| left == right,
		).map(|run| { range: run.range, value: { value: run.value } })
	}
}
