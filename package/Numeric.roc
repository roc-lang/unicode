import InternalNumericData
import Scalar

## Unicode Numeric_Type and exact Numeric_Value from the extracted UCD,
## including values derived from Unihan. All queries are allocation-free
## scalar lookups.
Numeric :: [].{
	Type : InternalNumericData.Value
	PropertyName : { short : Str, long : Str }
	AliasError : [UnknownAlias]

	## Exact signed fraction in reduced form with a positive denominator.
	## Integers have denominator one.
	Rational : { numerator : I64, denominator : U64 }

	type_property : PropertyName
	type_property = InternalNumericData.type_property

	value_property : PropertyName
	value_property = InternalNumericData.value_property

	## Decimal is the positional base-ten class. Digit includes compatibility
	## digits, and Numeric includes fractions and other numeric characters.
	type_of : Scalar -> Type
	type_of = |scalar| InternalNumericData.type_of(Scalar.to_u32(scalar))

	is_decimal_digit : Scalar -> Bool
	is_decimal_digit = |scalar| Numeric.type_of(scalar) == Decimal

	## None is the UCD default Numeric_Value=NaN. A numeric zero is Some.
	value_of : Scalar -> [None, Some(Rational)]
	value_of = |scalar| InternalNumericData.value_of(Scalar.to_u32(scalar))

	short : Type -> Str
	short = InternalNumericData.short

	long : Type -> Str
	long = InternalNumericData.long

	alias_count : Type -> U8
	alias_count = InternalNumericData.alias_count

	alias_at : Type, U8 -> [Some(Str), None]
	alias_at = InternalNumericData.alias_at

	## Parse an official Numeric_Type alias with UAX #44 loose matching.
	parse : Str -> Try(Type, AliasError)
	parse = |name| match InternalNumericData.parse(name) {
		Some(value) => Ok(value)
		None => Err(UnknownAlias)
	}
}
