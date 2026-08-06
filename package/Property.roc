import BidiClass
import BidiProperties
import ByteRange
import CanonicalCombiningClass
import EastAsianWidth
import Emoji
import GeneralCategory
import IndicPositionalCategory
import IndicSyllabicCategory
import InternalCharacterFlags
import InternalCompositeProperties
import InternalLooseAlias
import InternalUtf8
import JoiningGroup
import JoiningType
import Scalar
import VerticalOrientation

## A bounded, fused view of Unicode scalar properties commonly needed before
## font-specific shaping. `Row` is sealed and allocation-free: construction
## performs one dense composite index lookup, while optional sparse mappings
## are resolved only if their typed accessors are called. These are immutable
## Unicode facts and do not select, substitute, or position glyphs.
Property :: [].{
	Row := { scalar : Scalar, row_id : InternalCompositeProperties.RowId }.{
		general_category : Row -> GeneralCategory.Value
		general_category = |row| InternalCompositeProperties.general_category(row.row_id)

		canonical_combining_class : Row -> CanonicalCombiningClass
		canonical_combining_class = |row| CanonicalCombiningClass.from_u8(InternalCompositeProperties.canonical_combining_class(row.row_id))

		east_asian_width : Row -> EastAsianWidth.Value
		east_asian_width = |row| {
			match InternalCompositeProperties.east_asian_width(row.row_id) {
				A => Ambiguous
				F => Fullwidth
				H => Halfwidth
				N => Neutral
				Na => Narrow
				W => Wide
			}
		}

		bidi_class : Row -> BidiClass.Value
		bidi_class = |row| InternalCompositeProperties.bidi_class(row.row_id)

		bidi_mirrored : Row -> Bool
		bidi_mirrored = |row| InternalCompositeProperties.bidi_mirrored(row.row_id)

		## Sparse and genuinely optional; evaluated only when requested.
		bidi_mirroring_glyph : Row -> [Some(Scalar), None]
		bidi_mirroring_glyph = |row| BidiProperties.mirroring_glyph(row.scalar)

		## Sparse and genuinely optional; evaluated only when requested.
		bidi_paired_bracket : Row -> [Some(BidiProperties.PairedBracket), None]
		bidi_paired_bracket = |row| BidiProperties.paired_bracket(row.scalar)

		joining_type : Row -> JoiningType.Value
		joining_type = |row| InternalCompositeProperties.joining_type(row.row_id)

		joining_group : Row -> JoiningGroup.Value
		joining_group = |row| InternalCompositeProperties.joining_group(row.row_id)

		indic_syllabic_category : Row -> IndicSyllabicCategory.Value
		indic_syllabic_category = |row| InternalCompositeProperties.indic_syllabic_category(row.row_id)

		indic_positional_category : Row -> IndicPositionalCategory.Value
		indic_positional_category = |row| InternalCompositeProperties.indic_positional_category(row.row_id)

		default_ignorable : Row -> Bool
		default_ignorable = |row| InternalCompositeProperties.default_ignorable(row.row_id)

		variation_selector : Row -> Bool
		variation_selector = |row| InternalCompositeProperties.variation_selector(row.row_id)

		vertical_orientation : Row -> VerticalOrientation.Value
		vertical_orientation = |row| InternalCompositeProperties.vertical_orientation(row.row_id)

		emoji : Row -> Emoji.Properties
		emoji = |row| InternalCompositeProperties.emoji(row.row_id)
	}

	Entry : { located : Scalar.LocatedScalar, row : Row }
	PropertyName : { short : Str, long : Str }
	AliasError : [UnknownAlias]

	default_ignorable_property : PropertyName
	default_ignorable_property = { short: "DI", long: "Default_Ignorable_Code_Point" }

	variation_selector_property : PropertyName
	variation_selector_property = { short: "VS", long: "Variation_Selector" }

	## Construct an opaque row with exactly one dense composite lookup.
	of_scalar : Scalar -> Row
	of_scalar = |scalar| {
		{ scalar, row_id: InternalCompositeProperties.lookup_id(Scalar.to_u32(scalar)) }
	}

	## Query `Default_Ignorable_Code_Point` from `DerivedCoreProperties.txt`
	## through its independent narrow binary view.
	is_default_ignorable : Scalar -> Bool
	is_default_ignorable = |scalar| InternalCharacterFlags.lookup(Scalar.to_u32(scalar)).default_ignorable

	## Query `Variation_Selector` from `PropList.txt` through its independent
	## narrow binary view.
	is_variation_selector : Scalar -> Bool
	is_variation_selector = |scalar| InternalCharacterFlags.lookup(Scalar.to_u32(scalar)).variation_selector

	binary_short : Bool -> Str
	binary_short = |value| if value "Y" else "N"

	binary_long : Bool -> Str
	binary_long = |value| if value "Yes" else "No"

	binary_alias_count : Bool -> U8
	binary_alias_count = |_| 4

	binary_alias_at : Bool, U8 -> [Some(Str), None]
	binary_alias_at = |value, index| {
		if value {
			match index {
				0 => Some("Y")
				1 => Some("Yes")
				2 => Some("T")
				3 => Some("True")
				_ => None
			}
		} else {
			match index {
				0 => Some("N")
				1 => Some("No")
				2 => Some("F")
				3 => Some("False")
				_ => None
			}
		}
	}

	parse_binary : Str -> Try(Bool, AliasError)
	parse_binary = |name| {
		if InternalLooseAlias.matches(name, "Y") or InternalLooseAlias.matches(name, "Yes") or InternalLooseAlias.matches(name, "T") or InternalLooseAlias.matches(name, "True") {
			Ok(Bool.True)
		}
			else if InternalLooseAlias.matches(name, "N") or InternalLooseAlias.matches(name, "No") or InternalLooseAlias.matches(name, "F") or InternalLooseAlias.matches(name, "False") {
				Ok(Bool.False)
			}
				else {
					Err(UnknownAlias)
				}
	}

	## Decode a complete valid string once and fold scalar entries in source
	## order. No per-scalar string, list, or retained analysis is created.
	fold : Str, state, (state, Entry -> state) -> state
	fold = |source, initial, emit| {
		var state = initial
		for located in Scalar.iter(source) {
			state = emit(state, { located, row: Property.of_scalar(located.scalar) })
		}
		state
	}

	## Lazily scan a complete `Str`. The iterator retains the source and may
	## stop early without decoding its suffix.
	iter : Str -> Iter(Entry)
	iter = |source| {
		next_entry = |cursor| {
			match InternalUtf8.next(cursor) {
				Done => Err(NoMore)
				One({ item, rest }) => {
					match Scalar.from_u32(item.scalar) {
						Err(_) => Err(NoMore)
						Ok(scalar) => {
							match ByteRange.from_bounds(item.byte_start, item.byte_end) {
								Err(_) => Err(NoMore)
								Ok(byte_range) => Ok((
									{
										located: {
											scalar,
											byte_range,
											scalar_index: item.scalar_index,
										},
										row: Property.of_scalar(scalar),
									},
									rest,
								))
							}
						}
					}
				}
			}
		}

		Iter.custom(InternalUtf8.init(source), Unknown, next_entry)
	}
}
