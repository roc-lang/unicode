import FuzzSupport
import fuzz.Fuzz
import unicode.BidiClass
import unicode.BidiProperties
import unicode.ByteRange
import unicode.EastAsianWidth
import unicode.Emoji
import unicode.IndicPositionalCategory
import unicode.IndicSyllabicCategory
import unicode.JoiningGroup
import unicode.JoiningType
import unicode.CanonicalCombiningClass
import unicode.GeneralCategory
import unicode.Property
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange
import unicode.VerticalOrientation

RunShape : { byte_start : U64, byte_end : U64, scalar_start : U64, scalar_end : U64, value : Str }

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	code_points = input.code_points
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")

	entries = Property.fold(source, [], |items, entry| items.append(entry))
	iterated_scalars = Property.iter(source).fold([], |items, entry| items.append(Scalar.to_u32(entry.located.scalar)))
	folded_scalars = entries.map(|entry| Scalar.to_u32(entry.located.scalar))
	if iterated_scalars != folded_scalars or folded_scalars != code_points {
		crash "Property.iter disagreed with Property.fold on the decoded scalar sequence"
	}

	var $offsets = [0.U64]
	var $byte = 0
	var $index = 0
	var $gc_values = []
	var $ccc_values = []
	for entry in entries {
		part = parts.get($index) ?? crash "property scanning produced an extra scalar"
		end = $byte + part.count_utf8_bytes()
		if entry.located.scalar_index != $index or ByteRange.start(entry.located.byte_range) != $byte or ByteRange.end(entry.located.byte_range) != end {
			crash "property scanning did not preserve exact byte and scalar coordinates"
		}
		$offsets = $offsets.append(end)
		$byte = end
		$index = $index + 1
		validate_row(entry.located.scalar, entry.row)
		gc_direct = GeneralCategory.of_scalar(entry.located.scalar)
		gc_composite = Property.Row.general_category(entry.row)
		if GeneralCategory.short(gc_direct) != GeneralCategory.short(gc_composite) {
			crash "GeneralCategory direct query disagreed with the composite Property.Row view"
		}

		ccc_direct = CanonicalCombiningClass.of_scalar(entry.located.scalar)
		ccc_composite = Property.Row.canonical_combining_class(entry.row)
		if CanonicalCombiningClass.to_u8(ccc_direct) != CanonicalCombiningClass.to_u8(ccc_composite) {
			crash "CanonicalCombiningClass direct query disagreed with the composite Property.Row view"
		}

		validate_general_category_aliases(gc_direct)
		validate_canonical_combining_class_aliases(ccc_direct)

		$gc_values = $gc_values.append(GeneralCategory.short(gc_direct))
		$ccc_values = $ccc_values.append(CanonicalCombiningClass.to_u8(ccc_direct).to_str())
	}

	gc_runs = GeneralCategory.fold_runs(source, [], |runs, run| runs.append(gc_run_shape(run)))
	if gc_runs != GeneralCategory.iter_runs(source).fold([], |runs, run| runs.append(gc_run_shape(run))) {
		crash "GeneralCategory.fold_runs disagreed with GeneralCategory.iter_runs"
	}
	validate_runs(source, gc_runs, $gc_values, $offsets)

	ccc_runs = CanonicalCombiningClass.fold_runs(source, [], |runs, run| runs.append(ccc_run_shape(run)))
	if ccc_runs != CanonicalCombiningClass.iter_runs(source).fold([], |runs, run| runs.append(ccc_run_shape(run))) {
		crash "CanonicalCombiningClass.fold_runs disagreed with CanonicalCombiningClass.iter_runs"
	}
	validate_runs(source, ccc_runs, $ccc_values, $offsets)

	Fuzz.keep
}

validate_row : Scalar, Property.Row -> {}
validate_row = |scalar, row| {
	if BidiClass.of_scalar(scalar) != Property.Row.bidi_class(row)
		or BidiProperties.is_mirrored(scalar) != Property.Row.bidi_mirrored(row)
		or BidiProperties.mirroring_glyph(scalar) != Property.Row.bidi_mirroring_glyph(row)
		or BidiProperties.paired_bracket(scalar) != Property.Row.bidi_paired_bracket(row)
		or EastAsianWidth.of_scalar(scalar) != Property.Row.east_asian_width(row)
		or JoiningGroup.of_scalar(scalar) != Property.Row.joining_group(row)
		or JoiningType.of_scalar(scalar) != Property.Row.joining_type(row)
		or IndicPositionalCategory.of_scalar(scalar) != Property.Row.indic_positional_category(row)
		or IndicSyllabicCategory.of_scalar(scalar) != Property.Row.indic_syllabic_category(row)
		or VerticalOrientation.of_scalar(scalar) != Property.Row.vertical_orientation(row)
		or Property.is_default_ignorable(scalar) != Property.Row.default_ignorable(row)
		or Property.is_variation_selector(scalar) != Property.Row.variation_selector(row)
		or Emoji.of_scalar(scalar) != Property.Row.emoji(row) {
		crash "a direct scalar property disagreed with its composite accessor"
	}
	{}
}

gc_run_shape : GeneralCategory.Run -> RunShape
gc_run_shape = |run| shape_from_range(run.range, GeneralCategory.short(run.value))

ccc_run_shape : CanonicalCombiningClass.Run -> RunShape
ccc_run_shape = |run| shape_from_range(run.range, CanonicalCombiningClass.to_u8(run.value).to_str())

shape_from_range : TextRange, Str -> RunShape
shape_from_range = |range, value| {
	bytes = TextRange.byte_range(range)
	scalars = TextRange.scalar_range(range)
	{
		byte_start: ByteRange.start(bytes),
		byte_end: ByteRange.end(bytes),
		scalar_start: ScalarRange.start(scalars),
		scalar_end: ScalarRange.end(scalars),
		value,
	}
}

## Runs must be a lossless, scalar-aligned, contiguous partition of the
## source; adjacent runs must never share a value (maximality); and every
## scalar inside a run must carry that run's directly-queried value.
validate_runs : Str, List(RunShape), List(Str), List(U64) -> {}
validate_runs = |source, runs, entry_values, offsets| {
	if source == "" and !runs.is_empty() {
		crash "empty source produced a property run"
	}
	if source != "" and runs.is_empty() {
		crash "nonempty source produced no property runs"
	}

	var $next_byte = 0
	var $next_scalar = 0
	var $entry_index = 0
	var $previous_value = None
	for run in runs {
		if run.byte_start != $next_byte or run.byte_end <= run.byte_start or run.scalar_start != $next_scalar or run.scalar_end <= run.scalar_start {
			crash "property runs were empty, overlapping, or discontinuous"
		}
		if offsets.get(run.scalar_start) != Ok(run.byte_start) or offsets.get(run.scalar_end) != Ok(run.byte_end) {
			crash "a property run's byte and scalar axes did not select the same input"
		}
		match $previous_value {
			None => {}
			Some(previous) => if previous == run.value {
				crash "adjacent property runs shared a value and were not maximal"
			}
		}
		while $entry_index < run.scalar_end {
			entry_value = entry_values.get($entry_index) ?? crash "a property run referenced a scalar beyond the decoded entries"
			if entry_value != run.value {
				crash "a scalar inside a property run did not match the run's directly-queried value"
			}
			$entry_index = $entry_index + 1
		}
		$next_byte = run.byte_end
		$next_scalar = run.scalar_end
		$previous_value = Some(run.value)
	}
	if $next_byte != source.count_utf8_bytes() or $next_scalar != entry_values.len() {
		crash "property runs did not cover the complete source"
	}
	{}
}

validate_general_category_aliases : GeneralCategory.Value -> {}
validate_general_category_aliases = |value| {
	short = GeneralCategory.short(value)
	long = GeneralCategory.long(value)
	if GeneralCategory.short(GeneralCategory.parse(short) ?? crash "GeneralCategory.parse rejected its own short alias") != short {
		crash "GeneralCategory short alias did not round-trip"
	}
	if GeneralCategory.short(GeneralCategory.parse(long) ?? crash "GeneralCategory.parse rejected its own long alias") != short {
		crash "GeneralCategory long alias did not round-trip"
	}
	count = GeneralCategory.alias_count(value)
	var $index = 0.U8
	while $index < count {
		alias = match GeneralCategory.alias_at(value, $index) {
			Some(alias_text) => alias_text
			None => crash "GeneralCategory.alias_at returned None below its own alias_count"
		}
		if GeneralCategory.short(GeneralCategory.parse(alias) ?? crash "GeneralCategory.parse rejected its own generated alias") != short {
			crash "a GeneralCategory alias did not round-trip"
		}
		$index = $index + 1
	}
	{}
}

validate_canonical_combining_class_aliases : CanonicalCombiningClass -> {}
validate_canonical_combining_class_aliases = |value| {
	numeric = CanonicalCombiningClass.to_u8(value)
	match CanonicalCombiningClass.short(value) {
		None => {}
		Some(short) => {
			parsed = CanonicalCombiningClass.parse(short) ?? crash "CanonicalCombiningClass.parse rejected its own short alias"
			if CanonicalCombiningClass.to_u8(parsed) != numeric {
				crash "CanonicalCombiningClass short alias did not round-trip"
			}
		}
	}
	match CanonicalCombiningClass.long(value) {
		None => {}
		Some(long) => {
			parsed = CanonicalCombiningClass.parse(long) ?? crash "CanonicalCombiningClass.parse rejected its own long alias"
			if CanonicalCombiningClass.to_u8(parsed) != numeric {
				crash "CanonicalCombiningClass long alias did not round-trip"
			}
		}
	}
	count = CanonicalCombiningClass.alias_count(value)
	var $index = 0.U8
	while $index < count {
		alias = match CanonicalCombiningClass.alias_at(value, $index) {
			Some(alias_text) => alias_text
			None => crash "CanonicalCombiningClass.alias_at returned None below its own alias_count"
		}
		parsed = CanonicalCombiningClass.parse(alias) ?? crash "CanonicalCombiningClass.parse rejected its own generated alias"
		if CanonicalCombiningClass.to_u8(parsed) != numeric {
			crash "a CanonicalCombiningClass alias did not round-trip"
		}
		$index = $index + 1
	}
	{}
}

PropertyTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-property",
		examples: [Fuzz.saved({ name: "sparse-emoji-and-unassigned", revision: "1", bytes: [0x00, 0x28, 0x00, 0x00, 0xDB, 0xCF, 0x21, 0x29, 0x00, 0x40, 0x78, 0x03, 0x60, 0x0F, 0xF6, 0x80, 0x69, 0xEC, 0xA1] })],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
