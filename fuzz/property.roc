app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.2.1/9Qpttb6LTgcMaVsSBLsnaiS2mDUrf6Bxa6dX9Rqwviz4.tar.zst",
	unicode: "../package/main.roc",
}

import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.CanonicalCombiningClass
import unicode.GeneralCategory
import unicode.Property
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange

RunShape : { byte_start : U64, byte_end : U64, scalar_start : U64, scalar_end : U64, value : Str }

test : List(U32) -> Fuzz.Outcome
test = |code_points| {
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")

	entries = Property.fold(source, [], |items, entry| items.append(entry))
	iterated_scalars = Property.iter(source).fold([], |items, entry| items.append(Scalar.to_u32(entry.located.scalar)))
	folded_scalars = entries.map(|entry| Scalar.to_u32(entry.located.scalar))
	if iterated_scalars != folded_scalars {
		crash "Property.iter disagreed with Property.fold on the decoded scalar sequence"
	}

	var $gc_values = []
	var $ccc_values = []
	for entry in entries {
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
	validate_runs(source, gc_runs, $gc_values)

	ccc_runs = CanonicalCombiningClass.fold_runs(source, [], |runs, run| runs.append(ccc_run_shape(run)))
	if ccc_runs != CanonicalCombiningClass.iter_runs(source).fold([], |runs, run| runs.append(ccc_run_shape(run))) {
		crash "CanonicalCombiningClass.fold_runs disagreed with CanonicalCombiningClass.iter_runs"
	}
	validate_runs(source, ccc_runs, $ccc_values)

	Fuzz.keep
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
validate_runs : Str, List(RunShape), List(Str) -> {}
validate_runs = |source, runs, entry_values| {
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

target = Fuzz.target_with({
	name: "unicode-property",
	generator: FuzzSupport.scalar_sequence,
	test,
	show: FuzzSupport.show_scalars,
})
