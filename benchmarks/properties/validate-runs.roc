app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.ByteRange
import unicode.CanonicalCombiningClass
import unicode.Emoji
import unicode.GeneralCategory
import unicode.Property
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange

run! : Str => Str
run! = |input| {
    if Bool.not(gc_runs(input)) { return "FAIL\tgeneral-category runs" }
    if Bool.not(ccc_runs(input)) { return "FAIL\tccc runs" }
    if Bool.not(emoji_runs(input)) { return "FAIL\temoji runs" }
    if Bool.not(property_iteration(input)) { return "FAIL\tproperty iteration" }
    "PASS\truns-and-iteration"
}

gc_runs = |input| {
    expected = [
        shape_values(0, 2, 0, 2, "Lu"), shape_values(2, 6, 2, 4, "Ll"),
        shape_values(6, 8, 4, 5, "Mn"), shape_values(8, 9, 5, 6, "Ps"),
    ]
    source = runtime_str("AAαά(", input)
    folded = GeneralCategory.fold_runs(source, [], |runs, run| runs.append(run_shape(run.range, GeneralCategory.short(run.value))))
    var iterated = []
    for run in GeneralCategory.iter_runs(source) {
        iterated = iterated.append(run_shape(run.range, GeneralCategory.short(run.value)))
    }
    empty = GeneralCategory.fold_runs("", 0.U64, |count, _| count + 1)
    folded == expected and iterated == expected and empty == 0
}

ccc_runs = |input| {
    expected = [shape_values(0, 4, 0, 2, "230"), shape_values(4, 5, 2, 3, "0")]
    folded = CanonicalCombiningClass.fold_runs(runtime_str("́̀A", input), [], |runs, run| runs.append(run_shape(run.range, CanonicalCombiningClass.to_u8(run.value).to_str())))
    folded == expected
}

emoji_runs = |input| {
    expected = [shape_values(0, 2, 0, 2, "0"), shape_values(2, 10, 2, 4, "35"), shape_values(10, 11, 4, 5, "0")]
    folded = Emoji.fold_runs(runtime_str("AA😀😀A", input), [], |runs, run| runs.append(run_shape(run.range, emoji_bits(run.value).to_str())))
    folded == expected
}

property_iteration = |input| {
    source = runtime_str("Aé𐀀", input)
    var entries = []
    for entry in Property.iter(source) {
        range = entry.located.byte_range
        entries = entries.append({ scalar: Scalar.to_u32(entry.located.scalar), byte_start: ByteRange.start(range), byte_end: ByteRange.end(range), scalar_index: entry.located.scalar_index })
    }
    expected = [
        { scalar: 0x41, byte_start: 0, byte_end: 1, scalar_index: 0 },
        { scalar: 0xE9, byte_start: 1, byte_end: 3, scalar_index: 1 },
        { scalar: 0x10000, byte_start: 3, byte_end: 7, scalar_index: 2 },
    ]
    folded = Property.fold(source, 0.U64, |count, _| count + 1)
    first = match Iter.next(GeneralCategory.iter_runs(runtime_str("Aααααααααααααααα", input))) {
        One({ item, .. }) => run_shape(item.range, GeneralCategory.short(item.value))
        _ => shape_values(99, 99, 99, 99, "bad")
    }
    entries == expected and folded == 3 and first == shape_values(0, 1, 0, 1, "Lu")
}

runtime_str = |value, input| value.concat(input).drop_last_bytes(input.count_utf8_bytes()) ?? value

run_shape = |range, value| {
    bytes = TextRange.byte_range(range)
    scalars = TextRange.scalar_range(range)
    shape_values(ByteRange.start(bytes), ByteRange.end(bytes), ScalarRange.start(scalars), ScalarRange.end(scalars), value)
}

shape_values = |byte_start, byte_end, scalar_start, scalar_end, value| { byte_start, byte_end, scalar_start, scalar_end, value }

emoji_bits = |value| {
    bool_u64(value.emoji) + bool_u64(value.emoji_presentation) * 2 + bool_u64(value.emoji_modifier) * 4 + bool_u64(value.emoji_modifier_base) * 8 + bool_u64(value.emoji_component) * 16 + bool_u64(value.extended_pictographic) * 32
}

bool_u64 = |value| if value 1.U64 else 0.U64
