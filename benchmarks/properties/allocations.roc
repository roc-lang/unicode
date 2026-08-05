app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import pf.Host
import unicode.GeneralCategory
import unicode.Property
import unicode.Scalar
import unicode.TextRange
import unicode.ByteRange

## Report allocations for complete lazy scans, run scans, one-run early stop,
## and loose matching. Inputs are constructed by the runner before counters.
run! : Str => Str
run! = |source| {
    before_property = Host.alloc_count!({})
    var property_checksum = 0.U64
    for entry in Property.iter(source) {
        property_checksum = property_checksum + Scalar.to_u32(entry.located.scalar).to_u64() + 1
    }
    after_property = Host.alloc_count!({})

    before_runs = Host.alloc_count!({})
    var run_checksum = 0.U64
    for run in GeneralCategory.iter_runs(source) {
        range = TextRange.byte_range(run.range)
        run_checksum = run_checksum + ByteRange.start(range) + ByteRange.end(range) + 1
    }
    after_runs = Host.alloc_count!({})

    before_early = Host.alloc_count!({})
    early_checksum = match Iter.next(GeneralCategory.iter_runs(source)) {
        One({ item, .. }) => ByteRange.end(TextRange.byte_range(item.range)) + 1
        _ => 1.U64
    }
    after_early = Host.alloc_count!({})

    alias_source = source
    before_alias = Host.alloc_count!({})
    alias_checksum = match GeneralCategory.parse(alias_source) {
        Ok(value) => GeneralCategory.alias_count(value).to_u64()
        Err(_) => 0.U64
    }
    after_alias = Host.alloc_count!({})

    checksum = property_checksum + run_checksum + early_checksum + alias_checksum
    "${(after_property - before_property).to_str()}\t${(after_runs - before_runs).to_str()}\t${(after_early - before_early).to_str()}\t${(after_alias - before_alias).to_str()}\t${alias_checksum.to_str()}\t${checksum.to_str()}"
}
