app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import pf.Host
import unicode.ByteRange
import unicode.Grapheme

run! : Str => Str
run! = |source| {
    before = Host.alloc_count!({})
    iter_signature = signature_iter(Grapheme.iter_ranges(source))
    after = Host.alloc_count!({})
    list_signature = signature_list(Grapheme.ranges(source))

    "${iter_signature.count.to_str()}\t${list_signature.count.to_str()}\t${iter_signature.end_sum.to_str()}\t${list_signature.end_sum.to_str()}\t${iter_signature.indexed_end_sum.to_str()}\t${list_signature.indexed_end_sum.to_str()}\t${(after - before).to_str()}"
}

signature_iter = |initial| {
    var iterator = initial
    var count = 0.U64
    var end_sum = 0.U64
    var indexed_end_sum = 0.U64
    while Bool.True {
        match Iter.next(iterator) {
            Done => return { count, end_sum, indexed_end_sum }
            Skip({ rest }) => {
                iterator = rest
            }
            One({ item, rest }) => {
                end = ByteRange.end(item)
                count = count + 1
                end_sum = end_sum + end
                indexed_end_sum = indexed_end_sum + end * count
                iterator = rest
            }
        }
    }
    { count, end_sum, indexed_end_sum }
}

signature_list = |ranges| {
    ranges.fold({ count: 0.U64, end_sum: 0.U64, indexed_end_sum: 0.U64 }, |state, range| {
        count = state.count + 1
        end = ByteRange.end(range)
        {
            count,
            end_sum: state.end_sum + end,
            indexed_end_sum: state.indexed_end_sum + end * count,
        }
    })
}
