import unicode.ByteRange
import unicode.Scalar

ScanProbe :: [].{
    Signature : {
        count : U64,
        scalar_sum : U64,
        indexed_scalar_sum : U64,
        byte_start_sum : U64,
        byte_end_sum : U64,
        scalar_index_sum : U64,
    }

    scan : Str, U64 -> Signature
    scan = |source, limit| {
        var iterator = Scalar.iter(source)
        var count = 0.U64
        var scalar_sum = 0.U64
        var indexed_scalar_sum = 0.U64
        var byte_start_sum = 0.U64
        var byte_end_sum = 0.U64
        var scalar_index_sum = 0.U64

        while count < limit {
            match Iter.next(iterator) {
                Done => break
                Skip({ rest }) => {
                    iterator = rest
                }
                One({ item, rest }) => {
                    scalar = Scalar.to_u32(item.scalar).to_u64()
                    start = ByteRange.start(item.byte_range)
                    end = ByteRange.end(item.byte_range)
                    scalar_sum = scalar_sum + scalar
                    indexed_scalar_sum = indexed_scalar_sum + scalar * (count + 1)
                    byte_start_sum = byte_start_sum + start
                    byte_end_sum = byte_end_sum + end
                    scalar_index_sum = scalar_index_sum + item.scalar_index
                    count = count + 1
                    iterator = rest
                }
            }
        }

        {
            count,
            scalar_sum,
            indexed_scalar_sum,
            byte_start_sum,
            byte_end_sum,
            scalar_index_sum,
        }
    }

    render : Signature, U64 -> Str
    render = |signature, allocations| {
        "${signature.count.to_str()}\t${signature.scalar_sum.to_str()}\t${signature.indexed_scalar_sum.to_str()}\t${signature.byte_start_sum.to_str()}\t${signature.byte_end_sum.to_str()}\t${signature.scalar_index_sum.to_str()}\t${allocations.to_str()}"
    }
}
