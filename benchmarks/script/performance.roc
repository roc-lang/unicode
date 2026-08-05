app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.ByteRange
import unicode.ScalarRange
import unicode.Script
import unicode.ScriptItemization
import unicode.TextRange

fnv_offset : U64
fnv_offset = 14695981039346656037

fnv_prime : U64
fnv_prime = 1099511628211

run! : Str => Str
run! = |input| {
    input_len = input.count_utf8_bytes()
    if input_len < 9 {
        return "ERROR: expected 8-digit repeat count, newline, and UTF-8 corpus"
    }

    repeats_str = input.drop_last_bytes(input_len - 8) ?? ...
    source = input.drop_first_bytes(9) ?? ...
    repeats = U64.from_str(repeats_str) ?? return "ERROR: invalid repeat count"

    if repeats == 0 {
        signature = ScriptItemization.fold_runs(
            source,
            ScriptItemization.default,
            {
                count: 0.U64,
                byte_sum: 0.U64,
                scalar_sum: 0.U64,
                weighted_byte_ends: 0.U64,
                weighted_scalar_ends: 0.U64,
                checksum: fnv_offset,
            },
            |state, run| {
                count = state.count + 1
                bytes = TextRange.byte_range(run.range)
                scalars = TextRange.scalar_range(run.range)
                byte_start = ByteRange.start(bytes)
                byte_end = ByteRange.end(bytes)
                scalar_start = ScalarRange.start(scalars)
                scalar_end = ScalarRange.end(scalars)
                {
                    count,
                    byte_sum: state.byte_sum + byte_start + byte_end,
                    scalar_sum: state.scalar_sum + scalar_start + scalar_end,
                    weighted_byte_ends: state.weighted_byte_ends + count * byte_end,
                    weighted_scalar_ends: state.weighted_scalar_ends + count * scalar_end,
                    checksum: hash_run(state.checksum, run),
                }
            },
        )
        return "${signature.count.to_str()}\t${signature.byte_sum.to_str()}\t${signature.scalar_sum.to_str()}\t${signature.weighted_byte_ends.to_str()}\t${signature.weighted_scalar_ends.to_str()}\t${signature.checksum.to_str()}"
    }

    var remaining = repeats
    var total = 0.U64
    while remaining > 0 {
        checksum = ScriptItemization.fold_runs(
            source,
            ScriptItemization.default,
            fnv_offset,
            hash_run,
        )
        total = total.plus_wrap(checksum)
        remaining = remaining - 1
    }
    total.to_str()
}

hash_run = |initial, run| {
    bytes = TextRange.byte_range(run.range)
    scalars = TextRange.scalar_range(run.range)
    var hash = hash_u64(initial, ByteRange.start(bytes))
    hash = hash_u64(hash, ByteRange.end(bytes))
    hash = hash_u64(hash, ScalarRange.start(scalars))
    hash = hash_u64(hash, ScalarRange.end(scalars))
    for byte in Script.short_alias(run.script).iter_utf8() {
        hash = hash.bitwise_xor(byte.to_u64()).times_wrap(fnv_prime)
    }
    hash
}

hash_u64 = |initial, value| {
    var hash = initial
    var shift = 0.U8
    while shift < 64 {
        byte = value.shr_wrap(shift).bitwise_and(0xFF)
        hash = hash.bitwise_xor(byte).times_wrap(fnv_prime)
        shift = shift + 8
    }
    hash
}
