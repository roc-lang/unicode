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
run! = |source| {
    complete = ScriptItemization.fold_runs(
        source,
        ScriptItemization.default,
        signature_init,
        append_signature,
    )

    pushed = ScriptItemization.cursor_push(
        ScriptItemization.cursor_init(
            ScriptItemization.default,
            source.count_utf8_bytes() + 1,
        ),
        source,
        signature_init,
        append_signature,
    )
    streamed = match pushed {
        Failed(_) => return "ERROR: cursor_push failed"
        Pushed(next) => match ScriptItemization.cursor_finish(
            next.cursor,
            next.state,
            append_signature,
        ) {
            Failed(_) => return "ERROR: cursor_finish failed"
            FinishedCursor(done) => done.state
        }
    }

    complete_str = signature_str(complete)
    streamed_str = signature_str(streamed)
    "complete=${complete_str}\ncursor=${streamed_str}\nparity=${if complete_str == streamed_str "true" else "false"}"
}

signature_init = {
    count: 0.U64,
    byte_sum: 0.U64,
    scalar_sum: 0.U64,
    weighted_byte_ends: 0.U64,
    weighted_scalar_ends: 0.U64,
    checksum: fnv_offset,
}

append_signature = |state, run| {
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
}

signature_str = |signature| {
    "${signature.count.to_str()}\t${signature.byte_sum.to_str()}\t${signature.scalar_sum.to_str()}\t${signature.weighted_byte_ends.to_str()}\t${signature.weighted_scalar_ends.to_str()}\t${signature.checksum.to_str()}"
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
