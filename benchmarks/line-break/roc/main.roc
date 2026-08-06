app [run!] {
    pf: platform "../../../tests/platform/main.roc",
    unicode: "../../../package/main.roc",
}

import unicode.LineBreak
import unicode.TextPosition

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
        pushed = match LineBreak.Cursor.push(
            LineBreak.Cursor.init({}),
            source,
            { count: 0.U64, sum_offsets: 0.U64, weighted_offsets: 0.U64 },
            append_signature,
        ) {
            Failed(_) => return "ERROR: line-break push failed"
            Pushed(value) => value
        }
        finished = match LineBreak.Cursor.finish(
            pushed.cursor,
            pushed.state,
            append_signature,
        ) {
            Failed(_) => return "ERROR: line-break finish failed"
            End(value) => value
        }
        signature = finished.state
        return "${signature.count.to_str()} ${signature.sum_offsets.to_str()} ${signature.weighted_offsets.to_str()}"
    }

    var remaining = repeats
    var total = 0.U64
    while remaining > 0 {
        pushed = match LineBreak.Cursor.push(
            LineBreak.Cursor.init({}),
            source,
            0.U64,
            |count, _opportunity| count + 1,
        ) {
            Failed(_) => return "ERROR: line-break push failed"
            Pushed(value) => value
        }
        finished = match LineBreak.Cursor.finish(
            pushed.cursor,
            pushed.state,
            |count, _opportunity| count + 1,
        ) {
            Failed(_) => return "ERROR: line-break finish failed"
            End(value) => value
        }
        total = total + finished.state
        remaining = remaining - 1
    }

    total.to_str()
}

append_signature = |state, opportunity| {
    count = state.count + 1
    offset = TextPosition.byte_offset(opportunity.at)
    {
        count,
        sum_offsets: state.sum_offsets + offset,
        weighted_offsets: state.weighted_offsets + count * offset,
    }
}
