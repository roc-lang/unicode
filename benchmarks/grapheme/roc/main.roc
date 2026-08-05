app [run!] {
    pf: platform "../../../tests/platform/main.roc",
    unicode: "../../../package/main.roc",
}

import unicode.Grapheme

run! : Str => Str
run! = |input| {
    input_len = input.count_utf8_bytes()
    if input_len < 9 {
        return "ERROR: expected 8-digit repeat count, newline, and UTF-8 corpus"
    }

    repeats_str = input.drop_last_bytes(input_len - 8) ?? ...
    source = input.drop_first_bytes(9) ?? ...
    repeats = U64.from_str(repeats_str) ?? return "ERROR: invalid repeat count"
    var remaining = repeats
    var total = 0.U64

    while remaining > 0 {
        pushed = Grapheme.Cursor.push(
            Grapheme.Cursor.init({}),
            source,
            0.U64,
            |count, _range| count + 1,
        ) ?? ...
        finished = Grapheme.Cursor.finish(
            pushed.cursor,
            pushed.state,
            |count, _range| count + 1,
        ) ?? ...
        total = total + finished.state
        remaining = remaining - 1
    }

    total.to_str()
}
