app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import unicode.ByteRange
import unicode.ScalarRange
import unicode.Script
import unicode.ScriptItemization
import unicode.TextRange

run! : Str => Str
run! = |input| {
    aliases_ok = ["Greek", "isGreek", "Is_Greek", "Gr\teek"].fold(Bool.True, |ok, alias| {
        ok and match Script.from_alias(alias) {
            Ok(script) => script == Grek
            Err(_) => Bool.False
        }
    })
    recursive_prefix_rejected = match Script.from_alias("isis${input}") {
        Ok(_) => Bool.False
        Err(_) => Bool.True
    }

    right = signature(ScriptItemization.runs("a ", ScriptItemization.default))
    right_streamed = stream_signature(["a", " ", ""])
    left = signature(ScriptItemization.runs(" a", ScriptItemization.default))
    left_streamed = stream_signature(["", " ", "a"])

    "aliases=${bool_str(aliases_ok)};recursive_is_rejected=${bool_str(recursive_prefix_rejected)}\nright=${right}\nright_cursor=${right_streamed}\nleft=${left}\nleft_cursor=${left_streamed}\nparity=${bool_str(right == right_streamed and left == left_streamed)}"
}

stream_signature = |chunks| {
    pushed = chunks.fold(
        Active({
            cursor: ScriptItemization.cursor_init(ScriptItemization.default, 16),
            runs: [],
        }),
        |outcome, chunk| {
            match outcome {
                StreamFailed => StreamFailed
                Active({ cursor, runs }) => match ScriptItemization.cursor_push(
                    cursor,
                    chunk,
                    runs,
                    |state, run| state.append(run),
                ) {
                    Failed(_) => StreamFailed
                    Pushed(next) => Active({ cursor: next.cursor, runs: next.state })
                }
            }
        },
    )
    match pushed {
        StreamFailed => "failed"
        Active({ cursor, runs }) => match ScriptItemization.cursor_finish(
            cursor,
            runs,
            |state, run| state.append(run),
        ) {
            Failed(_) => "failed"
            FinishedCursor(done) => signature(done.state)
        }
    }
}

signature = |runs| {
    Str.join_with(
        runs.map(|run| {
            bytes = TextRange.byte_range(run.range)
            scalars = TextRange.scalar_range(run.range)
            "${ByteRange.start(bytes).to_str()}-${ByteRange.end(bytes).to_str()}:${ScalarRange.start(scalars).to_str()}-${ScalarRange.end(scalars).to_str()}:${Script.short_alias(run.script)}"
        }),
        ",",
    )
}

bool_str = |value| if value "true" else "false"
