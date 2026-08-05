app [run!] {
    pf: platform "../../platform/main.roc",
    unicode: "../../../package/main.roc",
}

import pf.Host
import unicode.Grapheme

run! : Str => Str
run! = |input| {
    lines = input.split_on("\n").drop_if(|line| line == "")
    match lines {
        [] => fail("header", "empty input")
        [header, .. as cases] => {
            match header.split_on("\t") {
                ["ROC_UNICODE_TEST_V1", suite, count_str] => {
                    expected_count = U64.from_str(count_str) ?? 0
                    if expected_count != cases.len() {
                        fail("header", "header count mismatch")
                    } else {
                        match run_cases!(suite, cases, 0) {
                            Ok(count) => "PASS\t${suite}\t${count.to_str()}"
                            Err({ case_id, message }) => fail(case_id, message)
                        }
                    }
                }
                _ => fail("header", "malformed protocol header")
            }
        }
    }
}

run_cases! : Str, List(Str), U64 => Try(U64, { case_id : Str, message : Str })
run_cases! = |suite, remaining, count| {
    match remaining {
        [] => Ok(count)
        [line, .. as rest] => match run_case!(suite, line) {
            Ok({}) => run_cases!(suite, rest, count + 1)
            Err(error) => Err(error)
        }
    }
}

run_case! : Str, Str => Try({}, { case_id : Str, message : Str })
run_case! = |suite, line| {
    match line.split_on("\t") {
        [case_id, input_hex, expectation] => {
            match decode_hex(input_hex) {
                Err(_) => Err({ case_id, message: "malformed UTF-8 hex input" })
                Ok(str) => {
                    before = Host.alloc_count!({})
                    result = if suite == "allocation-calibration" and expectation == "zero" {
                        []
                    } else {
                        Grapheme.ranges(str)
                    }
                    after = Host.alloc_count!({})
                    allocations = after - before
                    _ = result.len()

                    if suite == "allocation-calibration" {
                        if (expectation == "zero" and allocations == 0) or (expectation == "positive" and allocations > 0) {
                            Ok({})
                        } else {
                            Err({ case_id, message: "expected ${expectation}, got ${allocations.to_str()} allocations" })
                        }
                    } else {
                        expected = U64.from_str(expectation) ?? 18446744073709551615
                        if allocations == expected {
                            Ok({})
                        } else {
                            Err({ case_id, message: "expected ${expected.to_str()}, got ${allocations.to_str()} allocations" })
                        }
                    }
                }
            }
        }
        [case_id, ..] => Err({ case_id, message: "malformed case row" })
        _ => Err({ case_id: "unknown", message: "malformed case row" })
    }
}

decode_hex = |hex| {
    if hex == "" {
        Ok("")
    } else {
        parsed = hex.split_on(",").map(|byte| U8.from_str("0x${byte}"))
        match keep_oks(parsed) {
            Err(error) => Err(error)
            Ok(bytes) => Str.from_utf8(bytes)
        }
    }
}

keep_oks = |items| {
    items.fold(Ok([]), |state, item| {
        values = state?
        value = item?
        Ok(values.append(value))
    })
}

fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
