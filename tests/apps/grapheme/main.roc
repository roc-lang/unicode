app [run!] {
    pf: platform "../../platform/main.roc",
    unicode: "../../../package/main.roc",
}

import unicode.ByteRange
import unicode.Grapheme
import unicode.Scalar

run! : Str => Str
run! = |input| {
    lines = input.split_on("\n").drop_if(|line| line == "")
    match lines {
        [] => fail("header", "empty input")
        [header, .. as cases] => {
            match parse_header(header, "grapheme", cases.len()) {
                Err(message) => fail("header", message)
                Ok({}) => match run_cases(cases, 0) {
                    Ok(count) => "PASS\tgrapheme\t${count.to_str()}"
                    Err({ case_id, message }) => fail(case_id, message)
                }
            }
        }
    }
}

parse_header : Str, Str, U64 -> Try({}, Str)
parse_header = |header, suite, actual_count| {
    match header.split_on("\t") {
        ["ROC_UNICODE_TEST_V1", got_suite, count_str] if got_suite == suite => {
            expected_count = U64.from_str(count_str) ?? return Err("invalid header count")
            if expected_count == actual_count {
                Ok({})
            } else {
                Err("header count ${expected_count.to_str()} != ${actual_count.to_str()}")
            }
        }
        _ => Err("malformed protocol header")
    }
}

run_cases : List(Str), U64 -> Try(U64, { case_id : Str, message : Str })
run_cases = |remaining, count| {
    match remaining {
        [] => Ok(count)
        [line, .. as rest] => {
            match run_case(line) {
                Ok({}) => run_cases(rest, count + 1)
                Err(error) => Err(error)
            }
        }
    }
}

run_case : Str -> Try({}, { case_id : Str, message : Str })
run_case = |line| {
    match line.split_on("\t") {
        [case_id, code_points_hex, expected_offsets_str] => {
            code_points = code_points_hex.split_on(",").map(|hex| U32.from_str("0x${hex}"))
            expected_offsets = expected_offsets_str.split_on(",").map(U64.from_str)
            match (keep_oks(code_points), keep_oks(expected_offsets)) {
                (Ok(cps), Ok(expected)) => {
                    source = keep_oks(cps.map(scalar_to_str))
                    match source {
                        Err(_) => Err({ case_id, message: "could not encode source scalars" })
                        Ok(parts) => {
                            str = Str.join_with(parts, "")
                            got = break_offsets(Grapheme.ranges(str))
                            if got == expected {
                                Ok({})
                            } else {
                                Err({
                                    case_id,
                                    message: "expected ${Str.inspect(expected)}, got ${Str.inspect(got)}",
                                })
                            }
                        }
                    }
                }
                _ => Err({ case_id, message: "malformed numeric field" })
            }
        }
        [case_id, ..] => Err({ case_id, message: "malformed case row" })
        _ => Err({ case_id: "unknown", message: "malformed case row" })
    }
}

scalar_to_str : U32 -> Try(Str, [InvalidScalar, InternalEncodingFault])
scalar_to_str = |value| {
    match Scalar.from_u32(value) {
        Err(_) => {
            error : [InvalidScalar, InternalEncodingFault]
            error = InvalidScalar
            Err(error)
        }
        Ok(scalar) => {
            match Scalar.to_str(scalar) {
                Ok(encoded) => Ok(encoded)
                Err(_) => {
                    error : [InvalidScalar, InternalEncodingFault]
                    error = InternalEncodingFault
                    Err(error)
                }
            }
        }
    }
}

break_offsets : List(ByteRange) -> List(U64)
break_offsets = |ranges| {
    ranges.fold([0], |offsets, range| {
        offsets.append(ByteRange.end(range))
    })
}

keep_oks : List(Try(a, err)) -> Try(List(a), err)
keep_oks = |items| {
    items.fold(Ok([]), |state, item| {
        values = state?
        value = item?
        Ok(values.append(value))
    })
}

fail : Str, Str -> Str
fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
