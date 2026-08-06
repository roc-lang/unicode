app [run!] {
    pf: platform "../../platform/main.roc",
    unicode: "../../../package/main.roc",
}

import unicode.LineBreak
import unicode.Scalar
import unicode.TextPosition

run! : Str => Str
run! = |input| {
    match run_focused(input.count_utf8_bytes()) {
        Err(message) => return fail("focused", message)
        Ok({}) => {}
    }
    lines = input.split_on("\n").drop_if(|line| line == "")
    match lines {
        [] => fail("header", "empty input")
        [header, .. as cases] => {
            match parse_header(header, "line-break", cases.len()) {
                Err(message) => fail("header", message)
                Ok({}) => match run_cases(cases, 0) {
                    Ok(count) => "PASS\tline-break\t${count.to_str()}"
                    Err({ case_id, message }) => fail(case_id, message)
                }
            }
        }
    }
}

run_focused : U64 -> Try({}, Str)
run_focused = |runtime_seed| {
    zero = runtime_seed - runtime_seed
    verify_profile_revision(LineBreak.default_profile)?
    verify_profile_revision(PreserveGraphemes)?
    empty = boundary_shape(LineBreak.boundaries(""))
    expected_empty = [
        (zero, zero, Prohibited, NonTailorable),
        (zero, zero, Mandatory, NonTailorable),
    ]
    if empty != expected_empty {
        return Err("empty start/end decisions drifted: ${Str.inspect(empty)}")
    }

    crlf = boundary_shape(LineBreak.boundaries("\r\n"))
    expected_crlf = [
        (zero, zero, Prohibited, NonTailorable),
        (zero + 1, zero + 1, Prohibited, NonTailorable),
        (zero + 2, zero + 2, Mandatory, NonTailorable),
    ]
    if crlf != expected_crlf {
        return Err("CRLF authority/status drifted: ${Str.inspect(crlf)}")
    }

    zw_source = source_from_code_points([0x200B, 0x0061])?
    zw = boundary_shape(LineBreak.boundaries(zw_source))
    if zw.get(1) != Ok((3.U64, 1.U64, Allowed, NonTailorable)) {
        return Err("LB8 must remain an allowed non-tailorable boundary")
    }

    glue_source = source_from_code_points([0x0061, 0x00A0, 0x0062])?
    glue = boundary_shape(LineBreak.boundaries(glue_source))
    if glue.get(1) != Ok((1.U64, 1.U64, Prohibited, Tailorable))
        or glue.get(2) != Ok((3.U64, 2.U64, Prohibited, NonTailorable)) {
        return Err("LB12/LB12a authority boundary drifted: ${Str.inspect(glue)}")
    }

    prepend_source = source_from_code_points([0x0600, 0x4E00])?
    default_prepend = LineBreak.boundaries(prepend_source)
    preserved_prepend = LineBreak.boundaries_with(prepend_source, PreserveGraphemes)
    default_decision = match default_prepend.get(1) {
        Ok(event) => event.decision
        Err(_) => return Err("default profile omitted the inner boundary")
    }
    preserved_decision = match preserved_prepend.get(1) {
        Ok(event) => event.decision
        Err(_) => return Err("grapheme profile omitted the inner boundary")
    }
    if default_decision != Allowed or preserved_decision != Prohibited {
        return Err("PreserveGraphemes did not suppress only the tailorable opportunity")
    }

    initial = LineBreak.Cursor.init({})
    pushed = match LineBreak.Cursor.push(initial, "", [], |events, event| events.append(event)) {
        Failed({ error, .. }) => return Err("empty cursor push failed: ${Str.inspect(error)}")
        Pushed(value) => value
    }
    finished = match LineBreak.Cursor.finish(
        pushed.cursor,
        pushed.state,
        |events, event| events.append(event),
    ) {
        Failed({ error, .. }) => return Err("empty cursor finish failed: ${Str.inspect(error)}")
        End(value) => value
    }
    if opportunity_offsets(finished.state) != [0] {
        return Err("empty chunk cursor did not emit exactly LB3")
    }
    match LineBreak.Cursor.finish(
        finished.cursor,
        [],
        |events, event| events.append(event),
    ) {
        Failed({ error: AlreadyFinished, .. }) => {}
        _ => return Err("cursor finish is not sealed")
    }
    match LineBreak.Cursor.push(
        finished.cursor,
        "a",
        [],
        |events, event| events.append(event),
    ) {
        Failed({ error: AlreadyFinished, consumed: 0, .. }) => {}
        _ => return Err("cursor accepted a chunk after finish")
    }

    ascii_run = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    verify_block_cursor(ascii_run, UnicodeDefault)?
    verify_block_cursor(Str.join_with(["$(", ascii_run, ") 19"], ""), UnicodeDefault)?
    verify_block_cursor(Str.join_with([ascii_run, "\r\n", ascii_run], ""), UnicodeDefault)?
    verify_block_cursor(ascii_run, PreserveGraphemes)?

    Ok({})
}

verify_profile_revision : LineBreak.Profile -> Try({}, Str)
verify_profile_revision = |profile| {
    match (profile, LineBreak.profile_revision(profile)) {
        (UnicodeDefault, None) => Ok({})
        (PreserveGraphemes, Some(PreserveGraphemesV1)) => Ok({})
        _ => Err("line-break profile revision axis drifted")
    }
}

verify_block_cursor : Str, LineBreak.Profile -> Try({}, Str)
verify_block_cursor = |source, profile| {
    expected = opportunity_shape(LineBreak.opportunities_with(source, profile))
    pushed = match LineBreak.Cursor.push(
        LineBreak.Cursor.init_with(profile),
        source,
        [],
        |events, event| events.append(event),
    ) {
        Failed({ error, .. }) => return Err("ASCII block cursor push failed: ${Str.inspect(error)}")
        Pushed(value) => value
    }
    finished = match LineBreak.Cursor.finish(
        pushed.cursor,
        pushed.state,
        |events, event| events.append(event),
    ) {
        Failed({ error, .. }) => return Err("ASCII block cursor finish failed: ${Str.inspect(error)}")
        End(value) => value
    }
    got = opportunity_shape(finished.state)
    if got == expected {
        Ok({})
    } else {
        Err("ASCII block cursor disagreed with replayable traversal")
    }
}

boundary_shape : List(LineBreak.BreakBoundary) -> List((U64, U64, LineBreak.Decision, LineBreak.Authority))
boundary_shape = |events| events.map(|event| {
    (
        TextPosition.byte_offset(event.at),
        TextPosition.scalar_offset(event.at),
        event.decision,
        event.authority,
    )
})

source_from_code_points : List(U32) -> Try(Str, Str)
source_from_code_points = |code_points| {
    match keep_oks(code_points.map(scalar_to_str)) {
        Err(_) => Err("could not encode focused scalar source")
        Ok(parts) => Ok(Str.join_with(parts, ""))
    }
}

parse_header : Str, Str, U64 -> Try({}, Str)
parse_header = |header, suite, actual_count| {
    match header.split_on("\t") {
        ["ROC_UNICODE_TEST_V1", got_suite, count_str] if got_suite == suite => {
            expected_count = U64.from_str(count_str) ?? return Err("invalid header count")
            if expected_count == actual_count Ok({}) else Err("header count mismatch")
        }
        _ => Err("malformed protocol header")
    }
}

run_cases : List(Str), U64 -> Try(U64, { case_id : Str, message : Str })
run_cases = |remaining, count| {
    match remaining {
        [] => Ok(count)
        [line, .. as rest] => {
            run_case(line)?
            run_cases(rest, count + 1)
        }
    }
}

run_case : Str -> Try({}, { case_id : Str, message : Str })
run_case = |line| {
    match line.split_on("\t") {
        [case_id, code_points_hex, expected_offsets_str] => {
            cps = keep_oks(code_points_hex.split_on(",").map(|hex| U32.from_str("0x${hex}")))
            expected = keep_oks(expected_offsets_str.split_on(",").map(U64.from_str))
            match (cps, expected) {
                (Ok(code_points), Ok(expected_offsets)) => {
                    parts = keep_oks(code_points.map(scalar_to_str))
                    match parts {
                        Err(_) => Err({ case_id, message: "could not encode source scalars" })
                        Ok(scalar_parts) => {
                            source = Str.join_with(scalar_parts, "")
                            exhaustive = LineBreak.boundaries(source)
                            opportunities = LineBreak.opportunities(source)
                            got = opportunity_offsets(opportunities)
                            chunked = chunked_offsets(scalar_parts)
                            if exhaustive.len() != code_points.len() + 1 {
                                Err({ case_id, message: "exhaustive boundary count mismatch" })
                            } else if opportunity_shape(opportunities) != opportunity_boundary_shape(exhaustive) {
                                Err({ case_id, message: "exhaustive and opportunity traversals disagree" })
                            } else if got != expected_offsets {
                                Err({
                                    case_id,
                                    message: "expected ${Str.inspect(expected_offsets)}, got ${Str.inspect(got)}",
                                })
                            } else if chunked != Ok(expected_offsets) {
                                Err({
                                    case_id,
                                    message: "chunk cursor expected ${Str.inspect(expected_offsets)}, got ${Str.inspect(chunked)}",
                                })
                            } else {
                                Ok({})
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

chunked_offsets : List(Str) -> Try(List(U64), LineBreak.Cursor.Error)
chunked_offsets = |parts| {
    initial = { cursor: LineBreak.Cursor.init({}), offsets: [] }
    pushed = parts.fold(Ok(initial), |state, part| {
        current = state?
        match LineBreak.Cursor.push(
            current.cursor,
            part,
            current.offsets,
            |offsets, event| offsets.append(TextPosition.byte_offset(event.at)),
        ) {
            Failed({ error, .. }) => Err(error)
            Pushed(result) => Ok({ cursor: result.cursor, offsets: result.state })
        }
    })?
    match LineBreak.Cursor.finish(
        pushed.cursor,
        pushed.offsets,
        |offsets, event| offsets.append(TextPosition.byte_offset(event.at)),
    ) {
        Failed({ error, .. }) => Err(error)
        End(finished) => Ok(finished.state)
    }
}

opportunity_offsets : List(LineBreak.BreakOpportunity) -> List(U64)
opportunity_offsets = |events| {
    events.map(|event| TextPosition.byte_offset(event.at))
}

OpportunityShape : (U64, U64, [Mandatory, Allowed], LineBreak.Authority)

opportunity_shape : List(LineBreak.BreakOpportunity) -> List(OpportunityShape)
opportunity_shape = |events| events.map(|event| {
    (
        TextPosition.byte_offset(event.at),
        TextPosition.scalar_offset(event.at),
        event.decision,
        event.authority,
    )
})

opportunity_boundary_shape : List(LineBreak.BreakBoundary) -> List(OpportunityShape)
opportunity_boundary_shape = |events| events.fold([], |shapes, event| {
    decision = match event.decision {
        Prohibited => return shapes
        Mandatory => Mandatory
        Allowed => Allowed
    }
    shapes.append((
        TextPosition.byte_offset(event.at),
        TextPosition.scalar_offset(event.at),
        decision,
        event.authority,
    ))
})

scalar_to_str : U32 -> Try(Str, [InvalidScalar, InternalEncodingFault])
scalar_to_str = |value| {
    match Scalar.from_u32(value) {
        Err(_) => {
            error : [InvalidScalar, InternalEncodingFault]
            error = InvalidScalar
            Err(error)
        }
        Ok(scalar) => match Scalar.to_str(scalar) {
            Ok(encoded) => Ok(encoded)
            Err(_) => {
                error : [InvalidScalar, InternalEncodingFault]
                error = InternalEncodingFault
                Err(error)
            }
        }
    }
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
