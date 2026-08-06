import InternalUtf8
import TextPosition
import TextRange

## Shared allocation-free scanner for maximal adjacent runs of one typed
## scalar property. Every emitted run carries the package's sealed
## `TextRange` coordinate pair.
InternalPropertyRuns :: [].{
    Run(value) : {
        range : TextRange,
        value : value,
    }

    fold : Str, state, (U32 -> value), (value, value -> Bool), (state, Run(value) -> state) -> state
    fold = |source, initial, lookup, is_eq, emit| {
        scanned = InternalUtf8.fold_scalars(
            source,
            { state: initial, active: NoRun },
            |scan, scalar, byte_start, byte_end, scalar_index| {
                value = lookup(scalar)
                match scan.active {
                    NoRun => {
                        { state: scan.state, active: Active({ byte_start, byte_end, scalar_start: scalar_index, scalar_end: scalar_index + 1, value }) }
                    }
                    Active(run) => {
                        if is_eq(run.value, value) {
                            { state: scan.state, active: Active({
                                byte_start: run.byte_start,
                                byte_end,
                                scalar_start: run.scalar_start,
                                scalar_end: scalar_index + 1,
                                value: run.value,
                            }) }
                        } else {
                            next_state = emit_raw(scan.state, run, emit)
                            { state: next_state, active: Active({ byte_start, byte_end, scalar_start: scalar_index, scalar_end: scalar_index + 1, value }) }
                        }
                    }
                }
            },
        )

        match scanned.active {
            NoRun => scanned.state
            Active(run) => emit_raw(scanned.state, run, emit)
        }
    }

    iter : Str, (U32 -> value), (value, value -> Bool) -> Iter(Run(value))
    iter = |source, lookup, is_eq| {
        next_run = |initial| {
            first = match initial.pending {
                Pending(item) => One({ item, rest: initial.utf8 })
                NoPending => InternalUtf8.next(initial.utf8)
            }

            match first {
                Done => Err(NoMore)
                One({ item, rest }) => {
                    value = lookup(item.scalar)
                    var cursor = rest
                    var byte_end = item.byte_end
                    var scalar_end = item.scalar_index + 1

                    while Bool.True {
                        match InternalUtf8.next(cursor) {
                            Done => {
                                match make_run(item.byte_start, byte_end, item.scalar_index, scalar_end, value) {
                                    Err(_) => return Err(NoMore)
                                    Ok(run) => return Ok((run, { utf8: cursor, pending: NoPending }))
                                }
                            }
                            One({ item: candidate, rest: after_candidate }) => {
                                candidate_value = lookup(candidate.scalar)
                                if is_eq(value, candidate_value) {
                                    byte_end = candidate.byte_end
                                    scalar_end = candidate.scalar_index + 1
                                    cursor = after_candidate
                                } else {
                                    match make_run(item.byte_start, byte_end, item.scalar_index, scalar_end, value) {
                                        Err(_) => return Err(NoMore)
                                        Ok(run) => return Ok((run, { utf8: after_candidate, pending: Pending(candidate) }))
                                    }
                                }
                            }
                        }
                    }

                    Err(NoMore)
                }
            }
        }

        Iter.custom(
            { utf8: InternalUtf8.init(source), pending: NoPending },
            Unknown,
            next_run,
        )
    }
}

make_run = |byte_start, byte_end, scalar_start, scalar_end, value| {
    start = TextPosition.from_offsets(byte_start, scalar_start)
    end = TextPosition.from_offsets(byte_end, scalar_end)
    match TextRange.from_positions(start, end) {
        Err(error) => Err(error)
        Ok(range) => Ok({ range, value })
    }
}

emit_raw = |state, run, emit| {
    match make_run(run.byte_start, run.byte_end, run.scalar_start, run.scalar_end, run.value) {
        Err(_) => state
        Ok(public_run) => emit(state, public_run)
    }
}
