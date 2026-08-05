import ByteRange
import Scalar

CursorStatus : [Open, Ended, Faulted]

CursorState : {
    accumulator : U32,
    expected_width : U8,
    seen_width : U8,
    sequence_start : U64,
    byte_offset : U64,
    scalar_index : U64,
    status : CursorStatus,
}

## Incremental decoding of arbitrary UTF-8 byte chunks.
##
## Use this module at file, network, and other untrusted-byte boundaries. A
## valid Roc `Str` needs no decode error channel; use `Scalar.iter` there.
Utf8 :: [].{
    Problem := [
        InvalidStartByte,
        UnexpectedEndOfSequence,
        ExpectedContinuation,
        OverlongEncoding,
        EncodesSurrogateHalf,
        CodePointTooLarge,
    ]

    ## A malformed sequence with absolute logical-source coordinates.
    ##
    ## `sequence_start` identifies its leading byte. `offset` identifies the
    ## byte where malformed input was detected, or the logical end offset where
    ## a continuation byte was missing. For errors concerning the completed
    ## value, both offsets equal `sequence_start`.
    DecodeError : {
        problem : Problem,
        offset : U64,
        sequence_start : U64,
    }

    ## A raw UTF-8 stream decoder with at most three pending bytes.
    ##
    ## The cursor is sealed and must be explicitly finished. Its representation
    ## keeps only a partial scalar, absolute coordinates, and terminal status;
    ## a returned cursor never retains a consumed chunk.
    Cursor :: { state : CursorState }.{
        Error : [
            Malformed(DecodeError),
            OffsetOverflow({ at : U64 }),
            ScalarIndexOverflow({ at : U64 }),
            AlreadyFinished,
            AlreadyFailed,
            InternalFault,
        ]

        ## Begin decoding at byte offset and scalar index zero.
        ##
        ## This is constant time and does not allocate.
        init : {} -> Cursor
        init = |{}| {
            state: {
                accumulator: 0,
                expected_width: 0,
                seen_width: 0,
                sequence_start: 0,
                byte_offset: 0,
                scalar_index: 0,
                status: Open,
            },
        }

        ## Decode one arbitrary byte chunk and fold every completed scalar into
        ## caller state.
        ##
        ## `Pushed` means the whole chunk was accepted but does not mean end of
        ## text. `Failed` is terminal and returns both the failed cursor and the
        ## state containing any earlier, irrevocably decoded scalars. Callers
        ## must not present that state as a complete decode.
        ##
        ## Work is O(B) in chunk bytes with constant stack and auxiliary state.
        ## The decoder allocates no list or string and does not retain `chunk`.
        ## The callback controls any allocation in its own state.
        push : Cursor, List(U8), state, (state, Scalar.LocatedScalar -> state) -> [
            Pushed({ cursor : Cursor, state : state }),
            Failed({ cursor : Cursor, state : state, error : Error }),
        ]
        push = |cursor, chunk, initial_fold_state, emit| {
            initial = cursor.state
            match initial.status {
                Ended => return Failed({ cursor, state: initial_fold_state, error: AlreadyFinished })
                Faulted => return Failed({ cursor, state: initial_fold_state, error: AlreadyFailed })
                Open => {}
            }

            var $current = initial
            var $fold_state = initial_fold_state

            for byte in chunk {
                match step_byte($current, byte) {
                    Continue(next) => {
                        $current = next
                    }
                    Ready({ state: next, value, byte_start, byte_end, scalar_index }) => {
                        scalar = match Scalar.from_u32(value) {
                            Err(_) => {
                                failed = faulted(next)
                                return Failed({
                                    cursor: { state: failed },
                                    state: $fold_state,
                                    error: InternalFault,
                                })
                            }
                            Ok(scalar_value) => scalar_value
                        }
                        byte_range = match ByteRange.from_bounds(byte_start, byte_end) {
                            Err(_) => {
                                failed = faulted(next)
                                return Failed({
                                    cursor: { state: failed },
                                    state: $fold_state,
                                    error: InternalFault,
                                })
                            }
                            Ok(range) => range
                        }

                        $fold_state = emit($fold_state, {
                            scalar,
                            byte_range,
                            scalar_index,
                        })
                        $current = next
                    }
                    MalformedStep({ state: failed_state, error }) => {
                        return Failed({
                            cursor: { state: faulted(failed_state) },
                            state: $fold_state,
                            error: Malformed(error),
                        })
                    }
                    OffsetOverflowStep({ state: failed_state, at }) => {
                        return Failed({
                            cursor: { state: faulted(failed_state) },
                            state: $fold_state,
                            error: OffsetOverflow({ at: at }),
                        })
                    }
                    ScalarIndexOverflowStep({ state: failed_state, at }) => {
                        return Failed({
                            cursor: { state: faulted(failed_state) },
                            state: $fold_state,
                            error: ScalarIndexOverflow({ at: at }),
                        })
                    }
                }
            }

            Pushed({ cursor: { state: $current }, state: $fold_state })
        }

        ## Explicitly mark the logical end of the byte source.
        ##
        ## `End` reports the absolute byte length and decoded scalar count. An
        ## incomplete trailing sequence becomes `UnexpectedEndOfSequence` at
        ## the end offset. Calling `finish` twice, finishing a failed cursor, or
        ## pushing after either terminal state returns a typed state error.
        ## This is constant time and does not allocate.
        finish : Cursor -> [
            End({ cursor : Cursor, byte_offset : U64, scalar_count : U64 }),
            Failed({ cursor : Cursor, error : Error }),
        ]
        finish = |cursor| {
            current = cursor.state
            match current.status {
                Ended => Failed({ cursor, error: AlreadyFinished })
                Faulted => Failed({ cursor, error: AlreadyFailed })
                Open => {
                    if current.expected_width != 0 {
                        failed = faulted(current)
                        Failed({
                            cursor: { state: failed },
                            error: Malformed({
                                problem: UnexpectedEndOfSequence,
                                offset: current.byte_offset,
                                sequence_start: current.sequence_start,
                            }),
                        })
                    } else {
                        ended = with_status(current, Ended)
                        End({
                            cursor: { state: ended },
                            byte_offset: current.byte_offset,
                            scalar_count: current.scalar_index,
                        })
                    }
                }
            }
        }
    }
}

step_byte : CursorState, U8 -> [
    Continue(CursorState),
    Ready({
        state : CursorState,
        value : U32,
        byte_start : U64,
        byte_end : U64,
        scalar_index : U64,
    }),
    MalformedStep({ state : CursorState, error : Utf8.DecodeError }),
    OffsetOverflowStep({ state : CursorState, at : U64 }),
    ScalarIndexOverflowStep({ state : CursorState, at : U64 }),
]
step_byte = |state, byte| {
    if state.expected_width == 0 {
        if byte < 0x80 {
            byte_end = match state.byte_offset.plus_try(1) {
                Err(Overflow) => return OffsetOverflowStep({ state, at: state.byte_offset })
                Ok(offset) => offset
            }
            next_scalar_index = match state.scalar_index.plus_try(1) {
                Err(Overflow) => {
                    consumed = {
                        accumulator: byte.to_u32(),
                        expected_width: 1,
                        seen_width: 1,
                        sequence_start: state.byte_offset,
                        byte_offset: byte_end,
                        scalar_index: state.scalar_index,
                        status: Open,
                    }
                    return ScalarIndexOverflowStep({ state: consumed, at: state.byte_offset })
                }
                Ok(index) => index
            }

            Ready({
                state: empty_sequence(byte_end, next_scalar_index),
                value: byte.to_u32(),
                byte_start: state.byte_offset,
                byte_end,
                scalar_index: state.scalar_index,
            })
        } else if byte >= 0xC0 and byte <= 0xDF {
            byte_end = match state.byte_offset.plus_try(1) {
                Err(Overflow) => return OffsetOverflowStep({ state, at: state.byte_offset })
                Ok(offset) => offset
            }
            Continue(start_sequence(state, byte.bitwise_and(0x1F).to_u32(), 2, byte_end))
        } else if byte >= 0xE0 and byte <= 0xEF {
            byte_end = match state.byte_offset.plus_try(1) {
                Err(Overflow) => return OffsetOverflowStep({ state, at: state.byte_offset })
                Ok(offset) => offset
            }
            Continue(start_sequence(state, byte.bitwise_and(0x0F).to_u32(), 3, byte_end))
        } else if byte >= 0xF0 and byte <= 0xF4 {
            byte_end = match state.byte_offset.plus_try(1) {
                Err(Overflow) => return OffsetOverflowStep({ state, at: state.byte_offset })
                Ok(offset) => offset
            }
            Continue(start_sequence(state, byte.bitwise_and(0x07).to_u32(), 4, byte_end))
        } else {
            MalformedStep({
                state,
                error: {
                    problem: InvalidStartByte,
                    offset: state.byte_offset,
                    sequence_start: state.byte_offset,
                },
            })
        }
    } else if byte < 0x80 or byte > 0xBF {
        MalformedStep({
            state,
            error: {
                problem: ExpectedContinuation,
                offset: state.byte_offset,
                sequence_start: state.sequence_start,
            },
        })
    } else {
        byte_end = match state.byte_offset.plus_try(1) {
            Err(Overflow) => return OffsetOverflowStep({ state, at: state.byte_offset })
            Ok(offset) => offset
        }
        accumulator = state.accumulator
            .shl_wrap(6)
            .bitwise_or(byte.bitwise_and(0x3F).to_u32())
        seen_width = state.seen_width + 1

        if seen_width < state.expected_width {
            Continue({
                accumulator,
                expected_width: state.expected_width,
                seen_width,
                sequence_start: state.sequence_start,
                byte_offset: byte_end,
                scalar_index: state.scalar_index,
                status: Open,
            })
        } else {
            completed = {
                accumulator,
                expected_width: state.expected_width,
                seen_width,
                sequence_start: state.sequence_start,
                byte_offset: byte_end,
                scalar_index: state.scalar_index,
                status: Open,
            }

            match completed_problem(accumulator, state.expected_width) {
                Invalid(problem) => {
                    MalformedStep({
                        state: completed,
                        error: {
                            problem,
                            offset: state.sequence_start,
                            sequence_start: state.sequence_start,
                        },
                    })
                }
                Valid => {
                    next_scalar_index = match state.scalar_index.plus_try(1) {
                        Err(Overflow) => {
                            return ScalarIndexOverflowStep({
                                state: completed,
                                at: state.sequence_start,
                            })
                        }
                        Ok(index) => index
                    }

                    Ready({
                        state: empty_sequence(byte_end, next_scalar_index),
                        value: accumulator,
                        byte_start: state.sequence_start,
                        byte_end,
                        scalar_index: state.scalar_index,
                    })
                }
            }
        }
    }
}

start_sequence : CursorState, U32, U8, U64 -> CursorState
start_sequence = |state, accumulator, expected_width, byte_end| {
    {
        accumulator,
        expected_width,
        seen_width: 1,
        sequence_start: state.byte_offset,
        byte_offset: byte_end,
        scalar_index: state.scalar_index,
        status: Open,
    }
}

completed_problem : U32, U8 -> [Valid, Invalid(Utf8.Problem)]
completed_problem = |value, width| {
    if (width == 2 and value < 0x80) or (width == 3 and value < 0x800) or (width == 4 and value < 0x10000) {
        Invalid(OverlongEncoding)
    } else if value >= 0xD800 and value <= 0xDFFF {
        Invalid(EncodesSurrogateHalf)
    } else if value > 0x10FFFF {
        Invalid(CodePointTooLarge)
    } else {
        Valid
    }
}

empty_sequence : U64, U64 -> CursorState
empty_sequence = |byte_offset, scalar_index| {
    {
        accumulator: 0,
        expected_width: 0,
        seen_width: 0,
        sequence_start: byte_offset,
        byte_offset,
        scalar_index,
        status: Open,
    }
}

faulted : CursorState -> CursorState
faulted = |state| with_status(state, Faulted)

with_status : CursorState, CursorStatus -> CursorState
with_status = |state, status| {
    {
        accumulator: state.accumulator,
        expected_width: state.expected_width,
        seen_width: state.seen_width,
        sequence_start: state.sequence_start,
        byte_offset: state.byte_offset,
        scalar_index: state.scalar_index,
        status,
    }
}
