import ByteRange
import InternalSentenceData
import InternalUtf8

Class : InternalSentenceData.Class

Tail : [
	NoTerminator,
	DirectPeriod({ follows_case : Bool }),
	PeriodClose,
	PeriodSpace,
	TerminatorClose,
	TerminatorSpace,
]

MachineState : {
	started : Bool,
	range_start : U64,
	previous_raw : Class,
	previous_significant : Class,
	tail : Tail,
	pending : [NoPending, Pending(U64)],
}

Emission : [NoBoundary, Boundary(ByteRange)]

IterState : {
	cursor : InternalUtf8.Cursor,
	machine : MachineState,
	queued : [NoQueued, Queued(ByteRange)],
	finished : Bool,
}

## Shared UAX #29 C3-1 transition core for complete and chunked sources.
InternalSentence :: [].{
	Machine : MachineState

	init : {} -> Machine
	init = |_| {
		{
			started: Bool.False,
			range_start: 0,
			previous_raw: Other,
			previous_significant: Other,
			tail: NoTerminator,
			pending: NoPending,
		}
	}

	push : Machine, U32, U64 -> { machine : Machine, emission : Emission }
	push = |machine, scalar, at| {
		current = InternalSentenceData.lookup(scalar)
		if !machine.started {
			return {
				machine: { ..advance(machine, current), started: Bool.True, range_start: at },
				emission: NoBoundary,
			}
		}

		# SB3 and SB4 use raw adjacency and precede the SB5 ignore rule.
		if is_paragraph_separator(machine.previous_raw) {
			advanced = advance(machine, current)
			return if machine.previous_raw == CR and current == LF {
				{ machine: advanced, emission: NoBoundary }
			} else {
				emit_boundary(advanced, at)
			}
		}
		if current == Extend or current == Format {
			return { machine: { ..machine, previous_raw: current }, emission: NoBoundary }
		}

		advanced = advance(machine, current)
		match machine.pending {
			Pending(pending_at) => {
				# Only SB8 can defer a decision. Its intervening classes cannot
				# start a new terminator tail, so this is the only live cut.
				if current == Lower {
					{ machine: { ..advanced, pending: NoPending }, emission: NoBoundary }
				} else if stops_lookahead(current) {
					emit_boundary({ ..advanced, pending: NoPending }, pending_at)
				} else {
					{ machine: advanced, emission: NoBoundary }
				}
			}
			NoPending => match decide(machine.tail, current) {
				Join => { machine: advanced, emission: NoBoundary }
				Break => emit_boundary(advanced, at)
				Defer => { machine: { ..advanced, pending: Pending(at) }, emission: NoBoundary }
			}
		}
	}

	finish : Machine, U64 -> [NoRanges, Final(ByteRange), PendingAndFinal(ByteRange, ByteRange)]
	finish = |machine, byte_end| {
		if !machine.started {
			NoRanges
		} else {
			match machine.pending {
				NoPending => Final(range(machine.range_start, byte_end))
				Pending(at) => PendingAndFinal(range(machine.range_start, at), range(at, byte_end))
			}
		}
	}

	fold_chunk : Machine, U64, Str, state, (state, ByteRange -> state) -> { machine : Machine, state : state }
	fold_chunk = |machine, absolute_start, chunk, initial, emit| {
		InternalUtf8.fold_scalars(
			chunk,
			{ machine, state: initial },
			|fold, scalar, local_start, _local_end, _local_scalar_index| {
				# The caller checked absolute_start + the full chunk length.
				transition = InternalSentence.push(fold.machine, scalar, absolute_start + local_start)
				next_state = match transition.emission {
					NoBoundary => fold.state
					Boundary(item) => emit(fold.state, item)
				}
				{ machine: transition.machine, state: next_state }
			},
		)
	}

	iter_ranges : Str -> Iter(ByteRange)
	iter_ranges = |source| {
		Iter.custom(
			{
				cursor: InternalUtf8.init(source),
				machine: InternalSentence.init({}),
				queued: NoQueued,
				finished: Bool.False,
			},
			Err(Unknown),
			next_range,
		)
	}
}

advance : MachineState, Class -> MachineState
advance = |machine, current| {
	tail = match current {
		ATerm => DirectPeriod({ follows_case: machine.previous_significant == Upper or machine.previous_significant == Lower })
		STerm => TerminatorClose
		Close => match machine.tail {
			DirectPeriod(_) | PeriodClose => PeriodClose
			TerminatorClose => TerminatorClose
			_ => NoTerminator
		}
		Sp => match machine.tail {
			DirectPeriod(_) | PeriodClose | PeriodSpace => PeriodSpace
			TerminatorClose | TerminatorSpace => TerminatorSpace
			NoTerminator => NoTerminator
		}
		_ => NoTerminator
	}
	{ ..machine, previous_raw: current, previous_significant: current, tail }
}

decide : Tail, Class -> [Join, Break, Defer]
decide = |tail, current| {
	if tail == NoTerminator {
		return Join # SB998
	}
	match tail {
		DirectPeriod({ follows_case }) => {
			if current == Numeric or (follows_case and current == Upper) {
				return Join # SB6 and SB7
			}
		}
		_ => {}
	}

	# SB8, SB8a, SB9, and SB10 all suppress the same cut. Evaluate
	# known suppressions before deferring a cut for SB8 right context.
	if current == SContinue or is_terminator(current) or current == Sp or is_paragraph_separator(current) {
		return Join
	}
	if current == Close and tail != PeriodSpace and tail != TerminatorSpace {
		return Join
	}
	match tail {
		DirectPeriod(_) | PeriodClose | PeriodSpace => {
			if current == Lower {
				Join
			} else if stops_lookahead(current) {
				Break
			} else {
				Defer
			}
		}
		_ => Break # SB11
	}
}

is_paragraph_separator : Class -> Bool
is_paragraph_separator = |value| value == Sep or value == CR or value == LF

is_terminator : Class -> Bool
is_terminator = |value| value == ATerm or value == STerm

stops_lookahead : Class -> Bool
stops_lookahead = |value| value == OLetter or value == Upper or is_paragraph_separator(value) or is_terminator(value)

# The machine advances offsets only in source order and saves a pending cut
# after the current range start. Both emitted ends therefore bound nonempty
# ranges. The finish offset follows all consumed scalars.
range : U64, U64 -> ByteRange
range = |start, end| ByteRange.from_bounds(start, end) ?? ...

emit_boundary : MachineState, U64 -> { machine : MachineState, emission : Emission }
emit_boundary = |machine, at| {
	{ machine: { ..machine, range_start: at }, emission: Boundary(range(machine.range_start, at)) }
}

next_range : IterState -> Try((ByteRange, IterState), [NoMore])
next_range = |state| {
	match state.queued {
		Queued(item) => return Ok((item, { ..state, queued: NoQueued }))
		NoQueued => {}
	}
	if state.finished {
		return Err(NoMore)
	}
	var $cursor = state.cursor
	var $machine = state.machine
	while Bool.True {
		match InternalUtf8.next($cursor) {
			Done => {
				final_state = { cursor: $cursor, machine: $machine, queued: NoQueued, finished: Bool.True }
				match InternalSentence.finish($machine, $cursor.byte_offset) {
					NoRanges => return Err(NoMore)
					Final(item) => return Ok((item, final_state))
					PendingAndFinal(first, last) => return Ok((first, { ..final_state, queued: Queued(last) }))
				}
			}
			One({ item, rest }) => {
				result = InternalSentence.push($machine, item.scalar, item.byte_start)
				$cursor = rest
				$machine = result.machine
				match result.emission {
					NoBoundary => {}
					Boundary(found) => return Ok((found, { cursor: $cursor, machine: $machine, queued: NoQueued, finished: Bool.False }))
				}
			}
		}
	}
}
