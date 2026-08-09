import ByteRange
import InternalUtf8
import InternalWordData

WordBreak : InternalWordData.WordBreak

WordProps : InternalWordData.Props

Pending : [
	NoPending,
	PendingMid({ at : U64, before_is_joined : Bool, left : WordBreak, middle : WordBreak }),
]

MachineState : {
	started : Bool,
	range_start : U64,
	previous_raw : WordBreak,
	previous_significant : WordBreak,
	ri_odd : Bool,
	pending : Pending,
}

Boundaries : [NoBoundaries, One(ByteRange), Two(ByteRange, ByteRange)]

RangeIterState : {
	cursor : InternalUtf8.Cursor,
	machine : MachineState,
	queued : [NoQueued, Queued(ByteRange)],
	finished : Bool,
}

## Private UAX #29 C2-1 transition core. `push` is the only place that
## classifies a scalar and decides a boundary; complete and chunked drivers use
## it unchanged.
InternalWord :: [].{
	Machine : MachineState
	Emission : Boundaries

	init : {} -> Machine
	init = |_| {
		{
			started: Bool.False,
			range_start: 0,
			previous_raw: Other,
			previous_significant: Other,
			ri_odd: Bool.False,
			pending: NoPending,
		}
	}

	push : Machine, U32, U64 -> { machine : Machine, emissions : Emission }
	push = |machine, scalar, byte_start| push_props(machine, InternalWordData.lookup(scalar), byte_start)

	finish : Machine, U64 -> Emission
	finish = |machine, byte_end| {
		if !machine.started {
			NoBoundaries
		} else {
			match machine.pending {
				NoPending => One(range(machine.range_start, byte_end))
				PendingMid({ at, before_is_joined, .. }) => {
					if before_is_joined {
						One(range(machine.range_start, byte_end))
					} else {
						before_middle = range(machine.range_start, at)
						middle = range(at, byte_end)
						Two(before_middle, middle)
					}
				}
			}
		}
	}

	fold_ranges : Str, state, (state, ByteRange -> state) -> state
	fold_ranges = |source, initial, emit| {
		folded = fold_chunk(InternalWord.init({}), 0, source, initial, emit)
		fold_emission(folded.state, InternalWord.finish(folded.machine, source.count_utf8_bytes()), emit)
	}

	fold_chunk : Machine, U64, Str, state, (state, ByteRange -> state) -> { machine : Machine, state : state }
	fold_chunk = |machine, absolute_start, chunk, initial, emit| {
		InternalUtf8.fold_scalars(
			chunk,
			{ machine, state: initial },
			|fold, scalar, local_start, _local_end, _scalar_index| {
				transition = InternalWord.push(fold.machine, scalar, absolute_start + local_start)
				{
					machine: transition.machine,
					state: fold_emission(fold.state, transition.emissions, emit),
				}
			},
		)
	}

	iter_ranges : Str -> Iter(ByteRange)
	iter_ranges = |source| {
		Iter.custom(
			{
				cursor: InternalUtf8.init(source),
				machine: InternalWord.init({}),
				queued: NoQueued,
				finished: Bool.False,
			},
			Unknown,
			next_range,
		)
	}

	ranges : Str -> List(ByteRange)
	ranges = |source| InternalWord.fold_ranges(source, [], |items, item| items.append(item))
}

range = |start, end| ByteRange.from_bounds(start, end) ?? ...

fold_emission = |state, emissions, emit| {
	match emissions {
		NoBoundaries => state
		One(item) => emit(state, item)
		Two(first, second) => emit(emit(state, first), second)
	}
}

next_range : RangeIterState -> Try((ByteRange, RangeIterState), [NoMore])
next_range = |state| {
	match state.queued {
		Queued(item) => return Ok((item, { ..state, queued: NoQueued }))
		NoQueued => {}
	}

	if state.finished {
		return Err(NoMore)
	}

	var cursor = state.cursor
	var machine = state.machine

	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => {
				match InternalWord.finish(machine, cursor.byte_offset) {
					NoBoundaries => return Err(NoMore)
					One(item) => return Ok((item, { cursor, machine, queued: NoQueued, finished: Bool.True }))
					Two(first, second) => return Ok((first, { cursor, machine, queued: Queued(second), finished: Bool.True }))
				}
			}
			One({ item, rest }) => {
				transition = InternalWord.push(machine, item.scalar, item.byte_start)
				cursor = rest
				machine = transition.machine
				match transition.emissions {
					NoBoundaries => {}
					One(result) => return Ok((result, { cursor, machine, queued: NoQueued, finished: Bool.False }))
					Two(first, second) => return Ok((first, { cursor, machine, queued: Queued(second), finished: Bool.False }))
				}
			}
		}
	}

	Err(NoMore)
}

push_props : MachineState, WordProps, U64 -> { machine : MachineState, emissions : Boundaries }
push_props = |machine, props, byte_start| {
	current = props.word_break
	if !machine.started {
		{
			machine: initial_machine(current, byte_start),
			emissions: NoBoundaries,
		}
	} else if is_ignored(current) {
		# WB4 is deliberately below WB3/3a/3b. An ignored scalar immediately
		# after a newline begins a new range; elsewhere it cannot resolve a
		# pending Mid* decision and leaves the significant state intact.
		if is_newline(machine.previous_raw) {
			advance_without_pending(machine, current, byte_start, Bool.True, NoPending)
		} else {
			{
				machine: { ..machine, previous_raw: current },
				emissions: NoBoundaries,
			}
		}
	} else {
		match machine.pending {
			NoPending => {
				if is_newline(machine.previous_raw) or is_newline(current) {
					break_before = !(machine.previous_raw == CR and current == LF)
					advance_without_pending(machine, current, byte_start, break_before, NoPending)
				} else {
					push_significant(machine, props, byte_start)
				}
			}
			# A pending Mid* must be resolved before applying WB3a/b to a
			# following newline. That preserves the already-deferred boundary.
			PendingMid(pending) => resolve_pending(machine, props, byte_start, pending)
		}
	}
}

initial_machine = |current, byte_start| {
	{
		started: Bool.True,
		range_start: byte_start,
		previous_raw: current,
		previous_significant: current,
		ri_odd: current == Regional_Indicator,
		pending: NoPending,
	}
}

push_significant = |machine, props, byte_start| {
	current = props.word_break
	if can_start_pending(machine.previous_significant, current) {
		{
			machine: advance_machine(
				machine,
				current,
				byte_start,
				Bool.False,
				PendingMid({
					at: byte_start,
					before_is_joined: machine.previous_significant == Hebrew_Letter and current == Single_Quote,
					left: machine.previous_significant,
					middle: current,
				}),
			),
			emissions: NoBoundaries,
		}
	} else {
		break_before = ordinary_break(machine, props)
		advance_without_pending(machine, current, byte_start, break_before, NoPending)
	}
}

resolve_pending = |machine, props, byte_start, pending| {
	current = props.word_break
	if pending_matches(pending, current) {
		# The deferred boundary before Mid* is now known not to exist. The
		# current scalar joins through WB6/7, WB7b/c, or WB11/12.
		advance_without_pending(machine, current, byte_start, Bool.False, NoPending)
	} else if pending.before_is_joined {
		# WB7a permanently joins Hebrew_Letter to Single_Quote. Only the
		# following boundary remains conditional on the next significant scalar.
		fallback = {
			..machine,
			previous_significant: pending.middle,
			pending: NoPending,
		}
		break_before_current = ordinary_break(fallback, props)
		after = advance_machine(fallback, current, byte_start, break_before_current, NoPending)
		if break_before_current {
			{ machine: after, emissions: One(range(machine.range_start, byte_start)) }
		} else {
			{ machine: after, emissions: NoBoundaries }
		}
	} else {
		# The earlier boundary is now irrevocable. The current scalar may still
		# join the middle through WB3c (ZWJ × Extended_Pictographic); otherwise
		# WB999 gives the second boundary.
		before_middle = range(machine.range_start, pending.at)
		fallback = {
			..machine,
			range_start: pending.at,
			previous_significant: pending.middle,
			pending: NoPending,
		}
		break_before_current = ordinary_break(fallback, props)
		after = advance_machine(fallback, current, byte_start, break_before_current, NoPending)
		if break_before_current {
			{ machine: after, emissions: Two(before_middle, range(pending.at, byte_start)) }
		} else {
			{ machine: after, emissions: One(before_middle) }
		}
	}
}

advance_without_pending = |machine, current, byte_start, break_before, pending| {
	before = if break_before {
		One(range(machine.range_start, byte_start))
	} else {
		NoBoundaries
	}
	{ machine: advance_machine(machine, current, byte_start, break_before, pending), emissions: before }
}

advance_machine = |machine, current, byte_start, break_before, pending| {
	previous_significant = if is_ignored(current) and !break_before {
		machine.previous_significant
	} else {
		current
	}
	next_ri_odd = if current == Regional_Indicator {
		if !break_before and machine.previous_significant == Regional_Indicator {
			!machine.ri_odd
		} else {
			Bool.True
		}
	} else if is_ignored(current) and !break_before {
		machine.ri_odd
	} else {
		Bool.False
	}
	{
		started: Bool.True,
		range_start: if break_before {
			byte_start
		} else {
			machine.range_start
		},
		previous_raw: current,
		previous_significant,
		ri_odd: next_ri_odd,
		pending,
	}
}

ordinary_break = |machine, current| {
	previous_raw = machine.previous_raw
	previous = machine.previous_significant
	next = current.word_break

	if previous_raw == CR and next == LF {
		Bool.False
	} else if is_newline(previous_raw) or is_newline(next) {
		Bool.True
	} else if previous_raw == ZWJ and current.extended_pictographic {
		Bool.False
	} else if previous_raw == WSegSpace and next == WSegSpace {
		Bool.False
	} else if is_ah_letter(previous) and is_ah_letter(next) {
		Bool.False
	} else if previous == Numeric and next == Numeric {
		Bool.False
	} else if is_ah_letter(previous) and next == Numeric {
		Bool.False
	} else if previous == Numeric and is_ah_letter(next) {
		Bool.False
	} else if previous == Katakana and next == Katakana {
		Bool.False
	} else if is_extend_num_left(previous) and next == ExtendNumLet {
		Bool.False
	} else if previous == ExtendNumLet and is_extend_num_right(next) {
		Bool.False
	} else if previous == Regional_Indicator and next == Regional_Indicator and machine.ri_odd {
		Bool.False
	} else {
		Bool.True
	}
}

can_start_pending = |left, middle| {
	(is_ah_letter(left) and (middle == MidLetter or middle == MidNumLet or middle == Single_Quote))
		or (left == Hebrew_Letter and middle == Double_Quote)
			or (left == Numeric and (middle == MidNum or middle == MidNumLet or middle == Single_Quote))
}

pending_matches = |pending, current| {
	left = pending.left
	middle = pending.middle
	if is_ah_letter(left) and (middle == MidLetter or middle == MidNumLet or middle == Single_Quote) {
		is_ah_letter(current)
	} else if left == Hebrew_Letter and middle == Double_Quote {
		current == Hebrew_Letter
	} else if left == Numeric and (middle == MidNum or middle == MidNumLet or middle == Single_Quote) {
		current == Numeric
	} else {
		Bool.False
	}
}

is_newline = |value| value == CR or value == LF or value == Newline

is_ignored = |value| value == Extend or value == Format or value == ZWJ

is_ah_letter = |value| value == ALetter or value == Hebrew_Letter

is_extend_num_left = |value| is_ah_letter(value) or value == Numeric or value == Katakana or value == ExtendNumLet

is_extend_num_right = |value| is_ah_letter(value) or value == Numeric or value == Katakana
