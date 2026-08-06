import InternalLineBreak
import InternalUtf8
import TextPosition

BoundaryRecord : {
	at : TextPosition,
	decision : [Mandatory, Allowed, Prohibited],
	authority : [NonTailorable, Tailorable],
}

OpportunityRecord : {
	at : TextPosition,
	decision : [Mandatory, Allowed],
	authority : [NonTailorable, Tailorable],
}

BoundaryIterState : {
	cursor : InternalUtf8.Cursor,
	machine : InternalLineBreak.Machine,
	emit_start : Bool,
	finished : Bool,
}

OpportunityIterState : {
	cursor : InternalUtf8.Cursor,
	stream : InternalLineBreak.Stream,
	queued : [NoQueued, Queued(OpportunityRecord)],
	finished : Bool,
}

ChunkCursorState : {
	stream : InternalLineBreak.Stream,
	byte_offset : U64,
	scalar_offset : U64,
	status : [Open, Ended, Faulted],
}

## Unicode 17 default line-break opportunities and decisions (UAX #14 rev 55).
##
## This module reports logical boundaries; it does not choose a line width.
## The exact un-tailored Unicode algorithm is the obvious no-configuration
## API. `PreserveGraphemes` is an explicit restriction profile.
LineBreak :: [].{
	Profile : [UnicodeDefault, PreserveGraphemes]
	ProfileRevision : [PreserveGraphemesV1]
	Decision : [Mandatory, Allowed, Prohibited]
	Authority : [NonTailorable, Tailorable]
	BreakBoundary : BoundaryRecord
	BreakOpportunity : OpportunityRecord

	default_profile : Profile
	default_profile = UnicodeDefault

	## Revision of the package-defined tailoring policy, independent of the
	## Unicode/UAX version that defines the default algorithm.
	preserve_graphemes_revision : ProfileRevision
	preserve_graphemes_revision = PreserveGraphemesV1

	profile_revision : Profile -> [None, Some(ProfileRevision)]
	profile_revision = |profile| match profile {
		UnicodeDefault => None
		PreserveGraphemes => Some(PreserveGraphemesV1)
	}

	Cursor :: { state : ChunkCursorState }.{
		Error : [
			AlreadyFinished,
			AlreadyFailed,
			OffsetOverflow({ at : U64 }),
			ScalarOffsetOverflow({ at : U64 }),
		]

		## Begin an exact Unicode-default opportunity stream.
		init : {} -> Cursor
		init = |_| LineBreak.Cursor.init_with(UnicodeDefault)

		## Begin an opportunity stream under an explicit profile.
		init_with : Profile -> Cursor
		init_with = |profile| {
			state: {
				stream: InternalLineBreak.stream_init(preserves_graphemes(profile)),
				byte_offset: 0,
				scalar_offset: 0,
				status: Open,
			},
		}

		## Consume one scalar-aligned `Str` chunk and emit only irrevocable
		## Allowed or Mandatory opportunities in source order.
		##
		## Chunk ends are not end of text. The cursor retains one pending
		## right-context decision and coordinates, never a chunk or substring.
		## Exhaustive Prohibited events intentionally belong to replayable-Str
		## traversal: for `PR OP CM* X`, `PR|OP` can depend on `X`, while every
		## following boundary inside `CM*` is already prohibited, so an ordered
		## non-replayable exhaustive cursor would need an unbounded coordinate
		## queue.
		##
		## `Pushed` accepts the whole chunk. `Failed` seals the cursor and
		## returns the caller state containing all earlier irrevocable
		## opportunities plus the accepted byte count within this chunk. Byte
		## offset overflow is checked before scanning, so that failure has no
		## emissions and consumes zero bytes. A returned cursor never retains
		## the unconsumed chunk suffix.
		push : Cursor,
		Str,
		state,
		(state, BreakOpportunity -> state) -> [
			Pushed({ cursor : Cursor, state : state, consumed : U64 }),
			Failed({ cursor : Cursor, state : state, consumed : U64, error : Error }),
		]
		push = |cursor, chunk, initial_state, emit| {
			current = cursor.state
			match current.status {
				Ended => return Failed({ cursor, state: initial_state, consumed: 0, error: AlreadyFinished })
				Faulted => return Failed({ cursor, state: initial_state, consumed: 0, error: AlreadyFailed })
				Open => {}
			}
			next_byte_offset = match current.byte_offset.plus_try(chunk.count_utf8_bytes()) {
				Err(Overflow) => return Failed({
					cursor: { state: with_chunk_status(current, Faulted) },
					state: initial_state,
					consumed: 0,
					error: OffsetOverflow({ at: current.byte_offset }),
				})
				Ok(value) => value
			}
			folded = InternalUtf8.fold_with_ascii_blocks(
				chunk,
				{
					stream: current.stream,
					state: initial_state,
					byte_offset: current.byte_offset,
					scalar_offset: current.scalar_offset,
					consumed: 0,
					problem: NoProblem,
				},
				|fold, scalar, local_start, local_end, _local_scalar_index| {
					match fold.problem {
						ScalarProblem(_) => fold
						NoProblem => {
							absolute_scalar = fold.scalar_offset
							next_scalar = match fold.scalar_offset.plus_try(1) {
								Err(Overflow) => return {
									stream: fold.stream,
									state: fold.state,
									byte_offset: fold.byte_offset,
									scalar_offset: fold.scalar_offset,
									consumed: fold.consumed,
									problem: ScalarProblem(fold.scalar_offset),
								}
								Ok(value) => value
							}
							# The checked chunk-end addition proves this local
							# byte addition is in range.
							absolute_byte = current.byte_offset + local_start
							transition = InternalLineBreak.stream_push(
								fold.stream,
								scalar,
								TextPosition.from_offsets(absolute_byte, absolute_scalar),
							)
							{
								stream: transition.stream,
								state: fold_emissions(fold.state, transition.emissions, emit),
								byte_offset: current.byte_offset + local_end,
								scalar_offset: next_scalar,
								consumed: local_end,
								problem: NoProblem,
							}
						}
					}
				},
				|fold, vector, local_start, _local_scalar_index| {
					match fold.problem {
						ScalarProblem(_) => fold
						NoProblem => {
							match fold.scalar_offset.plus_try(16) {
								Ok(next_scalar) => {
									transition = InternalLineBreak.fold_ascii_block(
										fold.stream,
										vector,
										current.byte_offset + local_start,
										fold.scalar_offset,
										fold.state,
										emit,
									)
									{
										stream: transition.stream,
										state: transition.state,
										byte_offset: current.byte_offset + local_start + 16,
										scalar_offset: next_scalar,
										consumed: local_start + 16,
										problem: NoProblem,
									}
								}
								Err(Overflow) => fold_ascii_overflow(
									fold,
									vector,
									local_start,
									current.byte_offset,
									emit,
								)
							}
						}
					}
				},
			)
			match folded.problem {
				ScalarProblem(at) => Failed({
					cursor: {
						state: {
							stream: folded.stream,
							byte_offset: folded.byte_offset,
							scalar_offset: folded.scalar_offset,
							status: Faulted,
						},
					},
					state: folded.state,
					consumed: folded.consumed,
					error: ScalarOffsetOverflow({ at: at }),
				})
				NoProblem => Pushed({
					cursor: {
						state: {
							stream: folded.stream,
							byte_offset: next_byte_offset,
							scalar_offset: folded.scalar_offset,
							status: Open,
						},
					},
					state: folded.state,
					consumed: chunk.count_utf8_bytes(),
				})
			}
		}

		## Resolve end-sensitive candidates and emit the mandatory end
		## opportunity exactly once. After `End`, later calls fail with
		## `AlreadyFinished`; after any terminal push failure they fail with
		## `AlreadyFailed`.
		finish : Cursor,
		state,
		(state, BreakOpportunity -> state) -> [
			End({ cursor : Cursor, state : state }),
			Failed({ cursor : Cursor, state : state, error : Error }),
		]
		finish = |cursor, state, emit| {
			current = cursor.state
			match current.status {
				Ended => Failed({ cursor, state, error: AlreadyFinished })
				Faulted => Failed({ cursor, state, error: AlreadyFailed })
				Open => {
					transition = InternalLineBreak.stream_finish(
						current.stream,
						TextPosition.from_offsets(current.byte_offset, current.scalar_offset),
					)
					End({
						cursor: {
							state: {
								stream: transition.stream,
								byte_offset: current.byte_offset,
								scalar_offset: current.scalar_offset,
								status: Ended,
							},
						},
						state: fold_emissions(state, transition.emissions, emit),
					})
				}
			}
		}
	}

	## Lazily report every scalar boundary under the exact Unicode default,
	## including LB2 start and LB3 end.
	##
	## Exhaustive traversal may perform bounded forward lookahead and replay
	## that span from the retained `Str`. Total work is linear, no coordinate
	## list is retained, and early stop decodes no suffix beyond the lookahead
	## semantically required for the last requested boundary.
	iter_boundaries : Str -> Iter(BreakBoundary)
	iter_boundaries = |source| LineBreak.iter_boundaries_with(source, UnicodeDefault)

	## Exhaustive traversal under an explicit profile.
	iter_boundaries_with : Str, Profile -> Iter(BreakBoundary)
	iter_boundaries_with = |source, profile| {
		Iter.custom(
			{
				cursor: InternalUtf8.init(source),
				machine: InternalLineBreak.init(preserves_graphemes(profile)),
				emit_start: Bool.True,
				finished: Bool.False,
			},
			Unknown,
			next_boundary,
		)
	}

	## Lazily report only Allowed and Mandatory boundaries under the exact
	## Unicode default. This path decodes and classifies each scalar once.
	iter_opportunities : Str -> Iter(BreakOpportunity)
	iter_opportunities = |source| LineBreak.iter_opportunities_with(source, UnicodeDefault)

	## Opportunity traversal under an explicit profile.
	iter_opportunities_with : Str, Profile -> Iter(BreakOpportunity)
	iter_opportunities_with = |source, profile| {
		Iter.custom(
			{
				cursor: InternalUtf8.init(source),
				stream: InternalLineBreak.stream_init(preserves_graphemes(profile)),
				queued: NoQueued,
				finished: Bool.False,
			},
			Unknown,
			next_opportunity,
		)
	}

	## Collect every boundary under the exact Unicode default.
	boundaries : Str -> List(BreakBoundary)
	boundaries = |source| LineBreak.iter_boundaries(source).fold([], |items, item| items.append(item))

	## Collect every boundary under an explicit profile.
	boundaries_with : Str, Profile -> List(BreakBoundary)
	boundaries_with = |source, profile| {
		LineBreak.iter_boundaries_with(source, profile).fold([], |items, item| items.append(item))
	}

	## Collect Allowed and Mandatory opportunities under the exact default.
	opportunities : Str -> List(BreakOpportunity)
	opportunities = |source| LineBreak.iter_opportunities(source).fold([], |items, item| items.append(item))

	## Collect opportunities under an explicit profile.
	opportunities_with : Str, Profile -> List(BreakOpportunity)
	opportunities_with = |source, profile| {
		LineBreak.iter_opportunities_with(source, profile).fold([], |items, item| items.append(item))
	}
}

FoldProblem : [NoProblem, ScalarProblem(U64)]

with_chunk_status : ChunkCursorState, [Open, Ended, Faulted] -> ChunkCursorState
with_chunk_status = |state, status| {
	{
		stream: state.stream,
		byte_offset: state.byte_offset,
		scalar_offset: state.scalar_offset,
		status,
	}
}

preserves_graphemes : LineBreak.Profile -> Bool
preserves_graphemes = |profile| profile == PreserveGraphemes

next_boundary : BoundaryIterState -> Try((BoundaryRecord, BoundaryIterState), [NoMore])
next_boundary = |state| {
	if state.finished {
		return Err(NoMore)
	}
	if state.emit_start {
		return Ok((
			{
				at: TextPosition.from_offsets(0, 0),
				decision: Prohibited,
				authority: NonTailorable,
			},
			{
				cursor: state.cursor,
				machine: state.machine,
				emit_start: Bool.False,
				finished: state.finished,
			},
		))
	}

	var cursor = state.cursor
	var machine = state.machine
	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => {
				event = {
					at: TextPosition.from_offsets(cursor.byte_offset, cursor.scalar_index),
					decision: Mandatory,
					authority: NonTailorable,
				}
				return Ok((
					event,
					{
						cursor,
						machine,
						emit_start: Bool.False,
						finished: Bool.True,
					},
				))
			}
			One({ item, rest }) => {
				prepared = InternalLineBreak.prepare(machine, item.scalar, item.byte_start)
				advanced = InternalLineBreak.advance(machine, prepared)
				cursor = rest
				if !machine.started {
					machine = advanced
				} else {
					outcome = match InternalLineBreak.classify(machine, prepared) {
						Resolved(value) => value
						NeedsFirst(need) => resolve_with_lookahead(machine, prepared, need, rest)
					}
					return Ok((
						{
							at: TextPosition.from_offsets(item.byte_start, item.scalar_index),
							decision: outcome.decision,
							authority: outcome.authority,
						},
						{
							cursor,
							machine: advanced,
							emit_start: Bool.False,
							finished: Bool.False,
						},
					))
				}
			}
		}
	}
	Err(NoMore)
}

resolve_with_lookahead : InternalLineBreak.Machine, InternalLineBreak.Prepared, InternalLineBreak.FirstNeed, InternalUtf8.Cursor -> InternalLineBreak.Outcome
resolve_with_lookahead = |machine, prepared, need, cursor| {
	match next_significant(cursor, prepared.token) {
		NoSignificant => InternalLineBreak.resolve_end(machine, prepared, need)
		SignificantAhead({ token: first, rest }) => {
			match InternalLineBreak.resolve_first(machine, prepared, need, first) {
				Resolved(outcome) => outcome
				NeedsSecond(second_need) => match next_significant(rest, first) {
					NoSignificant => InternalLineBreak.resolve_second(
						machine,
						prepared,
						second_need,
						EndAhead,
					)
					SignificantAhead({ token: second, rest: _ }) => InternalLineBreak.resolve_second(
						machine,
						prepared,
						second_need,
						TokenAhead(second),
					)
				}
			}
		}
	}
}

SignificantAhead : [NoSignificant, SignificantAhead({ token : InternalLineBreak.Token, rest : InternalUtf8.Cursor })]

next_significant : InternalUtf8.Cursor, InternalLineBreak.Token -> SignificantAhead
next_significant = |initial, preceding| {
	var cursor = initial
	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => return NoSignificant
			One({ item, rest }) => {
				cursor = rest
				match InternalLineBreak.token_for_lookahead(preceding, item.scalar) {
					Attached => {}
					Significant(token) => return SignificantAhead({ token, rest })
				}
			}
		}
	}
	NoSignificant
}

next_opportunity : OpportunityIterState -> Try((OpportunityRecord, OpportunityIterState), [NoMore])
next_opportunity = |state| {
	match state.queued {
		Queued(event) => return Ok((
			event,
			{
				cursor: state.cursor,
				stream: state.stream,
				queued: NoQueued,
				finished: state.finished,
			},
		))
		NoQueued => {}
	}
	if state.finished {
		return Err(NoMore)
	}

	var cursor = state.cursor
	var stream = state.stream
	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => {
				transition = InternalLineBreak.stream_finish(
					stream,
					TextPosition.from_offsets(cursor.byte_offset, cursor.scalar_index),
				)
				return opportunity_from_emissions(
					transition.emissions,
					{
						cursor,
						stream: transition.stream,
						queued: NoQueued,
						finished: Bool.True,
					},
				)
			}
			One({ item, rest }) => {
				transition = InternalLineBreak.stream_push(
					stream,
					item.scalar,
					TextPosition.from_offsets(item.byte_start, item.scalar_index),
				)
				cursor = rest
				stream = transition.stream
				match transition.emissions {
					NoEvents => {}
					_ => return opportunity_from_emissions(
						transition.emissions,
						{
							cursor,
							stream,
							queued: NoQueued,
							finished: Bool.False,
						},
					)
				}
			}
		}
	}
	Err(NoMore)
}

opportunity_from_emissions : InternalLineBreak.Emissions, OpportunityIterState -> Try((OpportunityRecord, OpportunityIterState), [NoMore])
opportunity_from_emissions = |emissions, state| {
	match emissions {
		NoEvents => Err(NoMore)
		OneEvent(event) => Ok((event, state))
		TwoEvents({ first, second }) => Ok((
			first,
			{
				cursor: state.cursor,
				stream: state.stream,
				queued: Queued(second),
				finished: state.finished,
			},
		))
	}
}

fold_emissions : state, InternalLineBreak.Emissions, (state, OpportunityRecord -> state) -> state
fold_emissions = |initial, emissions, emit| {
	match emissions {
		NoEvents => initial
		OneEvent(event) => emit(initial, event)
		TwoEvents({ first, second }) => emit(emit(initial, first), second)
	}
}

fold_ascii_overflow = |initial, vector, local_start, absolute_byte_base, emit| {
	var fold = initial
	var lane = 0.U64
	while lane < 16 {
		match fold.problem {
			ScalarProblem(_) => {}
			NoProblem => match fold.scalar_offset.plus_try(1) {
				Err(Overflow) => {
					fold = {
						stream: fold.stream,
						state: fold.state,
						byte_offset: fold.byte_offset,
						scalar_offset: fold.scalar_offset,
						consumed: fold.consumed,
						problem: ScalarProblem(fold.scalar_offset),
					}
				}
				Ok(next_scalar) => {
					transition = InternalLineBreak.stream_push(
						fold.stream,
						vector.get_lane(lane).to_u32(),
						TextPosition.from_offsets(
							absolute_byte_base + local_start + lane,
							fold.scalar_offset,
						),
					)
					fold = {
						stream: transition.stream,
						state: fold_emissions(fold.state, transition.emissions, emit),
						byte_offset: absolute_byte_base + local_start + lane + 1,
						scalar_offset: next_scalar,
						consumed: local_start + lane + 1,
						problem: NoProblem,
					}
				}
			}
		}
		lane = lane + 1
	}
	fold
}
