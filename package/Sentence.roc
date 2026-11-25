import ByteRange
import InternalSentence

CursorState : {
	machine : InternalSentence.Machine,
	byte_offset : U64,
	status : [Open, Ended, Faulted],
}

## Unicode default UAX #29 C3-1 sentence-boundary ranges.
##
## The default rule can split after abbreviations such as "Mr.".
## It reports boundaries in the source without locale tailoring.
Sentence :: [].{
	Cursor :: { state : CursorState }.{
		Error : [AlreadyFinished, AlreadyFailed, OffsetOverflow({ at : U64 })]

		init : {} -> Cursor
		init = |_| {
			state: {
				machine: InternalSentence.init({}),
				byte_offset: 0,
				status: Open,
			},
		}

		## Push a scalar-aligned chunk. `Pushed` accepts the full chunk; `Failed`
		## is terminal and returns a faulted cursor. Empty chunks are no-ops.
		## The cursor may defer a period boundary while scanning for lowercase
		## right context. It retains one pending coordinate, with no chunk
		## storage or bound on how far ahead the decision can depend.
		push : Cursor,
		Str,
		state,
		(state, ByteRange -> state) -> [
			Pushed({ cursor : Cursor, state : state, consumed : U64 }),
			Failed({ cursor : Cursor, state : state, consumed : U64, error : Error }),
		]
		push = |cursor, chunk, initial, emit| {
			current = cursor.state
			match current.status {
				Ended => return Failed({ cursor, state: initial, consumed: 0, error: AlreadyFinished })
				Faulted => return Failed({ cursor, state: initial, consumed: 0, error: AlreadyFailed })
				Open => {}
			}

			next_offset = match current.byte_offset.plus_try(chunk.count_utf8_bytes()) {
				Err(Overflow) => return Failed({
					cursor: { state: { ..current, status: Faulted } },
					state: initial,
					consumed: 0,
					error: OffsetOverflow({ at: current.byte_offset }),
				})
				Ok(value) => value
			}

			folded = InternalSentence.fold_chunk(current.machine, current.byte_offset, chunk, initial, emit)
			Pushed({
				cursor: { state: { machine: folded.machine, byte_offset: next_offset, status: Open } },
				state: folded.state,
				consumed: chunk.count_utf8_bytes(),
			})
		}

		## Resolve the final pending decision and emit the final nonempty range.
		finish : Cursor,
		state,
		(state, ByteRange -> state) -> [
			End({ cursor : Cursor, state : state }),
			Failed({ cursor : Cursor, state : state, error : Error }),
		]
		finish = |cursor, state, emit| {
			current = cursor.state
			match current.status {
				Ended => Failed({ cursor, state, error: AlreadyFinished })
				Faulted => Failed({ cursor, state, error: AlreadyFailed })
				Open => End({
					cursor: { state: { ..current, status: Ended } },
					state: fold_final(state, InternalSentence.finish(current.machine, current.byte_offset), emit),
				})
			}
		}
	}

	## Lazily visit nonempty default sentence-boundary ranges in source order.
	iter_ranges : Str -> Iter(ByteRange)
	iter_ranges = |source| InternalSentence.iter_ranges(source)

	## Fold default sentence-boundary ranges without materializing them.
	fold_ranges : Str, state, (state, ByteRange -> state) -> state
	fold_ranges = |source, initial, emit| Sentence.iter_ranges(source).fold(initial, emit)

	## Collect default sentence-boundary ranges.
	ranges : Str -> List(ByteRange)
	ranges = |source| Sentence.fold_ranges(source, [], |items, item| items.append(item))

	## Return seamless slices, retaining the source backing allocation.
	slices : Str -> List(Str)
	slices = |source| {
		Sentence.ranges(source).map(|item| ByteRange.slice(item, source) ?? ...)
	}

	## Return independently owned strings for each sentence-boundary range.
	owned : Str -> List(Str)
	owned = |source| {
		Sentence.ranges(source).map(
			|item| {
				slice = ByteRange.slice(item, source) ?? ...
				Str.from_utf8(slice.to_utf8().map(|byte| byte)) ?? ...
			},
		)
	}
}

fold_final = |state, emissions, emit| {
	match emissions {
		NoRanges => state
		Final(item) => emit(state, item)
		PendingAndFinal(first, second) => emit(emit(state, first), second)
	}
}
