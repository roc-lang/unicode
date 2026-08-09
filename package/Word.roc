import ByteRange
import InternalWord

CursorState : {
	machine : InternalWord.Machine,
	byte_offset : U64,
	status : [Open, Ended, Faulted],
}

## Unicode 17 default UAX #29 word-boundary ranges.
##
## These are all ranges between default word boundaries: whitespace and
## punctuation are ordinary ranges, not filtered dictionary words.
Word :: [].{
	Cursor :: { state : CursorState }.{
		Error : [AlreadyFinished, AlreadyFailed, OffsetOverflow({ at : U64 })]

		init : {} -> Cursor
		init = |_| {
			state: {
				machine: InternalWord.init({}),
				byte_offset: 0,
				status: Open,
			},
		}

		## Push a scalar-aligned chunk. `Pushed` accepts the full chunk; `Failed`
		## is terminal and returns a faulted cursor. Empty chunks are no-ops.
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

			folded = InternalWord.fold_chunk(current.machine, current.byte_offset, chunk, initial, emit)
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
					state: fold_final(state, InternalWord.finish(current.machine, current.byte_offset), emit),
				})
			}
		}
	}

	## Lazily visit nonempty default word-boundary ranges in source order.
	iter_ranges : Str -> Iter(ByteRange)
	iter_ranges = |source| InternalWord.iter_ranges(source)

	## Fold default word-boundary ranges without materializing them.
	fold_ranges : Str, state, (state, ByteRange -> state) -> state
	fold_ranges = |source, initial, emit| InternalWord.fold_ranges(source, initial, emit)

	## Collect default word-boundary ranges.
	ranges : Str -> List(ByteRange)
	ranges = |source| InternalWord.ranges(source)

	## Return seamless slices, retaining the source backing allocation.
	slices : Str -> List(Str)
	slices = |source| {
		Word.ranges(source).map(|item| ByteRange.slice(item, source) ?? ...)
	}

	## Return independently owned strings for each word-boundary range.
	owned : Str -> List(Str)
	owned = |source| {
		Word.ranges(source).map(
			|item| {
				slice = ByteRange.slice(item, source) ?? ...
				Str.from_utf8(slice.to_utf8().map(|byte| byte)) ?? ...
			},
		)
	}
}

fold_final = |state, emissions, emit| {
	match emissions {
		NoBoundaries => state
		One(item) => emit(state, item)
		Two(first, second) => emit(emit(state, first), second)
	}
}
