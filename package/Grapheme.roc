import ByteRange
import InternalGrapheme

## Extended grapheme cluster boundaries and materializers.
##
## The public contract is range-first and implements the default, un-tailored
## Unicode 17 extended grapheme cluster algorithm.
Grapheme :: [].{

	## Incremental segmentation over scalar-aligned `Str` chunks.
	Cursor :: {
		machine : InternalGrapheme.Machine,
		byte_offset : U64,
		finished : Bool,
	}.{
		Error : [AlreadyFinished, OffsetOverflow]

		init : {} -> Cursor
		init = |{}| {
			{
				machine: InternalGrapheme.init({}),
				byte_offset: 0,
				finished: Bool.False,
			}
		}

		## Consume a chunk and fold each irrevocable cluster range into caller
		## state. A chunk boundary is not treated as end of text.
		push : Cursor, Str, state, (state, ByteRange -> state) -> Try({ cursor : Cursor, state : state }, Error)
		push = |cursor, chunk, state, emit| {
			if cursor.finished {
				Err(AlreadyFinished)
			} else {
				next_offset = match cursor.byte_offset.plus_try(chunk.count_utf8_bytes()) {
					Err(Overflow) => return Err(OffsetOverflow)
					Ok(offset) => offset
				}
				folded = InternalGrapheme.fold_chunk(
					cursor.machine,
					cursor.byte_offset,
					chunk,
					state,
					emit,
				)

				Ok({
					cursor: {
						machine: folded.machine,
						byte_offset: next_offset,
						finished: Bool.False,
					},
					state: folded.state,
				})
			}
		}

		## Mark end of text and emit the final nonempty cluster exactly once.
		finish : Cursor, state, (state, ByteRange -> state) -> Try({ cursor : Cursor, state : state }, Error)
		finish = |cursor, state, emit| {
			if cursor.finished {
				Err(AlreadyFinished)
			} else {
				final_state = if cursor.machine.started {
					final_range = ByteRange.from_bounds(
						cursor.machine.cluster_start,
						cursor.byte_offset,
					) ?? ...
					emit(state, final_range)
				} else {
					state
				}

				Ok({
					cursor: {
						machine: cursor.machine,
						byte_offset: cursor.byte_offset,
						finished: Bool.True,
					},
					state: final_state,
				})
			}
		}
	}

	## Visit half-open byte ranges in source order through `Iter`.
	##
	iter_ranges : Str -> Iter(ByteRange)
	iter_ranges = |source| InternalGrapheme.iter_ranges(source)

	## Collect half-open byte ranges in source order.
	ranges : Str -> List(ByteRange)
	ranges = |source| InternalGrapheme.ranges(source)

	## Return seamless slices of the source, retaining its backing storage.
	slices : Str -> List(Str)
	slices = |source| {
		InternalGrapheme.ranges(source).map(
			|range| {
				ByteRange.slice(range, source) ?? ...
			},
		)
	}

	## Return independently materialized cluster strings.
	owned : Str -> List(Str)
	owned = |source| {
		InternalGrapheme.ranges(source).map(
			|range| {
				slice = ByteRange.slice(range, source) ?? ...
				copied_bytes = slice.to_utf8().map(|byte| byte)
				Str.from_utf8(copied_bytes) ?? ...
			},
		)
	}
}
