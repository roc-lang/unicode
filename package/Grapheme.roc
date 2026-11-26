import InternalBoundedCollect
import ByteRange
import InternalGrapheme

## Extended grapheme cluster boundaries and materializers.
##
## The public contract is range-first and implements the default, un-tailored
## Unicode 18 extended grapheme cluster algorithm.
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
	slices = |source| Grapheme.ranges(source).map(|range| source_slice(source, range))

	## Split the source into independently materialized cluster strings.
	split : Str -> List(Str)
	split = |source| Grapheme.ranges(source).map(|range| owned_slice(source, range))

	## Resource limits for the bounded collecting operations in this module.
	CollectionLimits : InternalBoundedCollect.Limits
	CollectionError : InternalBoundedCollect.Error

	## Collect ranges with an input-byte and output-item limit. Fails atomically
	## before exceeding either limit and uses the same traversal as `ranges`.
	ranges_limited : Str, CollectionLimits -> Try(List(ByteRange), CollectionError)
	ranges_limited = |source, limits| {
		InternalBoundedCollect.from_iter(source.count_utf8_bytes(), limits, |{}| Grapheme.iter_ranges(source), |range| range)
	}

	## Collect slices with an input-byte and output-item limit. Fails atomically
	## before exceeding either limit and uses the same traversal as `slices`.
	slices_limited : Str, CollectionLimits -> Try(List(Str), CollectionError)
	slices_limited = |source, limits| {
		InternalBoundedCollect.from_iter(source.count_utf8_bytes(), limits, |{}| Grapheme.iter_ranges(source), |range| source_slice(source, range))
	}

	## Collect split with an input-byte and output-item limit. Fails atomically
	## before exceeding either limit and uses the same traversal as `split`.
	split_limited : Str, CollectionLimits -> Try(List(Str), CollectionError)
	split_limited = |source, limits| {
		InternalBoundedCollect.from_iter(source.count_utf8_bytes(), limits, |{}| Grapheme.iter_ranges(source), |range| owned_slice(source, range))
	}
}

# Both inputs come from the same boundary traversal. Its ranges are ordered
# scalar boundaries within the source. Copying the slice bytes preserves UTF-8.
source_slice = |source, range| ByteRange.slice(range, source) ?? ...

owned_slice = |source, range| {
	slice = source_slice(source, range)
	Str.from_utf8(slice.to_utf8().map(|byte| byte)) ?? ...
}
