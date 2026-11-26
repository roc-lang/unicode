## Collect a lazy traversal with atomic output limits. Check the input before
## advancing the iterator and the output before transforming or appending it.
InternalBoundedCollect :: [].{
	Limits : { max_input_bytes : U64, max_items : U64 }
	Error : [LimitExceeded({ resource : [InputBytes, OutputItems], limit : U64, required : U64 })]

	from_iter : U64, Limits, ({} -> Iter(item)), (item -> output) -> Try(List(output), Error)
	from_iter = |input_bytes, limits, make_iter, materialize| {
		if input_bytes > limits.max_input_bytes {
			return Err(LimitExceeded({ resource: InputBytes, limit: limits.max_input_bytes, required: input_bytes }))
		}

		var $next = make_iter({})
		var $output = []
		while Bool.True {
			match Iter.next($next) {
				Done => break
				Skip({ rest }) => {
					$next = rest
				}
				One({ item, rest }) => {
					required = match $output.len().plus_try(1) {
						Ok(count) => count
						Err(Overflow) => return Err(LimitExceeded({ resource: OutputItems, limit: limits.max_items, required: U64.highest }))
					}
					if required > limits.max_items {
						return Err(LimitExceeded({ resource: OutputItems, limit: limits.max_items, required }))
					}

					$output = $output.append(materialize(item))
					$next = rest
				}
			}
		}
		Ok($output)
	}
}
