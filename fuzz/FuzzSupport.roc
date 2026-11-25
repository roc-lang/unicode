import fuzz.Fuzz
import unicode.Scalar

FuzzSupport := [].{

	## A bounded test plan. Each scalar carries a chunk action preceding it.
	Plan : { code_points : List(U32), actions : List(U8), config : U8 }

	plan : Fuzz.Generator(Plan)
	plan = Fuzz.map(Fuzz.raw_bytes, plan_from_bytes)

	## First byte selects the policy, limit or line coordinates. Groups of three
	## bytes use their low 21 bits for scalar rank and high three bits for
	## chunk actions. An incomplete final group is zero-padded.
	plan_from_bytes : List(U8) -> Plan
	plan_from_bytes = |bytes| {
		config = match bytes.get(0) {
			Ok(first) => first
			Err(_) => 0
		}
		var $remaining = bytes.drop_first(1).take_first(384)
		var $scalars = []
		var $actions = []
		while !$remaining.is_empty() {
			(b0, b1, b2, rest) = match $remaining {
				[first, second, third, .. as tail] => (first, second, third, tail)
				[first, second] => (first, second, 0, [])
				[first] => (first, 0, 0, [])
				[] => (0, 0, 0, [])
			}
			raw = b0.to_u32().bitwise_or(b1.to_u32().shl_wrap(8)).bitwise_or(b2.bitwise_and(31).to_u32().shl_wrap(16))
			rank = raw % 0x10F800
			$scalars = $scalars.append(if (rank <= 0xD7FF) rank else rank + 0x800)
			$actions = $actions.append(b2.shr_wrap(5))
			$remaining = rest
		}
		{ code_points: $scalars, actions: $actions, config }
	}

	## Bit 0 flushes a chunk before this scalar. Bit 1 inserts an empty chunk.
	## Bit 2 flushes after this scalar. The first and last chunks may be empty.
	chunks : Plan, List(Str) -> List(Str)
	chunks = |input, parts| {
		var $chunks = []
		var $pending = ""
		var $index = 0
		for part in parts {
			action = input.actions.get($index) ?? crash "missing fuzz chunk action"
			if action.bitwise_and(1) != 0 {
				$chunks = $chunks.append($pending)
				$pending = ""
			}
			if action.bitwise_and(2) != 0 {
				$chunks = $chunks.append("")
			}
			$pending = Str.concat($pending, part)
			if action.bitwise_and(4) != 0 {
				$chunks = $chunks.append($pending)
				$pending = ""
			}
			$index = $index + 1
		}
		$chunks.append($pending)
	}

	## Stop after a generated number of yielded items and resume using only the
	## returned iterator, as a caller with no access to private traversal state.
	pause_iter : Iter(item), U64 -> List(item)
	pause_iter = |iter, stop_after| {
		var $iter = iter
		var $prefix = []
		while $prefix.len() < stop_after {
			match $iter.next() {
				Done => return $prefix
				Skip({ rest }) => {
					$iter = rest
				}
				One({ item, rest }) => {
					$iter = rest
					$prefix = $prefix.append(item)
				}
			}
		}
		$iter.fold($prefix, |items, item| items.append(item))
	}

	CollectorLimits : { max_input_bytes : U64, max_items : U64 }
	CollectorError : [LimitExceeded({ resource : [InputBytes, OutputItems], limit : U64, required : U64 })]

	## Compare public limited collectors with the independently collected result.
	## Probe exact capacities, varied smaller capacities, and input precedence.
	check_bounded_collection : Str, Str, U8, List(item), (CollectorLimits -> Try(List(item), CollectorError)) -> {}
		where [item.is_eq : item, item -> Bool]
	check_bounded_collection = |name, input_text, config, expected, collect| {
		bytes = input_text.count_utf8_bytes()
		count = expected.len()
		if collect({ max_input_bytes: bytes, max_items: count }) != Ok(expected) {
			crash "${name} changed its output at the exact byte and item limits"
		}
		if bytes > 0 {
			byte_limit = config.to_u64() % bytes
			if collect({ max_input_bytes: byte_limit, max_items: 0 }) != Err(LimitExceeded({ resource: InputBytes, limit: byte_limit, required: bytes })) {
				crash "${name} did not report the input-byte limit before collection"
			}
		}
		if count > 0 {
			item_limit = config.to_u64() % count
			if collect({ max_input_bytes: bytes, max_items: item_limit }) != Err(LimitExceeded({ resource: OutputItems, limit: item_limit, required: item_limit + 1 })) {
				crash "${name} did not report the first item beyond the output limit"
			}
		}
		{}
	}

	show_plan : Plan -> Str
	show_plan = |input| {
		chunk_plan = Str.inspect(input.actions)
		"config=${input.config.to_str()} actions=${chunk_plan} ${show_scalars(input.code_points)}"
	}

	source_parts : List(U32) -> List(Str)
	source_parts = |code_points| {
		code_points.map(
			|code_point| {
				scalar = Scalar.from_u32(code_point) ?? crash "fuzz scalar encoding admitted an invalid scalar"
				Scalar.to_str(scalar) ?? crash "fuzz scalar encoding could not produce UTF-8"
			},
		)
	}

	source : List(U32) -> Str
	source = |code_points| Str.join_with(source_parts(code_points), "")

	show_scalars : List(U32) -> Str
	show_scalars = |code_points| {
		labels = code_points.map(|code_point| "U+${hex_scalar(code_point)}")
		"code_points=[${Str.join_with(labels, ", ")}] text=${Str.inspect(source(code_points))}"
	}

	show_bytes : List(U8) -> Str
	show_bytes = |bytes| {
		hex = bytes.map(|byte| pad_hex(byte.to_u32(), 2))
		"bytes=${bytes.len().to_str()} hex=${Str.join_with(hex, " ")}"
	}

	hex_scalar : U32 -> Str
	hex_scalar = |value| pad_hex(value, 4)

	pad_hex : U32, U64 -> Str
	pad_hex = |value, minimum| {
		var $remaining = value
		var $text = ""
		while $remaining > 0 {
			digit = hex_digit($remaining % 16)
			$text = "${digit}${$text}"
			$remaining = $remaining / 16
		}
		if $text == "" {
			$text = "0"
		}
		while $text.count_utf8_bytes() < minimum {
			$text = "0${$text}"
		}
		$text
	}

	hex_digit : U32 -> Str
	hex_digit = |value| match value {
		0 => "0"
		1 => "1"
		2 => "2"
		3 => "3"
		4 => "4"
		5 => "5"
		6 => "6"
		7 => "7"
		8 => "8"
		9 => "9"
		10 => "A"
		11 => "B"
		12 => "C"
		13 => "D"
		14 => "E"
		15 => "F"
		_ => "?"
	}
}
