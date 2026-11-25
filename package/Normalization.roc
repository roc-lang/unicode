import Case
import InternalCanonicalCombiningClass
import InternalNormalizationData
import InternalUtf8
import Scalar

# Hangul constants from the Unicode Core Specification, §3.12.
s_base = 0xAC00.U32

l_base = 0x1100.U32

v_base = 0x1161.U32

t_base = 0x11A7.U32

t_count = 28.U32

v_count = 21.U32

n_count = 588.U32

s_count = 11172.U32

Member : { code : U32, class : U8 }

Map : [Identity, Mapped(U64)]

## Default Unicode normalization forms and identifier case folding.
Normalization :: [].{
	Form : [NFC, NFD, NFKC, NFKD]
	QuickCheck : [Yes, No, Maybe]
	Resource : [InputBytes, IntermediateScalars, OutputBytes]
	Limits : { max_input_bytes : U64, max_intermediate_scalars : U64, max_output_bytes : U64 }
	Error : [LimitExceeded({ resource : Resource, limit : U64, required : U64 }), InternalFault]

	## UAX #15 per-form quick check with canonical order verification.
	## The scan is total, stops at No, and retains constant state.
	quick_check : Str, Form -> QuickCheck
	quick_check = |source, form| {
		shift = match form {
			NFC => 0.U8
			NFD => 2
			NFKC => 4
			NFKD => 6
		}
		var $cursor = InternalUtf8.init(source)
		var $last_class = 0.U8
		var $result = Yes
		while Bool.True {
			match InternalUtf8.next($cursor) {
				Done => break
				One({ item, rest }) => {
					code = item.scalar
					class = InternalCanonicalCombiningClass.lookup(code)
					if class != 0 and class < $last_class {
						return No
					}
					flags = match InternalNormalizationData.quick_check(code.to_u64()) {
						Some(value) => value.shr_wrap(shift).bitwise_and(3)
						None => 0
					}
					if flags == 1 {
						return No
					}
					if flags == 2 {
						$result = Maybe
					}
					$last_class = class
					$cursor = rest
				}
			}
		}
		$result
	}

	## Normalize a complete string. Every buffer is bounded by the supplied
	## limits. max_intermediate_scalars caps the logical decomposed tape.
	## Reordering and composition may use another buffer of that size.
	## The result owns its text and keeps no source reference.
	normalize : Str, Form, Limits -> Try(Str, Error)
	normalize = |source, form, limits| {
		compatibility = form == NFKC or form == NFKD
		compose = form == NFC or form == NFKC
		run(source, limits, compatibility, compose, Bool.False)
	}

	## Decide exact normalization. The quick check settles Yes and No
	## directly. Maybe requires the same normalization engine as normalize.
	is_normalized : Str, Form, Limits -> Try(Bool, Error)
	is_normalized = |source, form, limits| {
		check_input(source, limits)?
		match Normalization.quick_check(source, form) {
			Yes => Ok(Bool.True)
			No => Ok(Bool.False)
			Maybe => Normalization.normalize(source, form, limits).map_ok(|normalized| normalized == source)
		}
	}

	## Compare two complete strings for canonical equivalence by NFD.
	## Each argument has an independent application of the supplied limits.
	canonically_equivalent : Str, Str, Limits -> Try(Bool, Error)
	canonically_equivalent = |left, right, limits| {
		a = Normalization.normalize(left, NFD, limits)?
		b = Normalization.normalize(right, NFD, limits)?
		Ok(a == b)
	}

	## Unicode toNFKC_Casefold. Apply NFKC_CF to each input scalar (including
	## the explicit deletions), then normalize the result to NFC.
	nfkc_case_fold : Str, Limits -> Try(Str, Error)
	nfkc_case_fold = |source, limits| run(source, limits, Bool.False, Bool.True, Bool.True)

	## Unicode canonical caseless matching, NFD(toCasefold(NFD(text))).
	## Case limits independently bound the full fold and its mapping facts.
	canonically_case_insensitive_equal : Str, Str, Limits, Case.Limits -> Try(Bool, [NormalizeFailed(Error), FoldFailed(Case.Error)])
	canonically_case_insensitive_equal = |left, right, limits, fold_limits| {
		a = canonical_fold(left, limits, fold_limits)?
		b = canonical_fold(right, limits, fold_limits)?
		Ok(a == b)
	}
}

canonical_fold = |source, limits, fold_limits| {
	decomposed = Normalization.normalize(source, NFD, limits).map_err(|error| NormalizeFailed(error))?
	folded = Case.fold(decomposed, Case.full, fold_limits).map_err(|error| FoldFailed(error))?
	Normalization.normalize(Case.result_text(folded), NFD, limits).map_err(|error| NormalizeFailed(error))
}

check_input = |source, limits| {
	required = source.count_utf8_bytes()
	if required > limits.max_input_bytes {
		Err(LimitExceeded({ resource: InputBytes, limit: limits.max_input_bytes, required }))
	} else {
		Ok({})
	}
}

run = |source, limits, compatibility, compose, identifier_fold| {
	check_input(source, limits)?
	var $cursor = InternalUtf8.init(source)
	var $members = []
	while Bool.True {
		match InternalUtf8.next($cursor) {
			Done => break
			One({ item, rest }) => {
				code = item.scalar
				mapping = if identifier_fold {
					match InternalNormalizationData.fold(code.to_u64()) {
						Some(token) => Mapped(token)
						None => Identity
					}
				} else {
					Identity
				}
				match mapping {
					Identity => {
						$members = decompose($members, code, compatibility, limits.max_intermediate_scalars)?
					}
					Mapped(token) => {
						span = mapping_span(token)
						var $at = span.start
						while $at < span.end {
							mapped = InternalNormalizationData.mapping_scalar($at).map_err(|_| InternalFault)?
							$members = decompose($members, mapped, Bool.False, limits.max_intermediate_scalars)?
							$at = $at + 1
						}
					}
				}
				$cursor = rest
			}
		}
	}
	ordered = canonical_order($members)?
	result = if compose {
		compose_all(ordered)?
	} else {
		ordered
	}
	encode(result, limits.max_output_bytes)
}

mapping_span = |token| {
	start = token.shr_wrap(8)
	{ start, end: start + token.bitwise_and(255) }
}

decompose = |initial, code, compatibility, limit| {
	if code >= s_base and code < s_base + s_count {
		index = code - s_base
		l = l_base + index // n_count
		v = v_base + (index % n_count) // t_count
		t = index % t_count
		members = append(append(initial, l, limit)?, v, limit)?
		return if t == 0 {
			Ok(members)
		} else {
			append(members, t_base + t, limit)
		}
	}

	compatible = if compatibility {
		InternalNormalizationData.compatibility(code.to_u64())
	} else {
		None
	}
	token = match compatible {
		Some(value) => value
		None => match InternalNormalizationData.canonical(code.to_u64()) {
			Some(value) => value
			None => return append(initial, code, limit)
		}
	}
	span = mapping_span(token)
	var $members = initial
	var $at = span.start
	while $at < span.end {
		code_at = InternalNormalizationData.mapping_scalar($at).map_err(|_| InternalFault)?
		$members = append($members, code_at, limit)?
		$at = $at + 1
	}
	Ok($members)
}

append : List(Member), U32, U64 -> Try(List(Member), Normalization.Error)
append = |members, code, limit| {
	required = members.len().plus_try(1) ?? return Err(LimitExceeded({ resource: IntermediateScalars, limit, required: U64.highest }))
	if required > limit {
		return Err(LimitExceeded({ resource: IntermediateScalars, limit, required }))
	}
	Ok(members.append({ code, class: InternalCanonicalCombiningClass.lookup(code) }))
}

# Canonical order consists of stable nonstarter blocks delimited by CCC 0.
# Scan each block once and use a stable counting pass only when disordered.
canonical_order : List(Member) -> Try(List(Member), Normalization.Error)
canonical_order = |initial| {
	var $members = initial
	var $start = 0.U64
	var $last = 0.U8
	var $disordered = Bool.False
	var $at = 0.U64
	while $at < $members.len() {
		item = $members.get($at).map_err(|_| InternalFault)?
		if item.class == 0 {
			if $disordered {
				$members = order_block($members, $start, $at)?
			}
			$start = $at + 1
			$disordered = Bool.False
		} else if item.class < $last {
			$disordered = Bool.True
		}
		$last = item.class
		$at = $at + 1
	}
	if $disordered {
		order_block($members, $start, $at)
	} else {
		Ok($members)
	}
}

order_block : List(Member), U64, U64 -> Try(List(Member), Normalization.Error)
order_block = |initial, start, end| {
	var $counts = List.repeat(0.U64, 256)
	var $at = start
	while $at < end {
		item = initial.get($at).map_err(|_| InternalFault)?
		class = item.class.to_u64()
		count = $counts.get(class).map_err(|_| InternalFault)?
		$counts = $counts.set(class, count + 1).map_err(|_| InternalFault)?
		$at = $at + 1
	}

	var $class = 0.U64
	var $offset = 0.U64
	while $class < 256 {
		count = $counts.get($class).map_err(|_| InternalFault)?
		$counts = $counts.set($class, $offset).map_err(|_| InternalFault)?
		$offset = $offset + count
		$class = $class + 1
	}

	var $ordered = List.repeat({ code: 0.U32, class: 0.U8 }, end - start)
	$at = start
	while $at < end {
		item = initial.get($at).map_err(|_| InternalFault)?
		class = item.class.to_u64()
		out = $counts.get(class).map_err(|_| InternalFault)?
		$ordered = $ordered.set(out, item).map_err(|_| InternalFault)?
		$counts = $counts.set(class, out + 1).map_err(|_| InternalFault)?
		$at = $at + 1
	}

	var $members = initial
	$at = start
	for item in $ordered {
		$members = $members.set($at, item).map_err(|_| InternalFault)?
		$at = $at + 1
	}
	Ok($members)
}

compose_all : List(Member) -> Try(List(Member), Normalization.Error)
compose_all = |members| {
	var $output = []
	var $starter = None
	var $last_class = 0.U8
	for item in members {
		composite = match $starter {
			Some(starter) if $last_class == 0 or $last_class < item.class => {
				match composite_of(starter.code, item.code) {
					Some(code) => Some({ index: starter.index, code })
					None => None
				}
			}
			_ => None
		}
		match composite {
			Some(value) => {
				$output = $output.set(value.index, { code: value.code, class: 0 }).map_err(|_| InternalFault)?
				$starter = Some(value)
			}
			None => {
				if item.class == 0 {
					$starter = Some({ index: $output.len(), code: item.code })
				}
				$last_class = item.class
				$output = $output.append(item)
			}
		}
	}
	Ok($output)
}

composite_of = |first, second| {
	if first >= l_base and first < l_base + 19 and second >= v_base and second < v_base + v_count {
		Some(s_base + (first - l_base) * n_count + (second - v_base) * t_count)
	} else if first >= s_base and first < s_base + s_count and (first - s_base) % t_count == 0 and second > t_base and second < t_base + t_count {
		Some(first + second - t_base)
	} else {
		key = first.to_u64().shl_wrap(21).bitwise_or(second.to_u64())
		match InternalNormalizationData.composition(key) {
			Some(value) => Some(value.to_u32_wrap())
			None => None
		}
	}
}

encode : List(Member), U64 -> Try(Str, Normalization.Error)
encode = |members, limit| {
	var $bytes = []
	for item in members {
		scalar = Scalar.from_u32(item.code).map_err(|_| InternalFault)?
		$bytes = Scalar.append_utf8($bytes, scalar, limit).map_err(|OutputLimitExceeded({ limit: byte_limit, required })|
			LimitExceeded({ resource: OutputBytes, limit: byte_limit, required }))?
	}
	Str.from_utf8($bytes).map_err(|_| InternalFault)
}
