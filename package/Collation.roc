import InternalCanonicalCombiningClass
import InternalCollationData
import InternalGeneralCategory
import InternalUtf8
import Locale
import Normalization
import Numeric
import Scalar

Element : { primary : U32, secondary : U32, tertiary : U32, variable : Bool, case_kind : U8 }

Unit : { scalar : U32, class : U8, removed : Bool }

Trie : { root : U32, overlay : U32 }

Match : { state : Trie, terminal : U64, next : U64 }

Levels : { primary : U32, secondary : U32, tertiary : U32, quaternary : U32 }

## Bounded Unicode collation with explicit DUCET and CLDR-derived profiles.
Collation :: [].{
	Strength : [Primary, Secondary, Tertiary, Quaternary, Identical]
	Alternate : [NonIgnorable, Shifted]
	CaseFirst : [Default, UpperFirst, LowerFirst]
	Options : { strength : Strength, alternate : Alternate, numeric : Bool, case_first : CaseFirst }
	default_options : Options
	default_options = { strength: Tertiary, alternate: NonIgnorable, numeric: Bool.False, case_first: Default }

	Profile : [Ducet, GermanStandard, GermanPhonebook, SpanishStandard, SpanishTraditional]
	ProfileRevision : [DucetV1, CldrRulesV1]
	Collator :: { profile : Profile, options : Options }

	SelectionError : [UnsupportedLocale(Str)]
	Resource : [Elements, NumericDigits, KeyBytes, SortItems, SortKeyBytes]
	Limits : {
		normalization : Normalization.Limits,
		max_elements : U64,
		max_numeric_digits : U64,
		max_key_bytes : U64,
	}
	SortLimits : { per_text : Limits, max_items : U64, max_total_key_bytes : U64 }
	Error : [NormalizationFailed(Normalization.Error), LimitExceeded({ resource : Resource, limit : U64, required : U64 }), InternalFault]

	## Select one supported locale identifier. Unsupported subtags or collation
	## types produce a typed error. Settings enter through Options.
	collator : Locale, Options -> Try(Collator, SelectionError)
	collator = |locale, options| {
		identifier = Locale.to_str(locale)
		selected = match identifier {
			"und" | "und-u-co-standard" => Ducet
			"de" | "de-u-co-standard" => GermanStandard
			"de-u-co-phonebk" => GermanPhonebook
			"es" | "es-u-co-standard" => SpanishStandard
			"es-u-co-trad" => SpanishTraditional
			_ => return Err(UnsupportedLocale(identifier))
		}
		Ok({ profile: selected, options })
	}

	profile : Collator -> Profile
	profile = |value| value.profile

	profile_revision : Collator -> ProfileRevision
	profile_revision = |value| if (value.profile == Ducet) DucetV1 else CldrRulesV1

	## Version of the independent locale catalog for a selected CLDR profile.
	## DUCET uses only UnicodeVersion.current.
	cldr_version : Collator -> [None, Some(Str)]
	cldr_version = |value| {
		if value.profile == Ducet {
			None
		} else {
			Some(InternalCollationData.cldr_version)
		}
	}

	## Sort keys compare bytewise under identical settings and data pins.
	## They are opaque to callers and their encoding can change on upgrade.
	sort_key : Collator, Str, Limits -> Try(List(U8), Error)
	sort_key = |value, source, limits| {
		normalized = Normalization.normalize(source, NFD, limits.normalization).map_err(|issue| NormalizationFailed(issue))?
		units = read_units(normalized)
		elements = collation_elements(units, roots(value.profile), value.profile != Ducet, value.options, limits)?
		form_key(elements, normalized, value.options, limits.max_key_bytes)
	}

	## Test relative collation order under one collator and the same per-text
	## limits. Collation equality can occur for different source bytes.
	compare : Collator, Str, Str, Limits -> Try([Lt, Eq, Gt], Error)
	compare = |value, left, right, limits| {
		left_key = Collation.sort_key(value, left, limits)?
		right_key = Collation.sort_key(value, right, limits)?
		Ok(compare_bytes(left_key, right_key))
	}

	## Stable sort. Compute each key once, preserving the original order
	## for collation-equal entries. Fail atomically when any limit is reached.
	sort : Collator, List(Str), SortLimits -> Try(List(Str), Error)
	sort = |value, strings, limits| {
		if strings.len() > limits.max_items {
			return Err(limit_error(SortItems, limits.max_items, strings.len()))
		}
		var $keys = []
		var $total = 0.U64
		var $index = 0.U64
		for source in strings {
			remaining = limits.max_total_key_bytes - $total
			key_limit = if remaining < limits.per_text.max_key_bytes {
				remaining
			} else {
				limits.per_text.max_key_bytes
			}
			key = match Collation.sort_key(value, source, { ..limits.per_text, max_key_bytes: key_limit }) {
				Ok(bytes) => bytes
				Err(LimitExceeded({ resource: KeyBytes, required, .. })) if remaining < limits.per_text.max_key_bytes =>
					return Err(limit_error(SortKeyBytes, limits.max_total_key_bytes, checked_sum($total, required)))
				Err(error) => return Err(error)
			}
			$keys = $keys.append({ key, source, index: $index })
			$total = $total + key.len()
			$index = $index + 1
		}
		sorted = $keys.sort_with(
			|left, right| match compare_bytes(left.key, right.key) {
				Lt => Before
				Gt => After
				Eq => if (left.index < right.index) Before else if (left.index > right.index) After else Same
			},
		)
		Ok(sorted.map(|item| item.source))
	}
}

roots : Collation.Profile -> Trie
roots = |profile| {
	overlay = match profile {
		Ducet | GermanStandard => 0
		SpanishStandard => 2
		SpanishTraditional => 3
		GermanPhonebook => 4
	}
	{ root: 1, overlay }
}

read_units : Str -> List(Unit)
read_units = |source| {
	InternalUtf8.fold_scalars(
		source,
		[],
		|units, scalar, _start, _end, _index| {
			units.append({ scalar, class: InternalCanonicalCombiningClass.lookup(scalar), removed: Bool.False })
		},
	)
}

step : Trie, U32 -> Trie
step = |state, scalar| {
	{
		root: InternalCollationData.child(state.root, scalar),
		overlay: InternalCollationData.child(state.overlay, scalar),
	}
}

mapping : Trie -> U64
mapping = |state| {
	overlay = InternalCollationData.terminal(state.overlay)
	if overlay != 0 {
		overlay
	} else {
		InternalCollationData.terminal(state.root)
	}
}

# S2.1 follows partial prefixes even when neither trie maps that prefix.
longest_contiguous : List(Unit), U64, Trie -> Try(Match, Collation.Error)
longest_contiguous = |units, start, root| {
	var $best = { state: root, terminal: 0, next: start + 1 }
	var $state = root
	var $at = start
	while $at < units.len() {
		unit = units.get($at) ?? return Err(InternalFault)
		$at = $at + 1
		if !unit.removed {
			$state = step($state, unit.scalar)
			if $state.root == 0 and $state.overlay == 0 {
				break
			}
			terminal = mapping($state)
			if terminal != 0 {
				$best = { state: $state, terminal, next: $at }
			}
		}
	}
	Ok($best)
}

# S2.1.1–3 extend an existing mapping by one unblocked non-starter.
# Remove only matched marks, keeping the unmatched marks in source order.
discontiguous : List(Unit), Match -> Try({ units : List(Unit), terminal : U64 }, Collation.Error)
discontiguous = |initial, best| {
	var $units = initial
	var $at = best.next
	var $state = best.state
	var $terminal = best.terminal
	var $skipped_class = 0.U8
	while $at < $units.len() {
		unit = $units.get($at) ?? return Err(InternalFault)
		if !unit.removed {
			if unit.class == 0 or unit.class <= $skipped_class {
				break
			}
			next = step($state, unit.scalar)
			terminal = mapping(next)
			if terminal == 0 {
				$skipped_class = unit.class
			} else {
				$units = $units.set($at, { ..unit, removed: Bool.True }) ?? return Err(InternalFault)
				$state = next
				$terminal = terminal
			}
		}
		$at = $at + 1
	}
	Ok({ units: $units, terminal: $terminal })
}

collation_elements : List(Unit), Trie, Bool, Collation.Options, Collation.Limits -> Try(List(Element), Collation.Error)
collation_elements = |initial, root, cldr_variables, options, limits| {
	var $units = initial
	var $out = []
	var $at = 0.U64
	while $at < $units.len() {
		unit = $units.get($at) ?? return Err(InternalFault)
		if unit.removed {
			$at = $at + 1
		} else if options.numeric and decimal_digit(unit.scalar) != None {
			run = read_digits($units, $at, limits.max_numeric_digits)?
			$out = numeric_elements($out, run.digits, limits.max_elements)?
			$at = run.next
		} else {
			best = longest_contiguous($units, $at, root)?
			if best.terminal == 0 {
				$out = append_implicit($out, unit.scalar, limits.max_elements)?
			} else {
				matched = discontiguous($units, best)?
				$units = matched.units
				$out = append_mapped($out, matched.terminal, cldr_variables, limits.max_elements)?
			}
			$at = best.next
		}
	}
	Ok($out)
}

append_mapped : List(Element), U64, Bool, U64 -> Try(List(Element), Collation.Error)
append_mapped = |initial, terminal, cldr_variables, limit| {
	count = terminal.bitwise_and(255)
	# The packed mapping bounds start by 56 bits and count by 8 bits.
	start = terminal.shr_wrap(8)
	if count > limit - initial.len() {
		return Err(limit_error(Elements, limit, checked_sum(initial.len(), count)))
	}
	variable_bit = if cldr_variables {
		0x20000000000000
	} else {
		0x4000000000000
	}
	var $output = initial
	var $index = 0.U64
	while $index < count {
		packed = InternalCollationData.element(start + $index) ?? return Err(InternalFault)
		$output = $output.append({
			primary: packed.shr_wrap(32).bitwise_and(0x3FFFF).to_u32_wrap(),
			secondary: packed.shr_wrap(16).bitwise_and(0xFFFF).to_u32_wrap(),
			tertiary: packed.bitwise_and(0xFFFF).to_u32_wrap(),
			variable: packed.bitwise_and(variable_bit) != 0,
			case_kind: packed.shr_wrap(51).bitwise_and(3).to_u8_wrap(),
		})
		$index = $index + 1
	}
	Ok($output)
}

append_implicit : List(Element), U32, U64 -> Try(List(Element), Collation.Error)
append_implicit = |out, scalar, limit| {
	if 2 > limit - out.len() {
		return Err(limit_error(Elements, limit, checked_sum(out.len(), 2)))
	}
	assigned = InternalGeneralCategory.lookup(scalar) != Cn
	weight = InternalCollationData.implicit(scalar, assigned)
	Ok(out.append(common_element(weight.first)).append(primary_weight(weight.second)))
}

primary_weight : U32 -> Element
primary_weight = |value| { primary: value, secondary: 0, tertiary: 0, variable: Bool.False, case_kind: 0 }

common_element : U32 -> Element
common_element = |primary| { primary, secondary: 0x20 * 4, tertiary: 2 * 4, variable: Bool.False, case_kind: 0 }

decimal_digit : U32 -> [None, Some(U8)]
decimal_digit = |code| match Scalar.from_u32(code) {
	Ok(scalar) => Numeric.decimal_digit(scalar)
	Err(_) => None
}

read_digits : List(Unit), U64, U64 -> Try({ digits : List(U8), next : U64 }, Collation.Error)
read_digits = |units, start, limit| {
	var $at = start
	var $count = 0.U64
	var $digits = []
	while $at < units.len() {
		unit = units.get($at) ?? return Err(InternalFault)
		if !unit.removed {
			match decimal_digit(unit.scalar) {
				None => break
				Some(digit) => {
					if $count == limit {
						return Err(limit_error(NumericDigits, limit, checked_sum($count, 1)))
					}
					if digit != 0 or !$digits.is_empty() {
						$digits = $digits.append(digit)
					}
					$count = $count + 1
				}
			}
		}
		$at = $at + 1
	}
	Ok({ digits: $digits, next: $at })
}

# A common prefix marks the digit group. Four nonzero 16-bit length units
# sort by significant-digit count at any allowed length. Individual digits
# then order equal-length numbers without integer conversion.
numeric_elements : List(Element), List(U8), U64 -> Try(List(Element), Collation.Error)
numeric_elements = |initial, digits, limit| {
	count = checked_sum(digits.len(), 5)
	if count > limit - initial.len() {
		return Err(limit_error(Elements, limit, checked_sum(initial.len(), count)))
	}
	var $out = initial.append(common_element(InternalCollationData.numeric_primary))
	for shift in [48.U8, 32, 16, 0] {
		length_unit = digits.len().shr_wrap(shift).bitwise_and(0xFFFF).to_u32_wrap() + 1
		$out = $out.append(primary_weight(length_unit))
	}
	for digit in digits {
		$out = $out.append(primary_weight(digit.to_u32() + 1))
	}
	Ok($out)
}

form_key : List(Element), Str, Collation.Options, U64 -> Try(List(U8), Collation.Error)
form_key = |elements, normalized, options, limit| {
	levels = levels_for(options.strength)
	var $bytes = []
	for level in levels {
		if level != Primary {
			$bytes = append_weight($bytes, 0, limit)?
		}
		var $after_variable = Bool.False
		for element in elements {
			shifted = options.alternate == Shifted and element.variable
			weights = weights_for(element, options, $after_variable)
			if element.primary != 0 {
				$after_variable = shifted
			}
			weight = match level {
				Primary => weights.primary
				Secondary => weights.secondary
				Tertiary => weights.tertiary
				Quaternary => weights.quaternary
			}
			if weight != 0 {
				$bytes = append_weight($bytes, weight, limit)?
			}
		}
	}
	if options.strength == Identical {
		$bytes = append_weight($bytes, 0, limit)?
		var $cursor = InternalUtf8.init(normalized)
		while Bool.True {
			match InternalUtf8.next($cursor) {
				Done => break
				One({ item, rest }) => {
					# UCA 18 reserves the lowest identical weight for U+FFFE;
					# every other weight preserves NFD scalar order.
					weight = if (item.scalar == 0xFFFE) 1 else item.scalar + 2
					$bytes = append_weight($bytes, weight, limit)?
					$cursor = rest
				}
			}
		}
	}
	Ok($bytes)
}

levels_for : Collation.Strength -> List([Primary, Secondary, Tertiary, Quaternary])
levels_for = |strength| match strength {
	Primary => [Primary]
	Secondary => [Primary, Secondary]
	Tertiary => [Primary, Secondary, Tertiary]
	Quaternary | Identical => [Primary, Secondary, Tertiary, Quaternary]
}

weights_for : Element, Collation.Options, Bool -> Levels
weights_for = |ce, options, after_variable| {
	moved = options.alternate == Shifted and ce.variable
	ignored = options.alternate == Shifted and ce.primary == 0 and after_variable
	all_zero = ce.primary == 0 and ce.secondary == 0 and ce.tertiary == 0
	if ignored or all_zero {
		return { primary: 0, secondary: 0, tertiary: 0, quaternary: 0 }
	}
	if moved {
		return { primary: 0, secondary: 0, tertiary: 0, quaternary: ce.primary }
	}
	quaternary = if options.alternate == Shifted {
		if (ce.primary == 0x0200 * 4) ce.primary else 0xFFFF * 4
	} else {
		0
	}
	{
		primary: ce.primary,
		secondary: ce.secondary,
		tertiary: tertiary_weight(ce, options.case_first),
		quaternary,
	}
}

tertiary_weight : Element, Collation.CaseFirst -> U32
tertiary_weight = |ce, order| {
	if ce.tertiary == 0 or order == Default {
		return ce.tertiary
	}
	case_weight = if ce.primary == 0 and ce.secondary == 0 {
		3
	} else if ce.case_kind == 1 {
		2
	} else if (ce.case_kind == 2) == (order == UpperFirst) {
		1
	} else {
		3
	}
	case_weight * 65536 + ce.tertiary
}

append_weight : List(U8), U32, U64 -> Try(List(U8), Collation.Error)
append_weight = |bytes, weight, limit| {
	if 4 > limit - bytes.len() {
		return Err(limit_error(KeyBytes, limit, checked_sum(bytes.len(), 4)))
	}
	Ok(
		bytes.append(weight.shr_wrap(24).to_u8_wrap())
			.append(weight.shr_wrap(16).to_u8_wrap())
			.append(weight.shr_wrap(8).to_u8_wrap())
			.append(weight.to_u8_wrap()),
	)
}

compare_bytes : List(U8), List(U8) -> [Lt, Eq, Gt]
compare_bytes = |left, right| {
	var $at = 0.U64
	while $at < left.len() and $at < right.len() {
		a = left.get($at) ?? 0
		b = right.get($at) ?? 0
		if a < b {
			return Lt
		}
		if a > b {
			return Gt
		}
		$at = $at + 1
	}
	if left.len() < right.len() {
		Lt
	} else if left.len() > right.len() {
		Gt
	} else {
		Eq
	}
}

limit_error : Collation.Resource, U64, U64 -> Collation.Error
limit_error = |resource, limit, required| LimitExceeded({ resource, limit, required })

checked_sum : U64, U64 -> U64
checked_sum = |left, right| left.plus_try(right) ?? U64.highest
