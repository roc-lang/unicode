import InternalUtf8

## Allocation-free UAX #44 revision 36, LM3 loose matching for property aliases.
##
## Property aliases are ASCII, but ignored whitespace is recognized by its
## Unicode White_Space code points so callers do not need to sanitize input.
## Case, whitespace, hyphen, underscore, and one nonempty initial `is` prefix
## are ignored; no normalized string is materialized.
InternalLooseAlias :: [].{
    matches : Str, Str -> Bool
    matches = |left, right| {
        left_count = significant_count(left)
        right_count = significant_count(right)
        left_skip = initial_is_prefix(left, left_count)
        right_skip = initial_is_prefix(right, right_count)
        left_length = left_count - left_skip
        right_length = right_count - right_skip

        if left_length != right_length {
            Bool.False
        } else {
            var index = 0.U64
            while index < left_length {
                left_scalar = normalized_at(left, index + left_skip)
                right_scalar = normalized_at(right, index + right_skip)
                if left_scalar != right_scalar {
                    return Bool.False
                }
                index = index + 1
            }
            Bool.True
        }
    }
}

significant_count : Str -> U64
significant_count = |source| {
    InternalUtf8.fold_scalars(
        source,
        0.U64,
        |count, scalar, _byte_start, _byte_end, _scalar_index| {
            if is_ignored(scalar) count else count + 1
        },
    )
}

normalized_at : Str, U64 -> U32
normalized_at = |source, target| {
    packed = InternalUtf8.fold_scalars(
        source,
        0.U128,
        |state, scalar, _byte_start, _byte_end, _scalar_index| {
            significant_index = state.bitwise_and(0xFFFFFFFFFFFFFFFF)
            if is_ignored(scalar) or significant_index > target.to_u128() {
                state
            } else if significant_index == target.to_u128() {
                normalized = ascii_lower(scalar)
                encoded = if normalized <= 0x7F normalized.to_u128() else 0xFF
                (significant_index + 1).bitwise_or(encoded.shl_wrap(64))
            } else {
                significant_index + 1
            }
        },
    )
    packed.shr_wrap(64).to_u32_wrap()
}

initial_is_prefix : Str, U64 -> U64
initial_is_prefix = |source, count| {
    # UAX #44 explicitly does not remove the prefix from the exact alias `IS`.
    if count > 2 and normalized_at(source, 0) == 0x69 and normalized_at(source, 1) == 0x73 {
        2
    } else {
        0
    }
}

is_ignored : U32 -> Bool
is_ignored = |scalar| {
    scalar == 0x2D
        or scalar == 0x5F
        or (scalar >= 0x09 and scalar <= 0x0D)
        or scalar == 0x20
        or scalar == 0x85
        or scalar == 0xA0
        or scalar == 0x1680
        or (scalar >= 0x2000 and scalar <= 0x200A)
        or scalar == 0x2028
        or scalar == 0x2029
        or scalar == 0x202F
        or scalar == 0x205F
        or scalar == 0x3000
}

ascii_lower : U32 -> U32
ascii_lower = |scalar| if scalar >= 0x41 and scalar <= 0x5A scalar + 0x20 else scalar
