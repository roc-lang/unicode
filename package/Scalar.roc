import ByteRange
import CodePoint
import InternalEAW
import InternalUtf8

## A Unicode scalar value: a code point other than a surrogate.
##
## `Scalar` is sealed. The only public numeric constructor checks both the
## Unicode upper bound and the surrogate range. Every scalar decoded from a
## Roc `Str` therefore needs no repeated validity check in later algorithms.
Scalar :: { code_point : CodePoint }.{
    ## A scalar and its absolute coordinates in the original logical source.
    ## `byte_range` is half-open and `scalar_index` is zero based.
    LocatedScalar : {
        scalar : Scalar,
        byte_range : ByteRange,
        scalar_index : U64,
    }

    EastAsianWidth := [Fullwidth, Wide, Ambiguous, Halfwidth, Neutral, Narrow]

    ## Construct a scalar after rejecting surrogates and values above
    ## `U+10FFFF`.
    ##
    ## This is constant time, does not allocate, and is total for every `U32`.
    from_u32 : U32 -> Try(Scalar, [InvalidScalar])
    from_u32 = |value| {
        match CodePoint.from_u32(value) {
            Err(_) => Err(InvalidScalar)
            Ok(code_point) => {
                if CodePoint.is_surrogate(code_point) {
                    Err(InvalidScalar)
                } else {
                    Ok({ code_point: code_point })
                }
            }
        }
    }

    ## Validate a full-domain code point as a scalar.
    ##
    ## This is constant time and does not allocate.
    from_code_point : CodePoint -> Try(Scalar, [Surrogate])
    from_code_point = |code_point| {
        if CodePoint.is_surrogate(code_point) {
            Err(Surrogate)
        } else {
            Ok({ code_point: code_point })
        }
    }

    ## Return the numeric Unicode scalar value.
    ##
    ## This is constant time and does not allocate.
    to_u32 : Scalar -> U32
    to_u32 = |{ code_point }| CodePoint.to_u32(code_point)

    ## Convert this scalar to the corresponding full-domain code point.
    ##
    ## This cannot fail because every scalar is in the code-point domain. It is
    ## constant time and does not allocate.
    to_code_point : Scalar -> CodePoint
    to_code_point = |{ code_point }| code_point

    ## Iterate lazily over the scalars in a valid Roc `Str`.
    ##
    ## Each result carries a half-open UTF-8 byte range and zero-based scalar
    ## index, both absolute from the beginning of `source`. The scan is O(B) in
    ## visited bytes, uses constant algorithmic state and stack, and creates no
    ## intermediate byte/scalar list. Stopping iteration early leaves the
    ## suffix undecoded. The iterator retains `source` for its own lifetime;
    ## yielded scalars and integer ranges do not retain it.
    iter : Str -> Iter(LocatedScalar)
    iter = |source| {
        next_located = |cursor| {
            match InternalUtf8.next(cursor) {
                Done => Err(NoMore)
                One({ item, rest }) => {
                    # The valid-Str decoder has already excluded surrogates;
                    # constructing the sealed representation here removes a
                    # redundant checked branch from every yielded item.
                    scalar : Scalar
                    scalar = { code_point: item.code_point }
                    Ok(({
                        scalar,
                        byte_range: item.byte_range,
                        scalar_index: item.scalar_index,
                    }, rest))
                }
            }
        }

        Iter.custom(InternalUtf8.init(source), Unknown, next_located)
    }

    ## Return the number of bytes in this scalar's UTF-8 encoding.
    ##
    ## This is constant time and does not allocate.
    utf8_len : Scalar -> U8
    utf8_len = |scalar| {
        value = Scalar.to_u32(scalar)
        if value < 0x80 {
            1
        } else if value < 0x800 {
            2
        } else if value < 0x10000 {
            3
        } else {
            4
        }
    }

    ## Encode one scalar as a newly allocated list of one to four UTF-8 bytes.
    ##
    ## The sealed input makes surrogate encoding impossible. Work and output
    ## size are constant. The returned list owns its bytes.
    to_utf8 : Scalar -> List(U8)
    to_utf8 = |scalar| encode_append([], scalar)

    ## Append one scalar's UTF-8 encoding to an existing byte list, provided
    ## the result does not exceed `max_output_bytes`.
    ##
    ## This is O(1) excluding a possible reallocation/copy of `bytes`. It never
    ## emits a surrogate encoding. The caller-supplied, operation-specific
    ## limit and all length arithmetic are checked before reserve or append;
    ## failure leaves the caller's original list available.
    append_utf8 : List(U8), Scalar, U64 -> Try(List(U8), [OutputLimitExceeded({ limit : U64, required : U64 })])
    append_utf8 = |bytes, scalar, max_output_bytes| {
        width = Scalar.utf8_len(scalar).to_u64()
        required = match bytes.len().plus_try(width) {
            Err(Overflow) => return Err(OutputLimitExceeded({ limit: max_output_bytes, required: U64.highest }))
            Ok(length) => length
        }

        if required > max_output_bytes {
            Err(OutputLimitExceeded({ limit: max_output_bytes, required }))
        } else {
            Ok(encode_append(bytes, scalar))
        }
    }

    ## Encode this scalar as an independently owned `Str`.
    ##
    ## Work and output size are constant. The implementation creates at most
    ## the fixed one-to-four-byte encoding and does not retain another source.
    to_str : Scalar -> Str
    to_str = |scalar| Str.from_utf8_lossy(Scalar.to_utf8(scalar))

    ## Return this scalar's Unicode East_Asian_Width property.
    ##
    ## This is a scalar property, not a terminal-column or glyph-width result.
    ## It is constant time and does not allocate.
    east_asian_width : Scalar -> EastAsianWidth
    east_asian_width = |scalar| {
        match InternalEAW.east_asian_width_property(Scalar.to_u32(scalar)) {
            F => Fullwidth
            W => Wide
            A => Ambiguous
            H => Halfwidth
            N => Neutral
            Na => Narrow
        }
    }

    ## Compare two scalars. This is constant time and does not allocate.
    is_eq : Scalar, Scalar -> Bool
    is_eq = |left, right| Scalar.to_u32(left) == Scalar.to_u32(right)
}

encode_append : List(U8), Scalar -> List(U8)
encode_append = |bytes, scalar| {
    value = Scalar.to_u32(scalar)

    if value < 0x80 {
        bytes.append(value.to_u8_wrap())
    } else if value < 0x800 {
        byte1 = value.shr_wrap(6).bitwise_or(0b11000000).to_u8_wrap()
        byte2 = value.bitwise_and(0b00111111).bitwise_or(0b10000000).to_u8_wrap()

        bytes.reserve(2).append(byte1).append(byte2)
    } else if value < 0x10000 {
        byte1 = value.shr_wrap(12).bitwise_or(0b11100000).to_u8_wrap()
        byte2 = value.shr_wrap(6).bitwise_and(0b00111111).bitwise_or(0b10000000).to_u8_wrap()
        byte3 = value.bitwise_and(0b00111111).bitwise_or(0b10000000).to_u8_wrap()

        bytes.reserve(3).append(byte1).append(byte2).append(byte3)
    } else {
        byte1 = value.shr_wrap(18).bitwise_or(0b11110000).to_u8_wrap()
        byte2 = value.shr_wrap(12).bitwise_and(0b00111111).bitwise_or(0b10000000).to_u8_wrap()
        byte3 = value.shr_wrap(6).bitwise_and(0b00111111).bitwise_or(0b10000000).to_u8_wrap()
        byte4 = value.bitwise_and(0b00111111).bitwise_or(0b10000000).to_u8_wrap()

        bytes.reserve(4).append(byte1).append(byte2).append(byte3).append(byte4)
    }
}
