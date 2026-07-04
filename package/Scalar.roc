import CodePoint

## A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value) - that is,
## any [code point](./CodePoint#CodePoint) except for [high-surrogate](./CodePoint#is_high_surrogate)
## and [low-surrogate](./CodePoint#is_low_surrogate) code points.
Scalar :: { cp : CodePoint }.{
    to_u32 : Scalar -> U32
    to_u32 = |{ cp }| {
        cp.to_u32()
    }

    ## Any Unicode code point except high-surrogate and low-surrogate code points.
    ##
    ## Note UTF-8 does not use surrogates as it is a variable-width encoding unlike UTF-16.
    from_u32 : U32 -> Try(Scalar, [InvalidScalar])
    from_u32 = |u32| {
        in_range_a = u32 >= 0x0000 and u32 <= 0xD7FF
        in_range_b = u32 >= 0xE000 and u32 <= 0x10FFFF
        if in_range_a or in_range_b {
            Ok({ cp: CodePoint.internal_from_u32_unchecked(u32) })
        } else {
            Err(InvalidScalar)
        }
    }

    to_code_point : Scalar -> CodePoint
    to_code_point = |{ cp }| cp

    ## Convert a code point to a scalar value. This can fail if the given
    ## code point is
    from_code_point : CodePoint -> Try(Scalar, [NonScalarCodePt])
    from_code_point = |code_point| {
        if code_point.is_valid_scalar() {
            Ok({ cp: code_point })
        } else {
            Err(NonScalarCodePt)
        }
    }

    from_str : Str -> List(Scalar)
    from_str = |_str| {
        crash "TODO implement"
    }

    # append_to_str : Scalar, Str -> Str
    # append_to_str = |{ cp }, str| {
    #     match Str.append_scalar(str, u32) {
    #         Ok(answer) => answer
    #         Err(InvalidScalar) => {
    #             u32str = u32.to_str()
    #
    #             crash "append_to_str received a Scalar value of $(u32str). This is an invalid Unicode scalar value, so it should not have been possible to obtain a `Scalar` which wraps it!"
    #         }
    #     }
    # }

    # TODO WHAT IS THIS?
    # fold_str : Str, state, (state, Scalar -> state) -> state

    # TODO WHAT IS THIS?
    # fold_str_until : Str, state, (state, U32 -> [Break state, Continue state]) -> state

    ## If the string begins with a [Unicode code point](http://www.unicode.org/glossary/#code_point)
    ## equal to the given [U32], returns [Bool.True]. Otherwise returns [Bool.Talse].
    ##
    ## If the given string is empty, or if the given [U32] is not a valid
    ## code point, returns [Bool.False].
    ## ```
    ## expect Str.starts_with_scalar("鹏 means 'roc'", 40527) # "鹏" is Unicode scalar 40527
    ## expect !Str.starts_with_scalar("9", 9) # the Unicode scalar for "9" is 57, not 9
    ## expect !Str.starts_with_scalar("", 40527)
    ## ```
    ##
    ## ## Performance Details
    ##
    ## This runs slightly faster than [Str.starts_with], so
    ## if you want to check whether a string begins with something that's representable
    ## in a single code point, you can use (for example) `Str.starts_with_scalar('鹏')`
    ## instead of `Str.starts_with("鹏")`. ('鹏' evaluates to the [U32] value `40527`.)
    ## This will not work for graphemes which take up multiple code points, however;
    ## `Str.starts_with_scalar('👩‍👩‍👦‍👦')` would be a compiler error because 👩‍👩‍👦‍👦 takes up
    ## multiple code points and cannot be represented as a single [U32].
    ## You'd need to use `Str.starts_with_scalar("🕊")` instead.
    starts_with_scalar : Str, U32 -> Bool

    ## Returns a [List] of the [Unicode scalar values](https://unicode.org/glossary/#unicode_scalar_value)
    ## in the given string.
    ##
    ## (Roc strings contain only scalar values, not [surrogate code points](https://unicode.org/glossary/#surrogate_code_point),
    ## so this is equivalent to returning a list of the string's [code points](https://unicode.org/glossary/#code_point).)
    ## ```
    ## expect Str.to_scalars("Roc") == [82, 111, 99]
    ## expect Str.to_scalars("鹏") == [40527]
    ## expect Str.to_scalars("சி") == [2970, 3007]
    ## expect Str.to_scalars("🐦") == [128038]
    ## expect Str.to_scalars("👩‍👩‍👦‍👦") == [128105, 8205, 128105, 8205, 128102, 8205, 128102]
    ## expect Str.to_scalars("I ♥ Roc") == [73, 32, 9829, 32, 82, 111, 99]
    ## expect Str.to_scalars("") == []
    ## ```
    to_scalars : Str -> List(U32)

    ## Append a [U32] scalar to the given string. If the given scalar is not a valid
    ## unicode value, it returns [Err(InvalidScalar)].
    ## ```
    ## expect Str.append_scalar("H", 105) == Ok("Hi")
    ## expect Str.append_scalar("😢", 0xabcdef) == Err(InvalidScalar)
    ## ```
    append_scalar : Str, U32 -> Try(Str, [InvalidScalar])
    append_scalar = |string, u32| {
        if CodePoint.is_valid_scalar(CodePoint.internal_from_u32_unchecked(u32)) {
            Ok(append_scalar_unsafe(string, u32))
        } else {
            Err(InvalidScalar)
        }
    }

    ## Folds over the unicode [U32] values for the given [Str] and calls a function
    ## to update state for each.
    ## ```
    ## f : List U32, U32 -> List U32
    ## f = |state, scalar| List.append state scalar
    ## expect Str.fold_scalars "ABC" [] f == [65, 66, 67]
    ## ```
    fold_scalars : Str, state, (state, U32 -> state) -> state
    fold_scalars = |string, init, step| {
        fold_scalars_help(string, init, step, 0, Str.count_utf8_bytes(string))
    }

    fold_scalars_until : Str, state, (state, U32 -> [Break(state), Continue(state)]) -> state
    fold_scalars_until = |string, init, step| {
        fold_scalars_until_help(string, init, step, 0, Str.count_utf8_bytes(string))
    }
}

# private methods
append_scalar_unsafe : Str, U32 -> Str
append_scalar_unsafe = |_string, _scalar| {
    crash "TODO"
}

get_scalar_unsafe : Str, U64 -> { scalar : U32, bytes_parsed : U64 }

fold_scalars_help : Str, state, (state, U32 -> state), U64, U64 -> state
fold_scalars_help = |string, state, step, index, length| {
    if index < length {
        { scalar, bytes_parsed } = get_scalar_unsafe(string, index)
        new_state = step(state, scalar)

        fold_scalars_help(string, new_state, step, (index + bytes_parsed), length)
    } else {
        state
    }
}

## Folds over the unicode [U32] values for the given [Str] and calls a function
## to update state for each.
## ```
## f : List(U32, U32) -> [Break(List(U32)), Continue(List(U32))]
## f = |state, scalar| {
##     check = 66
##     if scalar == check
##         Break([check])
##     else
##         Continue(state.append(scalar))
## }
## expect Str.fold_scalars_until("ABC", [], f) == [66]
## expect Str.fold_scalars_until("AxC", [], f) == [65, 120, 67]
## ```
fold_scalars_until_help : Str, state, (state, U32 -> [Break(state), Continue(state)]), U64, U64 -> state
fold_scalars_until_help = |string, state, step, index, length| {
    if index < length {
        { scalar, bytes_parsed } = get_scalar_unsafe(string, index)

        match step(state, scalar) {
            Continue(new_state) =>
                fold_scalars_until_help(string, new_state, step, (index + bytes_parsed), length)

            Break(new_state) =>
                new_state
            }
    } else {
        state
    }
}
