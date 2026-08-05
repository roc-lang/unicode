## Order-sensitive semantic signature shared by both benchmark paths.
## Every field boundary and exact value is mixed, so equal output proves much
## more than equal spelling lengths or optional-value presence.
Signature :: [].{
    mix : U64, U64 -> U64
    mix = |state, value| {
        state
            .bitwise_xor(value.plus_wrap(0x9E3779B97F4A7C15))
            .times_wrap(0x100000001B3)
    }

    mix_bool : U64, Bool -> U64
    mix_bool = |state, value| Signature.mix(state, if value 1 else 0)

    mix_str : U64, Str -> U64
    mix_str = |initial, value| {
        var state = Signature.mix(initial, 0x535452)
        for byte in value.iter_utf8() {
            state = Signature.mix(state, byte.to_u64())
        }
        Signature.mix(state, 0x454E44)
    }
}
