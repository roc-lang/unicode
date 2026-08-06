import InternalEAW
import InternalEmoji
import InternalGraphemeData

## Stable, test-private numeric encodings of generated Unicode properties.
## This module is exposed only by test-main.roc, never by the public package.
TestProperties :: {}.{
    gcb : U32 -> U8
    gcb = |u32| {
        match InternalGraphemeData.lookup(u32).gcb {
            Other => 0
            CR => 1
            LF => 2
            Control => 3
            Extend => 4
            ZWJ => 5
            RI => 6
            Prepend => 7
            SpacingMark => 8
            L => 9
            V => 10
            T => 11
            LV => 12
            LVT => 13
        }
    }

    eaw : U32 -> U8
    eaw = |u32| {
        match InternalEAW.east_asian_width_property(u32) {
            N => 0
            A => 1
            F => 2
            H => 3
            Na => 4
            W => 5
        }
    }

    emoji : U32 -> U8
    emoji = |u32| {
        (if InternalEmoji.is_emoji(u32) 1 else 0)
            + (if InternalEmoji.is_presentation(u32) 2 else 0)
            + (if InternalEmoji.is_modifier(u32) 4 else 0)
            + (if InternalEmoji.is_base(u32) 8 else 0)
            + (if InternalEmoji.is_component(u32) 16 else 0)
            + (if InternalEmoji.is_pictographic(u32) 32 else 0)
    }
}
