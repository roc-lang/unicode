## GENERATED from vendor/unicode/17.0.0 (Emoji 17.0). Run `python3 scripts/unicode_data.py generate`. ##
import CodePoint
import InternalEmojiData

InternalEmoji :: {}.{
    EMOJI : [Pictographic, Base, Modifier, Presentation, Component, Emoji]

    from_cp : CodePoint -> Try(EMOJI, [NonEmojiCodePoint])
    from_cp = |cp| {
        u32 = cp.to_u32()

        properties = InternalEmojiData.lookup(u32)
        if properties.extended_pictographic {
            Ok(Pictographic)
        } else if properties.emoji_modifier_base {
            Ok(Base)
        } else if properties.emoji_modifier {
            Ok(Modifier)
        } else if properties.emoji_presentation {
            Ok(Presentation)
        } else if properties.emoji_component {
            Ok(Component)
        } else if properties.emoji {
            Ok(Emoji)
        } else {
            Err(NonEmojiCodePoint)
        }
    }

    is_pictographic : U32 -> Bool
    is_pictographic = |u32| InternalEmojiData.lookup(u32).extended_pictographic

    is_base : U32 -> Bool
    is_base = |u32| InternalEmojiData.lookup(u32).emoji_modifier_base

    is_modifier : U32 -> Bool
    is_modifier = |u32| InternalEmojiData.lookup(u32).emoji_modifier

    is_presentation : U32 -> Bool
    is_presentation = |u32| InternalEmojiData.lookup(u32).emoji_presentation

    is_component : U32 -> Bool
    is_component = |u32| InternalEmojiData.lookup(u32).emoji_component

    is_emoji : U32 -> Bool
    is_emoji = |u32| InternalEmojiData.lookup(u32).emoji
}
