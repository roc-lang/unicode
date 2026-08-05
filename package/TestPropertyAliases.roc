import InternalPropertyAliases

TestPropertyAliases :: [].{
    allocation_probe : U8 -> U64
    allocation_probe = |selector| {
        category = if selector == 0 Cc else Lu
        combining_class = if selector == 0 230 else 0
        gc_property = InternalPropertyAliases.general_category_property
        ccc_property = InternalPropertyAliases.canonical_combining_class_property

        gc_alias_len = match InternalPropertyAliases.general_category_alias_at(category, selector.bitwise_and(1) + 2) {
            Some(alias) => alias.count_utf8_bytes()
            None => 0
        }
        gc_missing = match InternalPropertyAliases.general_category_alias_at(category, selector.bitwise_or(0x80)) {
            Some(_) => 0
            None => 1
        }
        ccc_short_len = match InternalPropertyAliases.canonical_combining_class_short(combining_class) {
            Some(alias) => alias.count_utf8_bytes()
            None => 0
        }
        ccc_long_len = match InternalPropertyAliases.canonical_combining_class_long(combining_class) {
            Some(alias) => alias.count_utf8_bytes()
            None => 0
        }
        ccc_alias_len = match InternalPropertyAliases.canonical_combining_class_alias_at(combining_class, selector.bitwise_and(1) + 2) {
            Some(alias) => alias.count_utf8_bytes()
            None => 0
        }
        ccc_missing = match InternalPropertyAliases.canonical_combining_class_alias_at(selector.bitwise_or(0xF0), selector.bitwise_and(1)) {
            Some(_) => 0
            None => 1
        }

        gc_property.short.count_utf8_bytes()
            + gc_property.long.count_utf8_bytes()
            + ccc_property.short.count_utf8_bytes()
            + ccc_property.long.count_utf8_bytes()
            + InternalPropertyAliases.general_category_short(category).count_utf8_bytes()
            + InternalPropertyAliases.general_category_long(category).count_utf8_bytes()
            + InternalPropertyAliases.general_category_alias_count(category).to_u64()
            + gc_alias_len
            + gc_missing
            + ccc_short_len
            + ccc_long_len
            + InternalPropertyAliases.canonical_combining_class_alias_count(combining_class).to_u64()
            + ccc_alias_len
            + ccc_missing
    }
}
