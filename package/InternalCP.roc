module [
    CP,
    fromU32Unchecked,
    toU32,
]

CP := U32 implements [Eq, Hash]

toU32 : CP -> U32
toU32 = \@CP u32 -> u32

fromU32Unchecked : U32 -> CP
fromU32Unchecked = @CP
