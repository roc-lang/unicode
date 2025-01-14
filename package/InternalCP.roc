module [
    CP,
    from_u32_unchecked,
    to_u32,
]

CP := U32 implements [Eq, Hash]

to_u32 : CP -> U32
to_u32 = \@CP(u32) -> u32

from_u32_unchecked : U32 -> CP
from_u32_unchecked = @CP
