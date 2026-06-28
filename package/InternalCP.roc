#TODO: simplify to InternalCP :: U32 once it's possible to implement to_u32
InternalCP :: { value: U32 }.{
    to_u32 : InternalCP -> U32
    to_u32 = |icp| icp.value

    from_u32_unchecked : U32 -> InternalCP
    from_u32_unchecked = |value| InternalCP.{ value }
}

expect {
    icp = InternalCP.from_u32_unchecked(123)
    result = icp.to_u32()
    result == 123
}