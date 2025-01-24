module [
    PropertyMap,
    CPMeta,
    remove_trailing_slash,
    take_hex_bytes,
    is_hex,
    hex_to_dec,
    starts_with_hex,
    hex_bytes_to_u32,
    hex_str_to_u32,
    property_map_from_file,
    filter_property_map,
    meta_to_expression,
]

CPMeta : [Single U32, Range U32 U32]
PropertyMap a : { cp : CPMeta, prop : a }

filter_property_map : List (PropertyMap a), (PropertyMap a -> Result CPMeta [NotNeeded]) -> List CPMeta
filter_property_map = |map, filter| List.keep_oks(map, filter)

remove_trailing_slash : Str -> Str
remove_trailing_slash = |str|
    trimmed = str |> Str.trim
    reversed = trimmed |> Str.to_utf8 |> List.reverse

    when reversed is
        [a, ..] if a == '/' ->
            reversed
            |> List.drop_first(1)
            |> List.reverse
            |> Str.from_utf8
            |> Result.with_default("")

        _ -> trimmed

expect remove_trailing_slash("abc  ") == "abc"
expect remove_trailing_slash("  abc/package/  ") == "abc/package"

take_hex_bytes : { val : List U8, rest : List U8 } -> { val : List U8, rest : List U8 }
take_hex_bytes = |input|
    when input.rest is
        [] -> input
        [first, ..] ->
            if first |> is_hex then
                # take the first hex byte and continue
                take_hex_bytes(
                    {
                        val: input.val |> List.append(first),
                        rest: input.rest |> List.drop_first(1),
                    },
                )
            else
                input

expect
    bytes = [35, 32, 61, 61, 61] # "# ==="
    take_hex_bytes({ val: [], rest: bytes }) == { val: [], rest: bytes }

expect
    bytes = [68, 54, 69, 49, 46, 46, 68, 54, 70, 66, 32, 32] # "D6E1..D6FB  "
    take_hex_bytes({ val: [], rest: bytes }) == { val: [68, 54, 69, 49], rest: [46, 46, 68, 54, 70, 66, 32, 32] }

is_hex : U8 -> Bool
is_hex = |u8| u8 == '0' or u8 == '1' or u8 == '2' or u8 == '3' or u8 == '4' or u8 == '5' or u8 == '6' or u8 == '7' or u8 == '8' or u8 == '9' or u8 == 'A' or u8 == 'B' or u8 == 'C' or u8 == 'D' or u8 == 'E' or u8 == 'F'

expect is_hex('0')
expect is_hex('A')
expect is_hex('F')
expect !(is_hex(';'))
expect !(is_hex('#'))

starts_with_hex : Str -> Result Str [NonHex]
starts_with_hex = |str|
    when Str.to_utf8(str) is
        [a, ..] if is_hex(a) -> Ok(str)
        _ -> Err(NonHex)

expect starts_with_hex("# ===") == Err(NonHex)
expect starts_with_hex("0000..") == Ok("0000..")

hex_str_to_u32 : Str -> U32
hex_str_to_u32 = |str|
    str |> Str.to_utf8 |> hex_bytes_to_u32

hex_bytes_to_u32 : List U8 -> U32
hex_bytes_to_u32 = |bytes|
    bytes
    |> List.reverse
    |> List.walk_with_index(0, |accum, byte, i| accum + (Num.pow_int(16, Num.to_u32(i))) * (hex_to_dec(byte)))
    |> Num.to_u32

expect hex_bytes_to_u32(['0', '0', '0', '0']) == 0
expect hex_bytes_to_u32(['0', '0', '0', '1']) == 1
expect hex_bytes_to_u32(['0', '0', '0', 'F']) == 15
expect hex_bytes_to_u32(['0', '0', '1', '0']) == 16
expect hex_bytes_to_u32(['0', '0', 'F', 'F']) == 255
expect hex_bytes_to_u32(['0', '1', '0', '0']) == 256
expect hex_bytes_to_u32(['0', 'F', 'F', 'F']) == 4095
expect hex_bytes_to_u32(['1', '0', '0', '0']) == 4096
expect hex_bytes_to_u32(['1', '6', 'F', 'F', '1']) == 94193

hex_to_dec : U8 -> U32
hex_to_dec = |byte|
    when byte is
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'A' -> 10
        'B' -> 11
        'C' -> 12
        'D' -> 13
        'E' -> 14
        'F' -> 15
        _ -> 0

expect hex_to_dec('0') == 0
expect hex_to_dec('F') == 15

property_map_from_file : Str, (Str -> Result a [ParsingError]) -> List { cp : CPMeta, prop : a }
property_map_from_file = |file, parse_prop_part|
    file
    |> Str.split_on("\n")
    |> List.keep_oks(Helpers.starts_with_hex)
    |> List.map(
        |l|
            when Str.split_on(l, ";") is
                [hex_part, prop_part] ->
                    when (parse_hex_part(hex_part), parse_prop_part(prop_part)) is
                        (Ok(cp), Ok(prop)) -> { cp, prop }
                        _ -> crash("Error parsing line -- ${l}")

                _ -> crash("Error unexpected ';' on line -- ${l}"),
    )

parse_hex_part : Str -> Result CPMeta [ParsingError]
parse_hex_part = |hex_part|
    when hex_part |> Str.trim |> Str.split_on("..") is
        [single] ->
            when code_point_parser(single) is
                Ok(a) -> Ok(Single(a))
                Err(_) -> Err(ParsingError)

        [start, end] ->
            when (code_point_parser(start), code_point_parser(end)) is
                (Ok(a), Ok(b)) -> Ok(Range(a, b))
                _ -> Err(ParsingError)

        _ -> Err(ParsingError)

expect parse_hex_part("0890..0891    ") == Ok(Range(2192, 2193))
expect parse_hex_part("08E2          ") == Ok(Single(2274))

code_point_parser : Str -> Result U32 [ParsingError]
code_point_parser = |input|

    { val: hex_bytes } = take_hex_bytes({ val: [], rest: Str.to_utf8(input) })

    when hex_bytes is
        [] -> Err(ParsingError)
        _ -> Ok(hex_bytes_to_u32(hex_bytes))

expect code_point_parser("0000") == Ok(0)
expect code_point_parser("16FF1") == Ok(94193)
expect code_point_parser("# ===") == Err(ParsingError)

# Convert to a string suitible for building a function
meta_to_expression : CPMeta -> Str
meta_to_expression = |cp|
    when cp is
        Single(a) -> "(u32 == ${Num.to_str(a)})"
        Range(a, b) -> "(u32 >= ${Num.to_str(a)} and u32 <= ${Num.to_str(b)})"
