Helpers :: {}.{
	CPMeta : [Single(U32), Range(U32, U32)]
	PropertyMap(a) : { cp : CPMeta, prop : a }

	filter_property_map : List(PropertyMap(a)), (PropertyMap(a) -> Try(CPMeta, [NotNeeded])) -> List(CPMeta)
	filter_property_map = |map, filter| {
		map->keep_oks(filter)
	}

	remove_trailing_slash : Str -> Str
	remove_trailing_slash = |str| {
		str.trim().drop_suffix("/")
	}

	take_hex_bytes : { val : List(U8), rest : List(U8) } -> { val : List(U8), rest : List(U8) }
	take_hex_bytes = |input| {
		match input.rest {
			[] => input
			[first, ..] => {
				if is_hex(first) {
					take_hex_bytes(
						{
							val: input.val.append(first),
							rest: input.rest.drop_first(1),
						},
					)
				} else {
					input
				}
			}
		}
	}

	is_hex : U8 -> Bool
	is_hex = |u8| {
		u8 == '0' or u8 == '1' or u8 == '2' or u8 == '3' or u8 == '4' or u8 == '5' or u8 == '6' or u8 == '7' or u8 == '8' or u8 == '9' or u8 == 'A' or u8 == 'B' or u8 == 'C' or u8 == 'D' or u8 == 'E' or u8 == 'F'
	}

	starts_with_hex : Str -> Try(Str, [NonHex])
	starts_with_hex = |str| {
		match str.to_utf8() {
			[a, ..] if is_hex(a) => Ok(str)
			_ => Err(NonHex)
		}
	}

	hex_str_to_u32 : Str -> U32
	hex_str_to_u32 = |str| {
		str.to_utf8()->hex_bytes_to_u32()
	}

	hex_bytes_to_u32 : List(U8) -> U32
	hex_bytes_to_u32 = |bytes| {
		bytes.fold(0, |acc, char| acc * 16 + hex_to_dec(char))
	}

	hex_to_dec : U8 -> U32
	hex_to_dec = |byte| {
		match byte {
			'0' => 0
			'1' => 1
			'2' => 2
			'3' => 3
			'4' => 4
			'5' => 5
			'6' => 6
			'7' => 7
			'8' => 8
			'9' => 9
			'A' => 10
			'B' => 11
			'C' => 12
			'D' => 13
			'E' => 14
			'F' => 15
			_ => 0
		}
	}

	property_map_from_file : Str, (Str -> Try(a, [ParsingError])) -> List({ cp : CPMeta, prop : a })
	property_map_from_file = |file, parse_prop_part| {
		file
			.split_on("\n")
			->keep_oks(starts_with_hex)
			.map(
				|l| {
					match l.split_on(";") {
						[hex_part, prop_part] => {
							match (parse_hex_part(hex_part), parse_prop_part(prop_part)) {
								(Ok(cp), Ok(prop)) => { cp, prop }
								_ => {
									crash "Error parsing line -- ${l}"
								}
							}
						}
						_ => {
							crash "Error unexpected ';' on line -- ${l}"
						}
					}
				},
			)
	}

	# Convert to a string suitible for building a function
	meta_to_expression : CPMeta -> Str
	meta_to_expression = |cp| {
		match cp {
			Single(a) => "(u32 == ${a.to_str()})"
			Range(a, b) => "(u32 >= ${a.to_str()} and u32 <= ${b.to_str()})"
		}
	}
}

# private methods
parse_hex_part : Str -> Try(Helpers.CPMeta, [ParsingError])
parse_hex_part = |hex_part| {
	match hex_part.trim().split_on("..") {
		[single] => {
			match code_point_parser(single) {
				Ok(a) => Ok(Single(a))
				Err(_) => Err(ParsingError)
			}
		}
		[start, end] => {
			match (code_point_parser(start), code_point_parser(end)) {
				(Ok(a), Ok(b)) => Ok(Range(a, b))
				_ => Err(ParsingError)
			}
		}
		_ => Err(ParsingError)
	}
}

code_point_parser : Str -> Try(U32, [ParsingError])
code_point_parser = |input| {
	{ val: hex_bytes, rest: _ } = Helpers.take_hex_bytes({ val: [], rest: input.to_utf8() })

	match hex_bytes {
		[] => Err(ParsingError)
		_ => Ok(Helpers.hex_bytes_to_u32(hex_bytes))
	}
}

# expects
expect Helpers.remove_trailing_slash("abc  ") == "abc"
expect Helpers.remove_trailing_slash("  abc/package/  ") == "abc/package"

expect {
	bytes = [35, 32, 61, 61, 61] # "# ==="
	Helpers.take_hex_bytes({ val: [], rest: bytes }) == { val: [], rest: bytes }
}

expect {
	bytes = [68, 54, 69, 49, 46, 46, 68, 54, 70, 66, 32, 32] # "D6E1..D6FB  "
	Helpers.take_hex_bytes({ val: [], rest: bytes }) == { val: [68, 54, 69, 49], rest: [46, 46, 68, 54, 70, 66, 32, 32] }
}

expect Helpers.is_hex('0')
expect Helpers.is_hex('A')
expect Helpers.is_hex('F')
expect !(Helpers.is_hex(';'))
expect !(Helpers.is_hex('#'))

expect Helpers.starts_with_hex("# ===") == Err(NonHex)
expect Helpers.starts_with_hex("0000..") == Ok("0000..")

expect Helpers.hex_bytes_to_u32(['0', '0', '0', '0']) == 0
expect Helpers.hex_bytes_to_u32(['0', '0', '0', '1']) == 1
expect Helpers.hex_bytes_to_u32(['0', '0', '0', 'F']) == 15
expect Helpers.hex_bytes_to_u32(['0', '0', '1', '0']) == 16
expect Helpers.hex_bytes_to_u32(['0', '0', 'F', 'F']) == 255
expect Helpers.hex_bytes_to_u32(['0', '1', '0', '0']) == 256
expect Helpers.hex_bytes_to_u32(['0', 'F', 'F', 'F']) == 4095
expect Helpers.hex_bytes_to_u32(['1', '0', '0', '0']) == 4096
expect Helpers.hex_bytes_to_u32(['1', '6', 'F', 'F', '1']) == 94193

expect Helpers.hex_to_dec('0') == 0
expect Helpers.hex_to_dec('F') == 15

expect parse_hex_part("0890..0891    ") == Ok(Range(2192, 2193))
expect parse_hex_part("08E2          ") == Ok(Single(2274))

expect code_point_parser("0000") == Ok(0)
expect code_point_parser("16FF1") == Ok(94193)
expect code_point_parser("# ===") == Err(ParsingError)

# TODO: the following should soon be available in Roc's builtins
keep_oks = |list, func| {
	var $result = []
	for item in list {
		match func(item) {
			Ok(ok) => {
				$result = $result.append(ok)
			}
			_ => {}
		}
	}
	$result
}

reverse = |list| {
	match list {
		[] => []
		[.. as rest, last] => reverse(rest).append(last)
	}
}
