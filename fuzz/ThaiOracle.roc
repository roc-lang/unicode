import unicode.TextPosition
import unicode.ThaiLineBreak

Token : { text : Str, length : U64, known : Bool }

Path : { unknown : U64, tokens : List(Token) }

ThaiOracle := [].{

	## Small independent path enumeration. The caller supplies authored
	## graphemes and limits their count to six.
	expected : List(Str), List(Str) -> List(TextPosition)
	expected = |clusters, lexicon| {
		count = clusters.len()
		var $paths = 1.U64
		if count > 1 {
			$paths = (1.U64).shl_wrap((count - 1).to_u8_wrap())
		}
		var $mask = 0.U64
		var $best = { unknown: U64.highest, tokens: [] }
		while $mask < $paths {
			match read_path(clusters, lexicon, $mask) {
				Some(path) => {
					if prefer(path, $best) {
						$best = path
					}
				}
				None => {}
			}
			$mask = $mask + 1
		}
		var $positions = []
		var $bytes = 0.U64
		var $scalars = 0.U64
		var $previous_known = Bool.False
		for token in $best.tokens {
			if $previous_known and token.known {
				$positions = $positions.append(TextPosition.from_offsets($bytes, $scalars))
			}
			$bytes = $bytes + token.text.count_utf8_bytes()
			# The authored alphabet uses only Thai 3-byte scalars.
			$scalars = $scalars + token.text.count_utf8_bytes() / 3
			$previous_known = token.known
		}
		$positions.append(TextPosition.from_offsets($bytes, $scalars))
	}

	agrees : List(Str), List(Str) -> Bool
	agrees = |clusters, lexicon| {
		source = Str.join_with(clusters, "")
		index = ThaiOracle.dictionary(lexicon)
		limits = { max_input_bytes: source.count_utf8_bytes(), max_run_bytes: source.count_utf8_bytes() }
		match ThaiLineBreak.opportunities(source, index, limits) {
			Err(_) => Bool.False
			Ok(opportunities) => {
				opportunities.map(|event| event.at) == ThaiOracle.expected(clusters, lexicon) and
					opportunities.last().map_ok(|event| event.decision) == Ok(Mandatory)
			}
		}
	}

	dictionary : List(Str) -> ThaiLineBreak.Dictionary
	dictionary = |lexicon| {
		ThaiLineBreak.Dictionary.from_words(
			lexicon,
			{ revision: "authored testing lexicon", limits: { max_words: 16, max_total_bytes: 256, max_word_bytes: 32 } },
		) ?? crash "test dictionary was rejected"
	}
}

read_path : List(Str), List(Str), U64 -> [None, Some(Path)]
read_path = |clusters, lexicon, mask| {
	var $tokens = []
	var $unknown = 0.U64
	var $chunk = ""
	var $length = 0.U64
	var $index = 0.U64
	for part in clusters {
		$chunk = Str.concat($chunk, part)
		$length = $length + 1
		$index = $index + 1
		if $index == clusters.len() or mask.bitwise_and((1.U64).shl_wrap(($index - 1).to_u8_wrap())) > 0 {
			known = lexicon.contains($chunk)
			if !known {
				if $length != 1 {
					return None
				}
				$unknown = $unknown + 1
			}
			$tokens = $tokens.append({ text: $chunk, length: $length, known })
			$chunk = ""
			$length = 0
		}
	}
	Some({ unknown: $unknown, tokens: $tokens })
}

prefer : Path, Path -> Bool
prefer = |candidate, previous| {
	if candidate.unknown != previous.unknown {
		return candidate.unknown < previous.unknown
	}
	if candidate.tokens.len() != previous.tokens.len() {
		return candidate.tokens.len() < previous.tokens.len()
	}
	var $index = 0.U64
	for token in candidate.tokens {
		other = previous.tokens.get($index) ?? crash "equal path lengths were not equal"
		if token.length != other.length {
			return token.length > other.length
		}
		$index = $index + 1
	}
	Bool.False
}
