Language : {
	language : Str,
	script : [Some(Str), None],
	region : [Some(Str), None],
	variants : List(Str),
}

Field : { key : Str, value : List(Str) }

Extension : { singleton : Str, parts : List(Str) }

## An explicit, well-formed Unicode BCP 47 locale identifier, following
## UTS #35 Part 1. Parsing checks syntax and case-insensitive uniqueness.
## Catalog-dependent aliases, availability, and fallback belong to consumers
## with pinned locale data. No operation reads the system locale.
Locale :: {
	language : Language,
	extensions : List(Extension),
	private_use : List(Str),
	keywords : List(Field),
}.{
	Error : [
		InvalidSubtag(Str),
		InvalidLanguage,
		MisplacedSubtag(Str),
		DuplicateVariant(Str),
		DuplicateExtension(Str),
		MissingExtensionValue(Str),
		InvalidExtensionKey(Str),
		DuplicateExtensionAttribute(Str),
		DuplicateExtensionKey(Str),
	]

	## The Unicode root locale, serialized as the BCP 47 identifier `und`.
	root : Locale
	root = {
		language: { language: "und", script: None, region: None, variants: [] },
		extensions: [],
		private_use: [],
		keywords: [],
	}

	## Parse Unicode BCP 47 syntax. Subtags use ASCII letters and digits
	## separated by hyphens. Catalog entries and supported policies are checked
	## separately by consumers of this syntactic identifier.
	parse : Str -> Try(Locale, Error)
	parse = |identifier| {
		parts = identifier.split_on("-")
		for part in parts {
			if !length_in(part, 1, 8) or !part.to_utf8().all(alphanum) {
				return Err(InvalidSubtag(part))
			}
		}
		parsed = parse_language(parts.map(Str.with_ascii_lowercased))?
		var $rest = parsed.rest
		var $seen = Set.empty()
		var $extensions = []
		var $keywords = []
		var $private_use = []
		while !$rest.is_empty() {
			match $rest {
				[singleton, .. as tail] => {
					if singleton.count_utf8_bytes() != 1 {
						return Err(MisplacedSubtag(singleton))
					}
					if tail.is_empty() {
						return Err(MissingExtensionValue(singleton))
					}
					if singleton == "x" {
						$private_use = tail
						break
					}
					if $seen.contains(singleton) {
						return Err(DuplicateExtension(singleton))
					}
					$seen = $seen.insert(singleton)
					segment = take_matching(tail, |subtag| subtag.count_utf8_bytes() >= 2)
					if segment.parts.is_empty() {
						return Err(MissingExtensionValue(singleton))
					}
					normalized = match singleton {
						"u" => {
							extension = parse_unicode_extension(segment.parts)?
							$keywords = extension.fields
							extension.parts
						}
						"t" => parse_transformed_extension(segment.parts)?
						_ => segment.parts
					}
					$extensions = $extensions.append({ singleton, parts: normalized })
					$rest = segment.rest
				}
				[] => break
			}
		}
		Ok({
			language: parsed.language,
			extensions: $extensions.sort_by(|extension| lexical_key(extension.singleton)),
			private_use: $private_use,
			keywords: $keywords,
		})
	}

	## Format in consistent BCP 47 casing. Variants, singleton extensions, and
	## Unicode extension attributes and keys use lexical order. Aliases and
	## likely subtags are resolved separately by consumers using locale data.
	to_str : Locale -> Str
	to_str = |locale| {
		base = language_parts(locale.language, Bool.True)
		with_extensions = locale.extensions.fold(
			base,
			|parts, extension|
				parts.append(extension.singleton).concat(extension.parts),
		)
		all = if locale.private_use.is_empty() {
			with_extensions
		} else {
			with_extensions.append("x").concat(locale.private_use)
		}
		Str.join_with(all, "-")
	}

	language : Locale -> Str
	language = |locale| locale.language.language

	script : Locale -> [Some(Str), None]
	script = |locale| option_map(locale.language.script, title_ascii)

	region : Locale -> [Some(Str), None]
	region = |locale| option_map(locale.language.region, Str.with_ascii_uppercased)

	variants : Locale -> List(Str)
	variants = |locale| locale.language.variants

	## Find a Unicode `u` extension key using ASCII case-insensitive lookup.
	## `Some([])` is a present keyword with the implicit type `true`.
	unicode_keyword : Locale, Str -> [Some(List(Str)), None]
	unicode_keyword = |locale, key| {
		for field in locale.keywords {
			if field.key.caseless_ascii_equals(key) {
				return Some(field.value)
			}
		}
		None
	}

	## Compare identifiers after syntax normalization.
	## Catalog aliases and likely subtags have separate semantics.
	is_eq : Locale, Locale -> Bool
	is_eq = |left, right| left.language == right.language and left.extensions == right.extensions and left.private_use == right.private_use
}

parse_language : List(Str) -> Try({ language : Language, rest : List(Str) }, Locale.Error)
parse_language = |parts| match parts {
	[language, .. as tail] if is_language(language) => {
		(script, after_script) = match tail {
			[maybe_script, .. as rest] if is_script(maybe_script) => (Some(maybe_script), rest)
			_ => (None, tail)
		}
		(region, after_region) = match after_script {
			[maybe_region, .. as rest] if is_region(maybe_region) => (Some(maybe_region), rest)
			_ => (None, after_script)
		}
		result = take_matching(after_region, is_variant)
		match first_duplicate(result.parts) {
			Some(variant) => Err(DuplicateVariant(variant))
			None => Ok({ language: { language, script, region, variants: result.parts.sort_by(lexical_key) }, rest: result.rest })
		}
	}
	_ => Err(InvalidLanguage)
}

parse_unicode_extension : List(Str) -> Try({ parts : List(Str), fields : List(Field) }, Locale.Error)
parse_unicode_extension = |parts| {
	attributes = take_matching(parts, is_field_value)
	match first_duplicate(attributes.parts) {
		Some(attribute) => return Err(DuplicateExtensionAttribute(attribute))
		None => {}
	}
	fields = parse_fields(attributes.rest, Bool.False)?
	Ok({ parts: attributes.parts.sort_by(lexical_key).concat(field_parts(fields)), fields })
}

parse_transformed_extension : List(Str) -> Try(List(Str), Locale.Error)
parse_transformed_extension = |parts| {
	(first, field_parts_to_parse) = match parts {
		[key, ..] if is_transformed_key(key) => ([], parts)
		_ => {
			parsed = parse_language(parts)?
			(language_parts(parsed.language, Bool.False), parsed.rest)
		}
	}
	fields = parse_fields(field_parts_to_parse, Bool.True)?
	Ok(first.concat(field_parts(fields)))
}

parse_fields : List(Str), Bool -> Try(List(Field), Locale.Error)
parse_fields = |parts, transformed| {
	var $rest = parts
	var $seen = Set.empty()
	var $fields = []
	while !$rest.is_empty() {
		match $rest {
			[key, .. as tail] => {
				valid = if (transformed) is_transformed_key(key) else is_unicode_key(key)
				if !valid {
					return Err(InvalidExtensionKey(key))
				}
				if $seen.contains(key) {
					return Err(DuplicateExtensionKey(key))
				}
				typed = take_matching(tail, is_field_value)
				if transformed and typed.parts.is_empty() {
					return Err(MissingExtensionValue(key))
				}
				value = if (!transformed and typed.parts == ["true"]) [] else typed.parts
				$seen = $seen.insert(key)
				$fields = $fields.append({ key, value })
				$rest = typed.rest
			}
			[] => break
		}
	}
	Ok($fields.sort_by(|field| lexical_key(field.key)))
}

field_parts : List(Field) -> List(Str)
field_parts = |fields| fields.fold([], |parts, field| parts.append(field.key).concat(field.value))

language_parts : Language, Bool -> List(Str)
language_parts = |value, core_case| {
	script = option_list(if (core_case) option_map(value.script, title_ascii) else value.script)
	region = option_list(if (core_case) option_map(value.region, Str.with_ascii_uppercased) else value.region)
	[value.language].concat(script).concat(region).concat(value.variants)
}

option_map : [Some(a), None], (a -> b) -> [Some(b), None]
option_map = |value, change| match value {
	Some(item) => Some(change(item))
	None => None
}

option_list : [Some(a), None] -> List(a)
option_list = |value| match value {
	Some(item) => [item]
	None => []
}

take_matching : List(Str), (Str -> Bool) -> { parts : List(Str), rest : List(Str) }
take_matching = |input, accepts| {
	var $count = 0
	for part in input {
		if !accepts(part) {
			break
		}
		$count = $count + 1
	}
	{ parts: input.take_first($count), rest: input.drop_first($count) }
}

first_duplicate : List(Str) -> [Some(Str), None]
first_duplicate = |items| {
	var $seen = Set.empty()
	for item in items {
		if $seen.contains(item) {
			return Some(item)
		}
		$seen = $seen.insert(item)
	}
	None
}

# Valid subtags have one through eight nonzero ASCII bytes. A big-endian
# zero-padded integer preserves byte order, including a prefix before a
# longer subtag that starts with the same bytes.
lexical_key : Str -> U64
lexical_key = |value| {
	bytes = value.to_utf8()
	packed = bytes.fold(0.U64, |key, byte| key.shl_wrap(8).bitwise_or(byte.to_u64()))
	packed.shl_wrap(((8 - bytes.len()) * 8).to_u8_wrap())
}

title_ascii : Str -> Str
title_ascii = |value| {
	# The grammar has already checked that script subtags have four ASCII letters.
	bytes = value.to_utf8()
	match bytes {
		[first, .. as tail] if first >= 'a' and first <= 'z' => Str.from_utf8([first - 0x20].concat(tail)) ?? value
		_ => value
	}
}

alpha : U8 -> Bool
alpha = |byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z')

digit : U8 -> Bool
digit = |byte| byte >= '0' and byte <= '9'

alphanum : U8 -> Bool
alphanum = |byte| alpha(byte) or digit(byte)

length_in : Str, U64, U64 -> Bool
length_in = |value, low, high| {
	length = value.count_utf8_bytes()
	length >= low and length <= high
}

is_language : Str -> Bool
is_language = |value| (length_in(value, 2, 3) or length_in(value, 5, 8)) and value.to_utf8().all(alpha)

is_script : Str -> Bool
is_script = |value| value.count_utf8_bytes() == 4 and value.to_utf8().all(alpha)

is_region : Str -> Bool
is_region = |value| {
	bytes = value.to_utf8()
	(bytes.len() == 2 and bytes.all(alpha)) or (bytes.len() == 3 and bytes.all(digit))
}

is_variant : Str -> Bool
is_variant = |value| {
	length_in(value, 5, 8) or match value.to_utf8() {
		[first, _, _, _] => digit(first)
		_ => Bool.False
	}
}

is_unicode_key : Str -> Bool
is_unicode_key = |value| match value.to_utf8() {
	[_, second] => alpha(second)
	_ => Bool.False
}

is_transformed_key : Str -> Bool
is_transformed_key = |value| match value.to_utf8() {
	[first, second] => alpha(first) and digit(second)
	_ => Bool.False
}

is_field_value : Str -> Bool
is_field_value = |value| value.count_utf8_bytes() >= 3

## UTS #35 Part 1's syntax-normalization example. Attributes, extension keys
## and the implicit true type can each change the resulting serialization.
expect {
	source = "en-u-foo-bar-nu-thai-ca-buddhist-kk-true"
	expected = "en-u-bar-foo-ca-buddhist-kk-nu-thai"
	locale = Locale.parse(source) ?? crash "valid Unicode extension was rejected"
	round_trip = Locale.parse(expected) ?? crash "formatted Unicode extension was rejected"
	locale == round_trip
		and Locale.to_str(locale) == expected
		and Locale.unicode_keyword(locale, "KK") == Some([])
		and Locale.unicode_keyword(locale, "Nu") == Some(["thai"])
		and Locale.unicode_keyword(locale, "co") == None
}

expect {
	input = "ZH-hANT-tW-Scouse-FONIPA-U-CO-PHONEBK-Z-NINE-T-es-latn-419-M0-UNGEGN-X-foo-B"
	locale = Locale.parse(input) ?? crash "valid components and extensions were rejected"
	Locale.to_str(locale) == "zh-Hant-TW-fonipa-scouse-t-es-latn-419-m0-ungegn-u-co-phonebk-z-nine-x-foo-b"
		and Locale.language(locale) == "zh"
		and Locale.script(locale) == Some("Hant")
		and Locale.region(locale) == Some("TW")
		and Locale.variants(locale) == ["fonipa", "scouse"]
}

expect {
	locale = Locale.parse("ES-419-u-ca-islamic-civil") ?? crash "valid numeric region or multi-part type was rejected"
	Locale.region(locale) == Some("419")
		and Locale.unicode_keyword(locale, "ca") == Some(["islamic", "civil"])
		and Locale.parse("und") == Ok(Locale.root)
		and Locale.to_str(Locale.root) == "und"
}

expect {
	Locale.parse("sr_Latn") == Err(InvalidSubtag("sr_Latn"))
		and Locale.parse("EN-u-co-phoné") == Err(InvalidSubtag("phoné"))
		and Locale.parse("en-") == Err(InvalidSubtag(""))
		and Locale.parse("i-default") == Err(InvalidLanguage)
		and Locale.parse("en-fonipa-FONIPA") == Err(DuplicateVariant("fonipa"))
		and Locale.parse("root") == Err(InvalidLanguage)
}

expect {
	Locale.parse("en-u") == Err(MissingExtensionValue("u"))
		and Locale.parse("en-t-0a-foo") == Err(InvalidLanguage)
		and Locale.parse("en-t-m0") == Err(MissingExtensionValue("m0"))
		and Locale.parse("en-t-m0-foo-M0-bar") == Err(DuplicateExtensionKey("m0"))
		and Locale.parse("en-u-m0") == Err(InvalidExtensionKey("m0"))
		and Locale.parse("en-u-foo-Foo") == Err(DuplicateExtensionAttribute("foo"))
		and Locale.parse("en-u-co-phonebk-CO-standard") == Err(DuplicateExtensionKey("co"))
		and Locale.parse("en-u-co-phonebk-U-ca-gregory") == Err(DuplicateExtension("u"))
		and Locale.parse("en-x") == Err(MissingExtensionValue("x"))
}

expect {
	locale = Locale.parse("EN-u-CO-x-Private") ?? crash "boolean key or private use rejected"
	Locale.to_str(locale) == "en-u-co-x-private"
		and Locale.unicode_keyword(locale, "co") == Some([])
}
