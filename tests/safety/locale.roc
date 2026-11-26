app [run!] {
	pf: platform "../platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Case
import unicode.Locale

run! : Str => Str
run! = |_| ""

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

expect {
	turkish = Locale.parse("tr-Latn-TR-x-example") ?? crash "valid Turkish locale"
	azerbaijani = Locale.parse("az") ?? crash "valid Azerbaijani locale"
	lithuanian = Locale.parse("lt-LT") ?? crash "valid Lithuanian locale"
	english = Locale.parse("en-u-co-phonebk") ?? crash "valid English locale"
	lower = |locale| {
		result = Case.to_lower("I", { profile: Case.mapping_profile_for_locale(locale), limits: Case.unlimited_limits }) ?? crash "case mapping failed"
		Case.result_text(result)
	}
	Case.mapping_profile_for_locale(lithuanian) == Case.lithuanian
		and Case.mapping_profile_for_locale(Locale.root) == Case.unicode_default
		and lower(turkish) == "ı"
		and lower(azerbaijani) == "ı"
		and lower(english) == "i"
		and Case.full_fold_profile_for_locale(turkish) == Case.turkic_full
		and Case.full_fold_profile_for_locale(lithuanian) == Case.full
}

expect {
	Locale.parse_limited("und", 3) == Ok(Locale.root)
		and Locale.parse_limited("und", 2) == Err(InputByteLimitExceeded({ limit: 2, required: 3 }))
		and Locale.parse_limited("xx-!", 3) == Err(InputByteLimitExceeded({ limit: 3, required: 4 }))
		and Locale.parse_limited("xx-!", 4) == Err(InvalidSubtag("!"))
		and Locale.parse_limited("語", 1) == Err(InputByteLimitExceeded({ limit: 1, required: 3 }))
}

# Omitted case profiles use Unicode default mapping and full default folding.
expect {
	lowered = Case.to_lower("I", { limits: Case.unlimited_limits })?
	folded = Case.fold("Iß", { limits: Case.unlimited_limits })?
	Case.result_text(lowered) == "i" and Case.result_text(folded) == "iss"
}
