## GENERATED from vendor/unicode/manifest.json. Run `python3 scripts/unicode_data.py generate`. ##

## The Unicode data and algorithm semantics implemented by this package.
UnicodeVersion :: { major : U16, minor : U16, patch : U16 }.{
    current : UnicodeVersion
    current = { major: 17, minor: 0, patch: 0 }

    major : UnicodeVersion -> U16
    major = |version_value| version_value.major

    minor : UnicodeVersion -> U16
    minor = |version_value| version_value.minor

    patch : UnicodeVersion -> U16
    patch = |version_value| version_value.patch

    to_str : UnicodeVersion -> Str
    to_str = |_| "17.0.0"

    is_eq : UnicodeVersion, UnicodeVersion -> Bool
    is_eq = |left, right| {
        left.major == right.major and left.minor == right.minor and left.patch == right.patch
    }
}
