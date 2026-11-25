# Unicode collation

Construct a `Collation.Collator` from an explicit `Locale` and
`Collation.Options`. The default options compare primary weights, accents,
and case or variants, with punctuation significant. Select `Shifted` to move
variable elements to the quaternary level. Select `Identical` for an additional
NFD scalar tie-break, which preserves canonical equivalence. Numeric ordering
compares runs of Unicode decimal digits by value without converting to a
machine integer. `case_first` explicitly controls tertiary case order.

The supported locale identifiers and aliases are listed below. Selection
returns `UnsupportedLocale` for other identifiers. Supply strength, alternate
handling, numeric order, and case order through the options record.

| Identifier | Ordering |
| --- | --- |
| `und`, `und-u-co-standard` | Unicode 18 DUCET |
| `de`, `de-u-co-standard` | CLDR 49 German standard |
| `de-u-co-phonebk` | CLDR 49 German phonebook |
| `es`, `es-u-co-standard` | CLDR 49 Spanish standard |
| `es-u-co-trad` | CLDR 49 Spanish traditional |

`Collation.compare`, `sort_key`, and `sort` agree on order. Compare keys
bytewise under the same data versions and options. Key bytes belong to this
encoding revision and can change on upgrade. `sort` orders collation-equal
strings stably and returns the original strings. `UnicodeVersion.current`,
`Collation.cldr_version`, and `Collation.profile_revision` identify the
independent release and policy axes.

The algorithm uses the complete-source NFD transformation, a bounded scalar
tape, the longest contiguous and applicable discontiguous contraction, and
implicit UCA weights for scalars without explicit mappings. Set the
normalization, collation-element, numeric-run and key-byte limits for each
input. A stable sort also needs input-item and total-key-byte limits. A limit
failure returns no key or partially sorted list.

The test driver checks adjacent scalar-only rows in both official UCA short
files at identical strength. The CLDR lane compares independently recorded
ICU4J 79.1 results for the three tailored profiles and checks stable sort and
limit behavior. ICU caps the precision of a single numeric run at 254
significant digits and can select a radical-stroke Han ordering. The recorded
differential inputs use the matched policy domains. Mutating the equal-class
blocking condition changes the Cyrillic contraction result and fails its
deterministic oracle.

The full source archive, specification revisions, and selected profiles share
the package's [manifest](../vendor/unicode/manifest.json). See
[UTS #10 revision 55](https://www.unicode.org/reports/tr10/tr10-55.html) and
[UTS #35 revision 79, Collation](https://www.unicode.org/reports/tr35/tr35-79/tr35-collation.html).
