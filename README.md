Convenient functions for working with unicode.

⚠️ This package has only gone through limited testing. [Make an issue](https://github.com/roc-lang/unicode/issues) when you hit a bug.

:eyes: [**examples**](https://github.com/roc-lang/unicode/tree/main/examples)

:book: [**documentation**](https://roc-lang.github.io/unicode)


## Learning about Unicode

The string/unicode rabbit hole goes deep, we have a [good overview](https://www.roc-lang.org/builtins/Str) (scroll to the unicode section).

## Unicode data

Production tables and conformance tests are generated from the official,
versioned Unicode files in `vendor/unicode/15.1.0/`.
`vendor/unicode/manifest.json` pins their source URLs, SHA-256 hashes, header
markers, and record counts. The repository keeps Unicode 15.1 behavior pinned
until an implementation change explicitly updates the manifest and generated
modules.

Validate the sources and check generated Roc modules with:

```sh
python3 scripts/unicode_data.py validate
python3 scripts/unicode_data.py generate --check
```

Regenerate them with `scripts/rebuild.sh`.

## Tests

Each purpose-built test app lives in `tests/apps/<suite>/main.roc` with an
adjacent `spec.json`. The runner builds optimized binaries once, sends cases
over a versioned stdin protocol, and executes deterministic shards in parallel.

```sh
ROC=roc scripts/all_tests.sh
python3 scripts/test.py grapheme
python3 scripts/test.py properties --jobs 8
python3 scripts/test.py allocations
```

The suites cover the 1,062 currently supported official grapheme cases, every
valid Unicode scalar for grapheme-break/East-Asian-width/emoji properties, and
allocation calibration on every host. The 125 unsupported grapheme cases are
explicitly tracked against issue #35; exact allocation snapshots are pinned for
optimized Linux x64 builds.
