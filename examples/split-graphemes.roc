app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.Grapheme

string = "🇦🇺🦘🪃"

expect Grapheme.split(string) == Ok(["🇦🇺", "🦘", "🪃"])

main! = |_args|
    string
    |> Grapheme.split
    |> Inspect.to_str
    |> |splitted|
        Stdout.line!("\n\nThe string \"${string}\" has following graphemes:")?
        Stdout.line!(splitted)
