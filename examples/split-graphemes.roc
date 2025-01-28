app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
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
