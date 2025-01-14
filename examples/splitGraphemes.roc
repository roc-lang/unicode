app [main!] {
    pf: platform "../../basic-cli/platform/main.roc",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.Grapheme

string = "🇦🇺🦘🪃"

expect Grapheme.split(string) == Ok(["🇦🇺", "🦘", "🪃"])

main! = \_args ->
    string
    |> Grapheme.split
    |> Inspect.to_str
    |> \splitted ->
        Stdout.line!("\n\nThe string \"${string}\" has following graphemes:")?
        Stdout.line!(splitted)
