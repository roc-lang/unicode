app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import pf.Task exposing [Task]
import unicode.Grapheme

string = "🇦🇺🦘🪃"

expect Grapheme.split string == Ok ["🇦🇺", "🦘", "🪃"]

main =
    string
        |> Grapheme.split
        |> Inspect.toStr
        |> \splitted ->
            Stdout.line! "\n\nThe string \"$(string)\" has following graphemes:"
            Stdout.line! splitted
