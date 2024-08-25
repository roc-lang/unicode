app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.11.0/SY4WWMhWQ9NvQgvIthcv15AUeA7rAIJHAHgiaSHGhdY.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import pf.Task
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
