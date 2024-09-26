app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.CodePoint

word = "世界"

## Get the display width (in amount of characters) of a Str
getVisualWidth : Str -> Result U32 CodePoint.Utf8ParseErr
getVisualWidth = \str ->
    str
    |> Str.toUtf8
    |> CodePoint.parseUtf8
    |> Result.map (\lst -> List.map lst CodePoint.visualWidth)
    |> Result.map List.sum

main =
    when getVisualWidth word is
        Ok width -> Stdout.line "\n\nThe word $(word) will be displayed with the width of $(Num.toStr width) characters on most UIs.\n\n"
        Err _ -> crash "ERROR: Unable to parse $(word)!"

expect (getVisualWidth word) == Ok 4
