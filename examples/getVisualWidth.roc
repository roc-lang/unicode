app [main!] {
    pf: platform "../../basic-cli/platform/main.roc",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.CodePoint

word = "世界"

## Get the display width (in amount of characters) of a Str
get_visual_width : Str -> Result U32 CodePoint.Utf8ParseErr
get_visual_width = \str ->
    str
    |> Str.to_utf8
    |> CodePoint.parse_utf8
    |> Result.map(\lst -> List.map(lst, CodePoint.visual_width))
    |> Result.map(List.sum)

main! = \_args ->
    when get_visual_width(word) is
        Ok(width) -> Stdout.line!("\n\nThe word ${word} will be displayed with the width of ${Num.to_str(width)} characters on most UIs.\n\n")
        Err(_) -> crash("ERROR: Unable to parse ${word}!")

expect (get_visual_width(word)) == Ok(4)