app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.CodePoint

default_word = "世界"

## Get the display width (in amount of characters) of a Str
get_visual_width : Str -> Try(U32, CodePoint.Utf8ParseErr)
get_visual_width = |str| {
    str
        .to_utf8()
        ->CodePoint.parse_utf8()?
        .map(CodePoint.visual_width)
        .sum()
        ->Ok()
}

main! = |args| {
    word = match args {
        [] => default_word
        [_app] => default_word
        [_app, arg1, ..] => arg1
    }
    match get_visual_width(word) {
        Ok(width) => {
            Stdout.line!("\n\nThe word ${word} will be displayed with the width of ${width.to_str()} characters on most UIs.\n\n")
            Ok({})
        }
        Err(_) => {
            crash "ERROR: Unable to parse ${word}!"
        }
    }
}

expect get_visual_width(default_word) == Ok(4)
