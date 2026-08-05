app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.Scalar

default_word = "世界"

## An application policy that treats Unicode East_Asian_Width Fullwidth and
## Wide scalars as two cells, and all others as one. This is not a universal
## terminal or glyph width algorithm.
get_visual_width : Str -> U32
get_visual_width = |str| {
    var $width = 0.U32
    for located in Scalar.iter(str) {
        $width = $width + match Scalar.east_asian_width(located.scalar) {
            Fullwidth => 2
            Wide => 2
            _ => 1
        }
    }
    $width
}

main! = |args| {
    word = match args {
        [] => default_word
        [_app] => default_word
        [_app, arg1, ..] => arg1
    }
    width = get_visual_width(word)
    Stdout.line!("\n\nThe word ${word} will be displayed with the width of ${width.to_str()} characters on most UIs.\n\n")?
    Ok({})
}

expect get_visual_width(default_word) == 4
