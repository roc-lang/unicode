app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.Text

default_string = "🇦🇺🦘🪃"

expect Text.Grapheme.owned(default_string) == ["🇦🇺", "🦘", "🪃"]

expect {
    Text.Grapheme.ranges(default_string).map(|range| {
        (Text.ByteRange.start(range), Text.ByteRange.end(range))
    }) == [(0, 8), (8, 12), (12, 16)]
}

expect Text.Grapheme.slices(default_string) == ["🇦🇺", "🦘", "🪃"]

expect {
    Text.Grapheme.iter_ranges(default_string).fold([], |bounds, range| {
        bounds.append((Text.ByteRange.start(range), Text.ByteRange.end(range)))
    }) == [(0, 8), (8, 12), (12, 16)]
}

main! = |args| {
    string = match args {
        [] => default_string
        [_app] => default_string
        [_app, arg1, ..] => arg1
    }
    graphemes = Text.Grapheme.owned(string)
    Stdout.line!("\n\nThe string \"${string}\" has following graphemes:")?
    Stdout.line!(Str.inspect(graphemes))?
    Ok({})
}
