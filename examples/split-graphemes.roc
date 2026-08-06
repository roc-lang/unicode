app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.ByteRange
import unicode.Grapheme
import unicode.UnicodeVersion

default_string = "🇦🇺🦘🪃"

expect Grapheme.owned(default_string) == ["🇦🇺", "🦘", "🪃"]

expect {
    Grapheme.ranges(default_string).map(|range| {
        (ByteRange.start(range), ByteRange.end(range))
    }) == [(0, 8), (8, 12), (12, 16)]
}

append_bounds = |bounds, range| {
    bounds.append((ByteRange.start(range), ByteRange.end(range)))
}

expect {
    first = Grapheme.Cursor.push(
        Grapheme.Cursor.init({}),
        "🇦",
        [],
        append_bounds,
    )

    match first {
        Err(_) => Bool.False
        Ok({ cursor: after_first, state: first_bounds }) => {
            second = Grapheme.Cursor.push(after_first, "🇺x", first_bounds, append_bounds)

            match second {
                Err(_) => Bool.False
                Ok({ cursor: after_second, state: second_bounds }) => {
                    match Grapheme.Cursor.finish(after_second, second_bounds, append_bounds) {
                        Err(_) => Bool.False
                        Ok({ state: final_bounds, .. }) => final_bounds == [(0, 8), (8, 9)]
                    }
                }
            }
        }
    }
}

expect UnicodeVersion.to_str(UnicodeVersion.current) == "17.0.0"

expect Grapheme.slices(default_string) == ["🇦🇺", "🦘", "🪃"]

expect {
    Grapheme.iter_ranges(default_string).fold([], |bounds, range| {
        bounds.append((ByteRange.start(range), ByteRange.end(range)))
    }) == [(0, 8), (8, 12), (12, 16)]
}

main! = |args| {
    string = match args {
        [] => default_string
        [_app] => default_string
        [_app, arg1, ..] => arg1
    }
    graphemes = Grapheme.owned(string)
    Stdout.line!("\n\nThe string \"${string}\" has following graphemes:")?
    Stdout.line!(Str.inspect(graphemes))?
    Ok({})
}
