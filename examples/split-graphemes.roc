app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import pf.Stderr
import unicode.Grapheme

default_string = "🇦🇺🦘🪃"

expect Grapheme.split(default_string) == Ok(["🇦🇺", "🦘", "🪃"])

main! = |args| {
	string = match args {
		[] => default_string
		[_app] => default_string
		[_app, arg1, ..] => arg1
	}
	match Grapheme.split(string) {
		Ok(splitted) => {
			Stdout.line!("\n\nThe string \"${string}\" has following graphemes:")
			Stdout.line!(Str.inspect(splitted))
			Ok({})
		}
		Err(_) => {
			Stderr.line!("Error splitting the string.")
			Err(Exit(1))
		}
	}
}
