app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import pf.Stderr
import unicode.CodePoint exposing [Utf8ParseErr]

## Get the number of code points for a given Str
nr_of_code_points : Str -> Try(U64, Utf8ParseErr)
nr_of_code_points = |str| {
	str.to_utf8()->CodePoint.parse_utf8().map_ok(List.len)
}

main! = |_args| {
	word = "ẇ͓̞͒͟͡ǫ̠̠̉̏͠͡ͅr̬̺͚̍͛̔͒͢d̠͎̗̳͇͆̋̊͂͐"

	match nr_of_code_points(word) {
		Ok(nr) => {
			Stdout.line!("String \"${word}\" consists of ${nr.to_str()} code points.")
			Ok({})
		}

		Err(_) => {
			Stderr.line!("Failed to parse string ${word} as Utf8.")
			Err(Exit(1))
		}
	}
}
