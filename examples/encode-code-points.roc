app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
	unicode: "../package/main.roc",
}

import pf.Stderr
import pf.Stdout
import unicode.CodePoint
import unicode.Scalar

parse_scalar : Str -> Try(Scalar, [InvalidHex(Str), OutOfRange(Str), Surrogate(Str)])
parse_scalar = |token| {
	value = U32.from_str("0x${token}") ?? return Err(InvalidHex(token))
	code_point = CodePoint.from_u32(value) ?? return Err(OutOfRange(token))
	match Scalar.from_code_point(code_point) {
		Ok(scalar) => Ok(scalar)
		Err(_) => Err(Surrogate(token))
	}
}

## Encode validated scalars under an application-selected output bound. The
## limit is checked before the append that would cross it, so failure never
## returns a prefix that looks like a complete result.
encode : List(Str), U64 -> Try(Str, [InvalidHex(Str), OutOfRange(Str), Surrogate(Str), LimitExceeded({ limit : U64, required : U64 }), InternalEncodingFault])
encode = |tokens, limit| {
	var $bytes = []
	for token in tokens {
		scalar = match parse_scalar(token) {
			Ok(value) => value
			Err(InvalidHex(value)) => return Err(InvalidHex(value))
			Err(OutOfRange(value)) => return Err(OutOfRange(value))
			Err(Surrogate(value)) => return Err(Surrogate(value))
		}
		match Scalar.append_utf8($bytes, scalar, limit) {
			Err(OutputLimitExceeded(details)) => return Err(LimitExceeded(details))
			Ok(next) => {
				$bytes = next
			}
		}
	}
	match Str.from_utf8($bytes) {
		Ok(text) => Ok(text)
		Err(_) => Err(InternalEncodingFault)
	}
}

error_message = |error| match error {
	InvalidHex(token) => "${Str.inspect(token)} is not hexadecimal"
	OutOfRange(token) => "${Str.inspect(token)} is above U+10FFFF"
	Surrogate(token) => "${Str.inspect(token)} is a surrogate code point, not a Unicode scalar"
	LimitExceeded({ limit, required }) => {
		"output limit ${limit.to_str()} bytes would be exceeded; ${required.to_str()} bytes are required"
	}
	InternalEncodingFault => "validated scalar encoding unexpectedly produced invalid UTF-8"
}

expect encode(["41", "E9", "1F998"], 7) == Ok("Aé🦘")
expect encode(["D800"], 4) == Err(Surrogate("D800"))
expect encode(["110000"], 4) == Err(OutOfRange("110000"))
expect encode(["1F998"], 3) == Err(LimitExceeded({ limit: 3, required: 4 }))

main! : List(Str) => Try({}, [Exit(I32), StderrErr(Str), StdoutErr(Str), ..])
main! = |args| {
	user_args = args.drop_first(1)
	(limit_text, tokens) = match user_args {
		[limit_arg, first, .. as rest] => (limit_arg, [first].concat(rest))
		_ => {
			Stderr.line!("usage: encode-code-points MAX_OUTPUT_BYTES HEX_CODE_POINT [HEX_CODE_POINT ...]")?
			return Err(Exit(2))
		}
	}
	limit = U64.from_str(limit_text) ?? {
		Stderr.line!("error: MAX_OUTPUT_BYTES must be a non-negative integer")?
		return Err(Exit(2))
	}
	match encode(tokens, limit) {
		Ok(text) => Stdout.line!(
			\\code-points: ${Str.join_with(tokens, " ")}
			\\utf8-bytes: ${text.count_utf8_bytes().to_str()}
			\\text: ${text}
			,
		)?
		Err(problem) => {
			Stderr.line!("error: ${error_message(problem)}")?
			return Err(Exit(1))
		}
	}
	Ok({})
}
