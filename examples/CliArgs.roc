## Convert native command-line arguments to Unicode text without losing or
## silently replacing invalid operating-system data.
import pf.OsStr exposing [OsStr]
import pf.IOErr exposing [IOErr]
import pf.Stderr

CliArgs := [].{
	decode : List(OsStr) -> Try(List(Str), [InvalidArgumentEncoding({ argument : U64, unit : U64 })])
	decode = |os_args| {
		var $args = []
		for os_arg in os_args {
			argument = $args.len()
			text = match OsStr.to_str_try(os_arg) {
				Ok(value) => value
				Err(InvalidStr(unit)) => return Err(InvalidArgumentEncoding({ argument, unit }))
			}
			$args = $args.append(text)
		}
		Ok($args)
	}

	## Decode argv for an application and report malformed native text as a usage
	## error. Applications can then keep their domain logic entirely in Str.
	to_strs! : List(OsStr) => Try(List(Str), [Exit(I32), StderrErr(IOErr), ..])
	to_strs! = |os_args|
		match decode(os_args) {
			Ok(args) => Ok(args)
			Err(InvalidArgumentEncoding({ argument, unit })) => {
				Stderr.line!(
					"error: command-line argument ${argument.to_str()} is not valid Unicode text (invalid unit at ${unit.to_str()})",
				)?
				Err(Exit(2))
			}
		}
}

expect CliArgs.decode([OsStr.utf8("tool"), OsStr.unix("cafe"), OsStr.windows("world")]) == Ok([
	"tool",
	"cafe",
	"world",
])
expect CliArgs.decode([OsStr.unix_bytes([0xFF])]) == Err(InvalidArgumentEncoding({ argument: 0, unit: 0 }))
expect CliArgs.decode([OsStr.utf8("tool"), OsStr.unix_bytes([0x61, 0xFF])]) == Err(InvalidArgumentEncoding({ argument: 1, unit: 1 }))
expect CliArgs.decode([OsStr.windows_u16s([0xD800])]) == Err(InvalidArgumentEncoding({ argument: 0, unit: 0 }))
