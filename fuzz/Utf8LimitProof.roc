import fuzz.Solve
import unicode.Scalar

Utf8LimitProof := [].{}

## Model `Scalar.append_utf8`'s width-bounded checked addition. In the
## independent law, order the arguments before subtracting to leave a
## representable available capacity. The proof spans all U8 lengths and
## limits and exactly the four valid encoded widths.
expect Solve.prove_within(
	{ count: Solve.u8, width: Solve.u8_in(1, 4), limit: Solve.u8 }.Solve,
	|{ count, width, limit }| {
		fits = Solve.all([count.plus_overflows(width).not(), (count + width).lte(limit)])
		room = Solve.all([count.lte(limit), width.lte(limit - count)])
		fits.iff(room)
	},
	2000,
) == Ok(Proved)

## Produce a witness whose four-byte scalar exactly fills a nonempty buffer's
## remaining capacity. Exercise the actual caller on both sides of that limit.
expect match Solve.find_within(
	{ count: Solve.u64_in(1, 8), limit: Solve.u64_in(0, 12) }.Solve,
	|{ count, limit }| (count + Solve.num(4)).eq(limit),
	200,
) {
	Ok(Found({ count, limit })) => {
		scalar = Scalar.from_u32(0x10FFFF) ?? crash "invalid proof fixture scalar"
		prefix = List.repeat(0x41.U8, count.value())
		width = [0xF4, 0x8F, 0xBF, 0xBF]
		accepted = Scalar.append_utf8(prefix, scalar, limit.value())
		too_small = limit.value() - 1
		rejected = Scalar.append_utf8(prefix, scalar, too_small)
		accepted == Ok(prefix.concat(width)) and rejected == Err(OutputLimitExceeded({ limit: too_small, required: limit.value() }))
	}
	_ => Bool.False
}
