import fuzz.Fuzz
import fuzz.Model
import unicode.ByteRange
import unicode.Scalar
import unicode.Utf8

Action : [Empty, Ascii, Lead, Tail, Finish]

Item : { value : U32, start : U64, end : U64, index : U64 }

Status : [Open, Pending, Complete, Broken]

Observation : [
	Push(U64, List(Item)),
	End(U64, U64),
	Bad(Utf8.Problem, U64, U64),
	AlreadyEnded,
	AlreadyBroken,
]

World : {
	history : List(Action),
	status : Status,
	offset : U64,
	index : U64,
	agrees : Bool,
}

## A tiny independent decoder for just `A` and the two-byte encoding of ¢.
## At most four actions cover a lead/tail split, an empty chunk inside it,
## premature finish and pushes after both kinds of termination.
machine : Model.Machine(World, Action)
machine = {
	init: { history: [], status: Open, offset: 0, index: 0, agrees: Bool.True },
	actions: |world| if (world.history.len() < 4) [Empty, Ascii, Lead, Tail, Finish] else [],
	step,
	rules: [
		Model.always("incremental output, consumption, and diagnostics match the tiny decoder", |world| world.agrees),
		Model.sometimes("the second byte arrives after an empty chunk", |world| world.history == [Lead, Empty, Tail, Finish]),
		Model.sometimes("a truncated stream rejects a subsequent push", |world| world.history == [Lead, Finish, Tail]),
		Model.sometimes("a finished stream rejects a subsequent push", |world| world.history == [Ascii, Finish, Ascii]),
	],
}

step : World, Action -> World
step = |world, action| {
	(status, offset, index, expected) = reference(world, action)
	cursor = world.history.fold(
		Utf8.Cursor.init({}),
		|prior, operation| {
			(next, _) = observe(prior, operation)
			next
		},
	)
	(_, observed) = observe(cursor, action)
	{
		history: world.history.append(action),
		status,
		offset,
		index,
		agrees: world.agrees and expected == observed,
	}
}

reference : World, Action -> (Status, U64, U64, Observation)
reference = |{ status, offset, index, .. }, action| match (status, action) {
	(Complete, _) => (Complete, offset, index, AlreadyEnded)
	(Broken, _) => (Broken, offset, index, AlreadyBroken)
	(_, Empty) => (status, offset, index, Push(0, []))
	(Open, Ascii) => (Open, offset + 1, index + 1, Push(1, [{ value: 0x41, start: offset, end: offset + 1, index }]))
	(Open, Lead) => (Pending, offset + 1, index, Push(1, []))
	(Open, Tail) => (Broken, offset, index, Bad(InvalidStartByte, offset, offset))
	(Open, Finish) => (Complete, offset, index, End(offset, index))
	(Pending, Tail) => (Open, offset + 1, index + 1, Push(1, [{ value: 0xA2, start: offset - 1, end: offset + 1, index }]))
	(Pending, Finish) => (Broken, offset, index, Bad(UnexpectedEndOfSequence, offset, offset - 1))
	(Pending, Ascii) | (Pending, Lead) => (Broken, offset, index, Bad(ExpectedContinuation, offset, offset - 1))
}

observe : Utf8.Cursor, Action -> (Utf8.Cursor, Observation)
observe = |cursor, action| {
	bytes : List(U8)
	bytes = match action {
		Empty => []
		Ascii => [0x41]
		Lead => [0xC2]
		Tail => [0xA2]
		Finish => return match Utf8.Cursor.finish(cursor) {
			End(done) => (done.cursor, End(done.byte_offset, done.scalar_count))
			Failed(failure) => (failure.cursor, failure_observation(failure.error))
		}
	}
	match Utf8.Cursor.push(
		cursor,
		bytes,
		[],
		|items, located| Continue(
			items.append({
				value: Scalar.to_u32(located.scalar),
				start: ByteRange.start(located.byte_range),
				end: ByteRange.end(located.byte_range),
				index: located.scalar_index,
			}),
		),
	) {
		Pushed(next) => (next.cursor, Push(next.consumed, next.state))
		Stopped(_) => crash "a continuing model callback stopped"
		Failed(failure) => {
			if failure.consumed != 0 or !failure.state.is_empty() {
				crash "a rejected one-byte chunk consumed data or emitted a scalar"
			}
			(failure.cursor, failure_observation(failure.error))
		}
	}
}

failure_observation : Utf8.Cursor.Error -> Observation
failure_observation = |error| match error {
	AlreadyFinished => AlreadyEnded
	AlreadyFailed => AlreadyBroken
	Malformed(detail) => Bad(detail.problem, detail.offset, detail.sequence_start)
	_ => crash "unexpected UTF-8 model error"
}

expect {
	report = Model.explore(machine, { depth: 5, key: |world| world.history, verify: EveryStep })
	if report.verdict != Holds(Exhausted) {
		crash "${Model.explain(report)} ${Str.inspect(report.verdict)}"
	}
	report.states == 781
}

decode : List(U8) -> List(Action)
decode = |bytes| bytes.take_first(4).map(
	|byte| match byte % 5 {
		0 => Empty
		1 => Ascii
		2 => Lead
		3 => Tail
		_ => Finish
	},
)

Utf8Model :: [].{
	target = Model.path_target({
		name: "unicode-utf8-model",
		machine,
		decode,
		examples: [
			Fuzz.saved({ name: "split-with-empty", revision: "1", bytes: [2, 0, 3, 4] }),
			Fuzz.saved({ name: "push-after-truncation", revision: "1", bytes: [2, 4, 3] }),
			Fuzz.saved({ name: "push-after-finish", revision: "1", bytes: [1, 4, 1] }),
		],
	})
}
