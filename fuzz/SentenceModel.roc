import fuzz.Model
import unicode.ByteRange
import unicode.Sentence

SentenceModel := [].{}

Middle : [Neutral, Ignored, Empty]

Ending : [Open, WithLower, WithUpper]

World : { middle : List(Middle), ending : Ending }

Action : [Append(Middle), ResolveLower, ResolveUpper]

# A single period/space prefix, up to three chunks, and either end-of-text
# or a decisive lowercase/uppercase letter. Preserve empty chunks as actions.
machine : Model.Machine(World, Action)
machine = {
	init: { middle: [], ending: Open },
	actions: |world| {
		if world.ending != Open {
			return []
		}
		resolvers = [ResolveLower, ResolveUpper]
		if world.middle.len() < 3 {
			resolvers.concat([Append(Neutral), Append(Ignored), Append(Empty)])
		} else {
			resolvers
		}
	},
	step: |world, action| match action {
		Append(part) => { ..world, middle: world.middle.append(part) }
		ResolveLower => { ..world, ending: WithLower }
		ResolveUpper => { ..world, ending: WithUpper }
	},
	rules: [
		Model.always("SB8 emits only the independently known prefix and final ranges", agrees),
		Model.sometimes("a lowercase resolver follows neutral and empty chunks", |world| world.ending == WithLower and world.middle == [Neutral, Empty]),
		Model.sometimes("a split precedes the first neutral after an ignored mark", |world| world.ending == WithUpper and world.middle == [Ignored, Neutral]),
		Model.sometimes("finish must resolve a cut that could still be suppressed", |world| world.ending == Open and world.middle == [Neutral, Ignored, Neutral]),
	],
}

middle_text : Middle -> Str
middle_text = |part| match part {
	Neutral => "™"
	Ignored => "\u(0301)"
	Empty => ""
}

# Authored units cover only the single SB8 decision after "A. ". Compute the
# expected cut by finding the first symbol that belongs to the next sentence.
# A combining mark before that symbol belongs to the prefix.
expected : World -> { ready : List(U64), finished : List(U64) }
expected = |world| {
	var $at = 3.U64
	var $first_neutral = None
	for part in world.middle {
		if part == Neutral and $first_neutral == None {
			$first_neutral = Some($at)
		}
		$at = $at + middle_text(part).count_utf8_bytes()
	}
	match world.ending {
		WithLower => { ready: [], finished: [$at + 1] }
		WithUpper => {
			cut = match $first_neutral {
				None => $at
				Some(position) => position
			}
			{ ready: [cut], finished: [cut, $at + 1] }
		}
		Open => {
			ends = match $first_neutral {
				None => [$at]
				Some(position) => [position, $at]
			}
			{ ready: [], finished: ends }
		}
	}
}

agrees : World -> Bool
agrees = |world| {
	parts = ["A. "].concat(world.middle.map(middle_text))
	chunks = match world.ending {
		Open => parts
		WithLower => parts.append("a")
		WithUpper => parts.append("A")
	}
	want = expected(world)
	var $cursor = Sentence.Cursor.init({})
	var $ready = []
	append = |items, item| items.append(ByteRange.end(item))
	for chunk in chunks {
		match Sentence.Cursor.push($cursor, chunk, $ready, append) {
			Pushed(result) => {
				if result.consumed != chunk.count_utf8_bytes() {
					return Bool.False
				}
				$cursor = result.cursor
				$ready = result.state
			}
			Failed(_) => return Bool.False
		}
	}
	if $ready != want.ready {
		return Bool.False
	}
	done = match Sentence.Cursor.finish($cursor, $ready, append) {
		End(result) => result
		Failed(_) => return Bool.False
	}
	if done.state != want.finished {
		return Bool.False
	}
	match Sentence.Cursor.push(done.cursor, "a", [], append) {
		Failed({ error: AlreadyFinished, consumed: 0, state, .. }) if state.is_empty() => {}
		_ => return Bool.False
	}
	match Sentence.Cursor.finish(done.cursor, [], append) {
		Failed({ error: AlreadyFinished, state, .. }) => state.is_empty()
		_ => Bool.False
	}
}

expect {
	report = Model.explore(machine, { depth: 5, key: |world| world, verify: EveryStep })
	if report.verdict != Holds(Exhausted) {
		crash "${Model.explain(report)} ${Str.inspect(report.verdict)}"
	}
	report.states == 120 and report.steps == 119 and report.depth == 4 and report.reached.len() == 3
}
