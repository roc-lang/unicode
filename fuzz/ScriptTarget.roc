import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Grapheme
import unicode.Scalar
import unicode.ScalarRange
import unicode.Script
import unicode.ScriptItemization
import unicode.TextRange

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	parts = FuzzSupport.source_parts(input.code_points)
	source = Str.join_with(parts, "")
	policy = ScriptItemization.with_preferred(
		match input.config % 4 {
			0 => []
			1 => [Hira, Kana]
			2 => [Kana, Hira, Latn]
			_ => [Arab, Latn, Deva]
		},
	)
	expected = ScriptItemization.runs(source, policy)
	iterated = FuzzSupport.pause_iter(ScriptItemization.iter_runs(source, policy), input.config.to_u64() % (parts.len() + 1))
	folded = ScriptItemization.fold_runs(source, policy, [], append_run)
	if expected != iterated or expected != folded {
		crash "script itemization iter, fold, and collected runs disagreed"
	}
	FuzzSupport.check_bounded_collection("ScriptItemization.runs_limited", source, input.config, expected, |limits| ScriptItemization.runs_limited(source, policy, limits))

	clusters = Grapheme.ranges(source)
	cluster_ends = clusters.map(ByteRange.end)
	var $byte = 0
	var $scalar = 0
	var $previous = None
	for run in expected {
		bytes = TextRange.byte_range(run.range)
		scalars = TextRange.scalar_range(run.range)
		part = ByteRange.slice(bytes, source) ?? crash "script run split a UTF-8 scalar"
		width = Scalar.iter(part).fold(0.U64, |count, _| count + 1)
		if ByteRange.start(bytes) != $byte or ScalarRange.start(scalars) != $scalar or width == 0 or ScalarRange.len(scalars) != width {
			crash "script runs did not partition both coordinate axes"
		}
		if !cluster_ends.contains(ByteRange.end(bytes)) {
			crash "script itemization split a grapheme"
		}
		if $previous == Some(run.script) {
			crash "adjacent script runs shared a script"
		}
		$byte = ByteRange.end(bytes)
		$scalar = ScalarRange.end(scalars)
		$previous = Some(run.script)
	}
	if $byte != source.count_utf8_bytes() or $scalar != parts.len() {
		crash "script runs did not cover the complete input"
	}
	chunks = FuzzSupport.chunks(input, parts)
	at_most_one_pending_unit_per_grapheme = clusters.len()
	exact = cursor_runs(chunks, policy, at_most_one_pending_unit_per_grapheme)
	if exact != Ok(expected) {
		crash "a cursor with enough pending storage disagreed with script runs"
	}
	limit = input.config.shr_wrap(2).to_u64() % (clusters.len() + 1)
	match cursor_runs(chunks, policy, limit) {
		Ok(runs) => if runs != expected {
			crash "a successful limited script cursor changed the runs"
		}
		Err(PendingUnitLimitExceeded(detail)) => if detail.limit != limit or detail.required != limit + 1 {
			crash "script cursor reported an incorrect pending limit failure"
		}
		Err(_) => crash "an unexpected failure occurred in a finite script cursor"
	}
	Fuzz.keep
}

append_run : List(ScriptItemization.Run), ScriptItemization.Run -> List(ScriptItemization.Run)
append_run = |runs, run| runs.append(run)

cursor_runs : List(Str), ScriptItemization.ConservativeScxV1, U64 -> Try(List(ScriptItemization.Run), ScriptItemization.Error)
cursor_runs = |chunks, policy, limit| {
	var $cursor = ScriptItemization.cursor_init(policy, limit)
	var $runs = []
	for chunk in chunks {
		match ScriptItemization.cursor_push($cursor, chunk, $runs, append_run) {
			Pushed(next) => {
				$cursor = next.cursor
				$runs = next.state
			}
			Failed(failure) => {
				check_failure(failure, $runs)
				return Err(failure.error)
			}
		}
	}
	match ScriptItemization.cursor_finish($cursor, $runs, append_run) {
		FinishedCursor(done) => {
			match ScriptItemization.cursor_push(done.cursor, "a", [], append_run) {
				Failed({ error: AlreadyFinished, state: [], .. }) => {}
				_ => crash "script cursor accepted a push after finish"
			}
			match ScriptItemization.cursor_finish(done.cursor, [], append_run) {
				Failed({ error: AlreadyFinished, state: [], .. }) => {}
				_ => crash "script cursor accepted repeated finish"
			}
			Ok(done.state)
		}
		Failed(failure) => {
			check_failure(failure, $runs)
			Err(failure.error)
		}
	}
}

check_failure = |failure, earlier_runs| {
	if failure.state != earlier_runs {
		crash "script cursor failure modified the caller's fold state"
	}
	match ScriptItemization.cursor_push(failure.cursor, "", [], append_run) {
		Failed({ error: AlreadyFailed, state: [], .. }) => {}
		_ => crash "script cursor did not seal after failure"
	}
}

ScriptTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-script",
		examples: [
			Fuzz.saved({ name: "neutral-units-between-scripts", revision: "1", bytes: [0, 0x41, 0, 0, 0x2E, 0, 0xC0, 0x2E, 0, 0, 0xD0, 0x05, 0] }),
		],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
