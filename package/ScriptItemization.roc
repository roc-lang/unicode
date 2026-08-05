import ByteRange
import InternalGrapheme
import InternalScriptData
import InternalScriptExtensionsData
import InternalUtf8
import Script
import TextPosition
import TextRange

PrivateScript : U8
PrivateSet : { word0 : U64, word1 : U64, word2 : U64, length : U8 }

CandidateState : [NoCandidates, Candidates(PrivateSet)]
PrimaryState : [NoPrimary, Primary(PrivateScript)]

ClusterState : {
    started : Bool,
    byte_start : U64,
    scalar_start : U64,
    primary_found : Bool,
    candidates : CandidateState,
    primary : PrimaryState,
    has_unknown : Bool,
}

UnitKind : [Definite(PrivateScript), Restricted({ candidates : PrivateSet, primary : PrimaryState }), BroadCommon, UnknownBarrier]
Unit : {
    byte_start : U64,
    byte_end : U64,
    scalar_start : U64,
    scalar_end : U64,
    kind : UnitKind,
}

PendingStart : [NoPending, Pending({ byte_start : U64, scalar_start : U64 })]
ExplicitNeighbor : [NoExplicit, UnknownBoundary, Explicit(PrivateScript)]

RunState(state) : {
    state : state,
    last : [NoRun, LastRun(ScriptItemization.Run)],
    emit : state, ScriptItemization.Run -> state,
}

ReplayState(state) : {
    result : RunState(state),
    common : [NoCommon, CommonSpan({ byte_start : U64, byte_end : U64, scalar_start : U64, scalar_end : U64 })],
    left : ExplicitNeighbor,
    right : ExplicitNeighbor,
    policy : ScriptItemization.ConservativeScxV1,
}

CursorError : [
    AlreadyFinished,
    AlreadyFailed,
    OffsetOverflow,
    PendingUnitLimitExceeded({ limit : U64, required : U64, at_byte : U64, at_scalar : U64 }),
]

CursorStatus : [Active, Finished, FailedStatus(CursorError)]

CursorFold(state) : {
    cursor : ScriptItemization.Cursor,
    result : RunState(state),
    error : [NoError, CursorFailure(CursorError)],
    chunk_scalar_end : U64,
}

UnitCursor : {
    utf8 : InternalUtf8.Cursor,
    machine : InternalGrapheme.Machine,
    cluster : ClusterState,
    byte_base : U64,
    scalar_base : U64,
    finished : Bool,
}

ResolvedSegment : { unit : Unit, script : PrivateScript }
DeferredSegment : [NoDeferredSegment, Deferred(ResolvedSegment)]
ResumeAfterReplay : [ResumeOuter(ExplicitNeighbor), FinishAfterReplay]

LazyReplay : {
    units : UnitCursor,
    left : ExplicitNeighbor,
    right : ExplicitNeighbor,
    common : [NoCommon, CommonSpan({ byte_start : U64, byte_end : U64, scalar_start : U64, scalar_end : U64 })],
    byte_end : U64,
    scalar_end : U64,
    delimiter : [NoDelimiter, Delimiter(ResolvedSegment)],
    resume : ResumeAfterReplay,
}

IterationMode : [ScanningOuter, Replaying(LazyReplay), IterationDone]
IterationState : {
    source : Str,
    policy : ScriptItemization.ConservativeScxV1,
    outer : UnitCursor,
    pending : PendingStart,
    left : ExplicitNeighbor,
    mode : IterationMode,
    deferred : DeferredSegment,
    last : [NoRun, LastRun(ScriptItemization.Run)],
}

UnitStep : [NextUnit({ unit : Unit, cursor : UnitCursor }), NoMoreUnits(UnitCursor)]
ResolvedStep : [NextResolved({ segment : ResolvedSegment, state : IterationState }), NoMoreResolved(IterationState)]

## Script runs under the package's explicitly named ConservativeScxV1 policy.
##
## This is shaping-oriented package policy, not a normative Unicode
## itemization algorithm. It uses Unicode 17 Script/Script_Extensions facts and
## the Unicode 17 extended grapheme algorithm. Grapheme clusters are atomic.
## Restricted scx candidates use nearest explicit neighbors, then explicit
## preference order, then their explicit primary Script, otherwise Common.
## Broadly Common spans inherit one equal/single explicit side; conflicting or
## absent sides remain Common. Inherited-only content becomes Common. Unknown
## remains visible and blocks propagation. Paired punctuation and language are
## deliberately not inferred by ConservativeScxV1.
ScriptItemization :: [].{
    ConservativeScxV1 := { preferred_scripts : List(Script.Value) }

    ## One coalesced script run. Both axes of `range` identify the same
    ## grapheme-aligned span of the logical source; the value retains no text.
    Run : {
        range : TextRange,
        script : Script.Value,
    }

    ## Exact ConservativeScxV1 state for non-replayable scalar-aligned chunks.
    ## Pending entries are compact cluster descriptors; no chunk or slice is
    ## retained. Storage is proportional to unresolved units and bounded by
    ## `max_pending_units`.
    Cursor := {
        machine : InternalGrapheme.Machine,
        cluster : ClusterState,
        byte_offset : U64,
        scalar_offset : U64,
        pending : List(Unit),
        left : ExplicitNeighbor,
        last : [NoRun, LastRun(Run)],
        policy : ConservativeScxV1,
        max_pending_units : U64,
        status : CursorStatus,
    }

    Error : CursorError

    ## The independently versioned package-policy identity.
    policy_revision : Str
    policy_revision = "ConservativeScxV1"

    default : ConservativeScxV1
    default = ConservativeScxV1.{ preferred_scripts: [] }

    ## Construct explicit application preference order. The policy retains the
    ## caller's immutable list; construction does not copy it. Resolving one
    ## restricted cluster scans preferences in order, so its lookup cost is
    ## O(P). Applications should keep this explicit tailoring list short.
    ## Common, Inherited, Unknown, duplicates, and scripts absent from a
    ## candidate set have no effect; no language is inferred.
    with_preferred : List(Script.Value) -> ConservativeScxV1
    with_preferred = |preferred_scripts| ConservativeScxV1.{ preferred_scripts }

    ## Lazily traverse ordered, coalesced, half-open runs.
    ##
    ## This complete-string exact policy performs interval-local replay:
    ## definite scripts/Unknown delimit ambiguous intervals in the first scan,
    ## and each such interval is classified once more from its grapheme-aligned
    ## start. No unresolved text, substring, or descriptor tape is copied.
    ## Every scalar is classified at most twice; auxiliary state is constant.
    ## The iterator retains the source and explicit preference list for its
    ## lifetime; yielded `Run` values retain neither.
    iter_runs : Str, ConservativeScxV1 -> Iter(Run)
    iter_runs = |source, policy| {
        Iter.custom(
            {
                source,
                policy,
                outer: unit_cursor_init(source, 0, 0),
                pending: NoPending,
                left: NoExplicit,
                mode: ScanningOuter,
                deferred: NoDeferredSegment,
                last: NoRun,
            },
            Unknown,
            next_run,
        )
    }

    ## Fold without collecting. The user callback is a shallow consumer of the
    ## non-generic exact replay iterator; it is not threaded through Unicode
    ## decoding, grapheme transition, or replay-machine specializations.
    fold_runs : Str, ConservativeScxV1, state, (state, Run -> state) -> state
    fold_runs = |source, policy, initial, emit| {
        var state = initial
        for run in ScriptItemization.iter_runs(source, policy) {
            state = emit(state, run)
        }
        state
    }

    ## Collect runs. Allocation is proportional to returned run count only.
    runs : Str, ConservativeScxV1 -> List(Run)
    runs = |source, policy| {
        ScriptItemization.fold_runs(source, policy, [], |runs, run| runs.append(run))
    }

    ## Start an exact non-replayable chunk cursor with an explicit bound on
    ## retained unresolved grapheme-cluster descriptors.
    cursor_init : ConservativeScxV1, U64 -> Cursor
    cursor_init = |policy, max_pending_units| Cursor.{
        machine: InternalGrapheme.init({}),
        cluster: empty_cluster,
        byte_offset: 0,
        scalar_offset: 0,
        pending: [],
        left: NoExplicit,
        last: NoRun,
        policy,
        max_pending_units,
        status: Active,
    }

    ## Consume one scalar-aligned chunk. A chunk end is not end of text.
    ## Limit/coordinate failure is atomic: returned caller state is unchanged,
    ## the crossing descriptor is not retained, and the returned cursor is
    ## terminally failed.
    cursor_push : Cursor, Str, state, (state, Run -> state) -> [Pushed({ cursor : Cursor, state : state }), Failed({ cursor : Cursor, state : state, error : Error })]
    cursor_push = |cursor, chunk, initial, emit| {
        match cursor.status {
            Finished => Failed({ cursor, state: initial, error: AlreadyFinished })
            FailedStatus(_) => Failed({ cursor, state: initial, error: AlreadyFailed })
            Active => {
                next_byte_offset = match cursor.byte_offset.plus_try(chunk.count_utf8_bytes()) {
                    Err(Overflow) => return terminal_failure(cursor, initial, OffsetOverflow)
                    Ok(value) => value
                }
                folded = InternalUtf8.fold_scalars(
                    chunk,
                    {
                        cursor,
                        result: { state: initial, last: cursor.last, emit },
                        error: NoError,
                        chunk_scalar_end: 0,
                    },
                    |fold_state, scalar, local_start, _local_end, local_index| {
                        if fold_state.error != NoError {
                            fold_state
                        } else {
                            byte_start = match cursor.byte_offset.plus_try(local_start) {
                                Err(Overflow) => return { ..fold_state, error: CursorFailure(OffsetOverflow) }
                                Ok(value) => value
                            }
                            scalar_index = match cursor.scalar_offset.plus_try(local_index) {
                                Err(Overflow) => return { ..fold_state, error: CursorFailure(OffsetOverflow) }
                                Ok(value) => value
                            }
                            transition = InternalGrapheme.push(fold_state.cursor.machine, scalar, byte_start)
                            (after_unit, fresh_cluster) = match transition.boundary {
                                NoBoundary => (fold_state, fold_state.cursor.cluster)
                                Boundary(_) => {
                                    unit = finish_cluster(fold_state.cursor.cluster, byte_start, scalar_index)
                                    (consume_stream_unit(fold_state, unit), empty_cluster)
                                }
                            }
                            {
                                ..after_unit,
                                cursor: {
                                    ..after_unit.cursor,
                                    machine: transition.machine,
                                    cluster: add_scalar(fresh_cluster, scalar, byte_start, scalar_index),
                                },
                                chunk_scalar_end: local_index + 1,
                            }
                        }
                    },
                )
                match folded.error {
                    CursorFailure(error) => terminal_failure(cursor, initial, error)
                    NoError => {
                        next_scalar_offset = match cursor.scalar_offset.plus_try(folded.chunk_scalar_end) {
                            Err(Overflow) => return terminal_failure(cursor, initial, OffsetOverflow)
                            Ok(value) => value
                        }
                        next = {
                            ..folded.cursor,
                            byte_offset: next_byte_offset,
                            scalar_offset: next_scalar_offset,
                            last: folded.result.last,
                        }
                        Pushed({ cursor: next, state: folded.result.state })
                    }
                }
            }
        }
    }

    ## Resolve end-of-text once, emit the final coalesced run, and complete.
    cursor_finish : Cursor, state, (state, Run -> state) -> [FinishedCursor({ cursor : Cursor, state : state }), Failed({ cursor : Cursor, state : state, error : Error })]
    cursor_finish = |cursor, initial, emit| {
        match cursor.status {
            Finished => Failed({ cursor, state: initial, error: AlreadyFinished })
            FailedStatus(_) => Failed({ cursor, state: initial, error: AlreadyFailed })
            Active => {
                with_cluster = if cursor.cluster.started {
                    unit = finish_cluster(cursor.cluster, cursor.byte_offset, cursor.scalar_offset)
                    consume_stream_unit(
                        {
                            cursor,
                            result: { state: initial, last: cursor.last, emit },
                            error: NoError,
                            chunk_scalar_end: 0,
                        },
                        unit,
                    )
                } else {
                    { cursor, result: { state: initial, last: cursor.last, emit }, error: NoError, chunk_scalar_end: 0 }
                }
                match with_cluster.error {
                    CursorFailure(error) => terminal_failure_finish(cursor, initial, error)
                    NoError => {
                        flushed = flush_stream_pending(with_cluster, NoExplicit, cursor.byte_offset, cursor.scalar_offset)
                        match flushed.error {
                            CursorFailure(error) => terminal_failure_finish(cursor, initial, error)
                            NoError => {
                                final_state = emit_last(flushed.result)
                                finished = {
                                    ..flushed.cursor,
                                    pending: [],
                                    last: NoRun,
                                    status: Finished,
                                }
                                FinishedCursor({ cursor: finished, state: final_state })
                            }
                        }
                    }
                }
            }
        }
    }
}

next_run : IterationState -> Try((ScriptItemization.Run, IterationState), [NoMore])
next_run = |initial| {
    var state = initial
    while Bool.True {
        match next_resolved(state) {
            NoMoreResolved(done) => match done.last {
                NoRun => return Err(NoMore)
                LastRun(run) => return Ok((run, { ..done, last: NoRun }))
            }
            NextResolved(step) => {
                next = run_from_segment(step.segment)
                match step.state.last {
                    NoRun => { state = { ..step.state, last: LastRun(next) } }
                    LastRun(previous) => {
                        if runs_are_adjacent_equal(previous, next) {
                            state = { ..step.state, last: LastRun(combine_runs(previous, next)) }
                        } else {
                            return Ok((previous, { ..step.state, last: LastRun(next) }))
                        }
                    }
                }
            }
        }
    }
    Err(NoMore)
}

next_resolved : IterationState -> ResolvedStep
next_resolved = |initial| {
    var state = initial
    while Bool.True {
        match state.deferred {
            Deferred(segment) => return NextResolved({
                segment,
                state: { ..state, deferred: NoDeferredSegment },
            })
            NoDeferredSegment => {}
        }

        match state.mode {
            IterationDone => return NoMoreResolved(state)
            Replaying(replay) => {
                match next_replay_resolved(state, replay) {
                    NoMoreResolved(next) => { state = next }
                    NextResolved(step) => return NextResolved(step)
                }
            }
            ScanningOuter => {
                match next_unit(state.outer) {
                    NoMoreUnits(cursor) => match state.pending {
                        NoPending => return NoMoreResolved({ ..state, outer: cursor, mode: IterationDone })
                        Pending(start) => {
                            replay = init_lazy_replay(
                                state.source,
                                start,
                                cursor.byte_base + cursor.utf8.byte_offset,
                                cursor.scalar_base + cursor.utf8.scalar_index,
                                state.left,
                                NoExplicit,
                                NoDelimiter,
                                FinishAfterReplay,
                            )
                            state = {
                                ..state,
                                outer: cursor,
                                pending: NoPending,
                                mode: Replaying(replay),
                            }
                        }
                    }
                    NextUnit(step) => {
                        state = { ..state, outer: step.cursor }
                        match step.unit.kind {
                            Restricted(_) => {
                                state = { ..state, pending: pending_from_unit(state.pending, step.unit) }
                            }
                            BroadCommon => {
                                state = { ..state, pending: pending_from_unit(state.pending, step.unit) }
                            }
                            Definite(script) => match state.pending {
                                NoPending => return NextResolved({
                                    segment: { unit: step.unit, script },
                                    state: { ..state, left: Explicit(script) },
                                })
                                Pending(start) => {
                                    delimiter = { unit: step.unit, script }
                                    replay = init_lazy_replay(
                                        state.source,
                                        start,
                                        step.unit.byte_start,
                                        step.unit.scalar_start,
                                        state.left,
                                        Explicit(script),
                                        Delimiter(delimiter),
                                        ResumeOuter(Explicit(script)),
                                    )
                                    state = { ..state, pending: NoPending, mode: Replaying(replay) }
                                }
                            }
                            UnknownBarrier => match state.pending {
                                NoPending => return NextResolved({
                                    segment: { unit: step.unit, script: InternalScriptData.unknown_private_id },
                                    state: { ..state, left: UnknownBoundary },
                                })
                                Pending(start) => {
                                    delimiter = { unit: step.unit, script: InternalScriptData.unknown_private_id }
                                    replay = init_lazy_replay(
                                        state.source,
                                        start,
                                        step.unit.byte_start,
                                        step.unit.scalar_start,
                                        state.left,
                                        UnknownBoundary,
                                        Delimiter(delimiter),
                                        ResumeOuter(UnknownBoundary),
                                    )
                                    state = { ..state, pending: NoPending, mode: Replaying(replay) }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    NoMoreResolved({ ..state, mode: IterationDone })
}

next_replay_resolved : IterationState, LazyReplay -> ResolvedStep
next_replay_resolved = |state, initial_replay| {
    var replay = initial_replay
    while Bool.True {
        match next_unit(replay.units) {
            NoMoreUnits(cursor) => match replay.common {
                CommonSpan(span) => {
                    script = resolve_common(replay.left, replay.right)
                    segment = resolved_span(span, replay.byte_end, replay.scalar_end, script)
                    next_replay = {
                        ..replay,
                        units: cursor,
                        common: NoCommon,
                        left: if is_explicit_private(script) Explicit(script) else NoExplicit,
                    }
                    return NextResolved({
                        segment,
                        state: { ..state, mode: Replaying(next_replay) },
                    })
                }
                NoCommon => return finish_lazy_replay({ ..state, mode: Replaying({ ..replay, units: cursor }) }, replay)
            }
            NextUnit(step) => {
                replay = { ..replay, units: step.cursor }
                match step.unit.kind {
                    BroadCommon => {
                        common = match replay.common {
                            NoCommon => CommonSpan({
                                byte_start: step.unit.byte_start,
                                byte_end: step.unit.byte_end,
                                scalar_start: step.unit.scalar_start,
                                scalar_end: step.unit.scalar_end,
                            })
                            CommonSpan(span) => CommonSpan({
                                ..span,
                                byte_end: step.unit.byte_end,
                                scalar_end: step.unit.scalar_end,
                            })
                        }
                        replay = { ..replay, common }
                    }
                    Restricted(details) => {
                        script = resolve_restricted(details, replay.left, replay.right, state.policy)
                        right_neighbor = if is_explicit_private(script) Explicit(script) else NoExplicit
                        current = { unit: step.unit, script }
                        match replay.common {
                            NoCommon => return NextResolved({
                                segment: current,
                                state: { ..state, mode: Replaying({ ..replay, left: right_neighbor }) },
                            })
                            CommonSpan(span) => {
                                common_script = resolve_common(replay.left, right_neighbor)
                                common = resolved_span(span, step.unit.byte_start, step.unit.scalar_start, common_script)
                                return NextResolved({
                                    segment: common,
                                    state: {
                                        ..state,
                                        deferred: Deferred(current),
                                        mode: Replaying({ ..replay, common: NoCommon, left: right_neighbor }),
                                    },
                                })
                            }
                        }
                    }
                    Definite(script) => {
                        right_neighbor = Explicit(script)
                        current = { unit: step.unit, script }
                        match replay.common {
                            NoCommon => return NextResolved({
                                segment: current,
                                state: { ..state, mode: Replaying({ ..replay, left: right_neighbor }) },
                            })
                            CommonSpan(span) => {
                                common_script = resolve_common(replay.left, right_neighbor)
                                common = resolved_span(span, step.unit.byte_start, step.unit.scalar_start, common_script)
                                return NextResolved({
                                    segment: common,
                                    state: {
                                        ..state,
                                        deferred: Deferred(current),
                                        mode: Replaying({ ..replay, common: NoCommon, left: right_neighbor }),
                                    },
                                })
                            }
                        }
                    }
                    UnknownBarrier => {
                        current = { unit: step.unit, script: InternalScriptData.unknown_private_id }
                        match replay.common {
                            NoCommon => return NextResolved({
                                segment: current,
                                state: { ..state, mode: Replaying({ ..replay, left: UnknownBoundary }) },
                            })
                            CommonSpan(span) => {
                                common_script = resolve_common(replay.left, UnknownBoundary)
                                common = resolved_span(span, step.unit.byte_start, step.unit.scalar_start, common_script)
                                return NextResolved({
                                    segment: common,
                                    state: {
                                        ..state,
                                        deferred: Deferred(current),
                                        mode: Replaying({ ..replay, common: NoCommon, left: UnknownBoundary }),
                                    },
                                })
                            }
                        }
                    }
                }
            }
        }
    }
    NoMoreResolved({ ..state, mode: IterationDone })
}

finish_lazy_replay = |state, replay| {
    match replay.delimiter {
        Delimiter(segment) => {
            next_left = match replay.resume {
                ResumeOuter(left) => left
                FinishAfterReplay => NoExplicit
            }
            NextResolved({
                segment,
                state: { ..state, mode: ScanningOuter, left: next_left },
            })
        }
        NoDelimiter => NoMoreResolved({ ..state, mode: IterationDone })
    }
}

init_lazy_replay = |source, start, byte_end, scalar_end, left, right, delimiter, resume| {
    range = match ByteRange.from_bounds(start.byte_start, byte_end) {
        Ok(value) => value
        Err(_) => ...
    }
    selected = match ByteRange.slice(range, source) {
        Ok(value) => value
        Err(_) => ...
    }
    {
        units: unit_cursor_init(selected, start.byte_start, start.scalar_start),
        left,
        right,
        common: NoCommon,
        byte_end,
        scalar_end,
        delimiter,
        resume,
    }
}

pending_from_unit = |pending, unit| match pending {
    NoPending => Pending({ byte_start: unit.byte_start, scalar_start: unit.scalar_start })
    Pending(_) => pending
}

unit_cursor_init : Str, U64, U64 -> UnitCursor
unit_cursor_init = |source, byte_base, scalar_base| {
    {
        utf8: InternalUtf8.init(source),
        machine: InternalGrapheme.init({}),
        cluster: empty_cluster,
        byte_base,
        scalar_base,
        finished: Bool.False,
    }
}

next_unit : UnitCursor -> UnitStep
next_unit = |initial| {
    var cursor = initial
    while Bool.True {
        if cursor.finished {
            return NoMoreUnits(cursor)
        }
        match InternalUtf8.next(cursor.utf8) {
            Done => {
                finished = { ..cursor, cluster: empty_cluster, finished: Bool.True }
                if cursor.cluster.started {
                    return NextUnit({
                        unit: finish_cluster(
                            cursor.cluster,
                            cursor.byte_base + cursor.utf8.byte_offset,
                            cursor.scalar_base + cursor.utf8.scalar_index,
                        ),
                        cursor: finished,
                    })
                }
                return NoMoreUnits(finished)
            }
            One({ item, rest }) => {
                byte_start = cursor.byte_base + item.byte_start
                scalar_index = cursor.scalar_base + item.scalar_index
                transition = InternalGrapheme.push(cursor.machine, item.scalar, byte_start)
                fresh_cluster = match transition.boundary {
                    NoBoundary => cursor.cluster
                    Boundary(_) => empty_cluster
                }
                next = {
                    ..cursor,
                    utf8: rest,
                    machine: transition.machine,
                    cluster: add_scalar(fresh_cluster, item.scalar, byte_start, scalar_index),
                }
                match transition.boundary {
                    NoBoundary => { cursor = next }
                    Boundary(_) => return NextUnit({
                        unit: finish_cluster(cursor.cluster, byte_start, scalar_index),
                        cursor: next,
                    })
                }
            }
        }
    }
    NoMoreUnits(cursor)
}

resolved_span = |span, byte_end, scalar_end, script| {
    {
        unit: {
            byte_start: span.byte_start,
            byte_end,
            scalar_start: span.scalar_start,
            scalar_end,
            kind: BroadCommon,
        },
        script,
    }
}

run_from_segment = |segment| {
    {
        range: text_range_from_unit(segment.unit),
        script: InternalScriptData.from_private_id(segment.script),
    }
}

runs_are_adjacent_equal = |left, right| {
    left_end = TextRange.end(left.range)
    right_start = TextRange.start(right.range)
    left.script == right.script and TextPosition.is_eq(left_end, right_start)
}

combine_runs = |left, right| {
    {
        ..left,
        range: match TextRange.from_positions(TextRange.start(left.range), TextRange.end(right.range)) {
            Ok(range) => range
            Err(_) => ...
        },
    }
}

text_range_from_unit = |unit| {
    match TextRange.from_positions(
        TextPosition.from_offsets(unit.byte_start, unit.scalar_start),
        TextPosition.from_offsets(unit.byte_end, unit.scalar_end),
    ) {
        Ok(range) => range
        Err(_) => ...
    }
}

terminal_failure = |cursor, state, error| {
    failed = { ..cursor, status: FailedStatus(error) }
    Failed({ cursor: failed, state, error })
}

terminal_failure_finish = |cursor, state, error| {
    failed = { ..cursor, status: FailedStatus(error) }
    Failed({ cursor: failed, state, error })
}

consume_stream_unit = |fold_state, unit| {
    match fold_state.error {
        CursorFailure(_) => fold_state
        NoError => match unit.kind {
            Definite(script) => {
                flushed = flush_stream_pending(
                    fold_state,
                    Explicit(script),
                    unit.byte_start,
                    unit.scalar_start,
                )
                match flushed.error {
                    CursorFailure(_) => flushed
                    NoError => {
                        {
                            ..flushed,
                            cursor: {
                                ..flushed.cursor,
                                pending: [],
                                left: Explicit(script),
                            },
                            result: append_run(flushed.result, unit, script),
                        }
                    }
                }
            }
            UnknownBarrier => {
                flushed = flush_stream_pending(
                    fold_state,
                    UnknownBoundary,
                    unit.byte_start,
                    unit.scalar_start,
                )
                match flushed.error {
                    CursorFailure(_) => flushed
                    NoError => {
                        {
                            ..flushed,
                            cursor: {
                                ..flushed.cursor,
                                pending: [],
                                left: UnknownBoundary,
                            },
                            result: append_run(flushed.result, unit, InternalScriptData.unknown_private_id),
                        }
                    }
                }
            }
            Restricted(_) => retain_stream_pending(fold_state, unit)
            BroadCommon => retain_stream_pending(fold_state, unit)
        }
    }
}

retain_stream_pending = |fold_state, unit| {
    required = match fold_state.cursor.pending.len().plus_try(1) {
        Err(Overflow) => U64.highest
        Ok(value) => value
    }
    if required > fold_state.cursor.max_pending_units {
        {
            ..fold_state,
            error: CursorFailure(PendingUnitLimitExceeded({
                limit: fold_state.cursor.max_pending_units,
                required,
                at_byte: unit.byte_start,
                at_scalar: unit.scalar_start,
            })),
        }
    } else {
        {
            ..fold_state,
            cursor: {
                ..fold_state.cursor,
                pending: fold_state.cursor.pending.append(unit),
            },
        }
    }
}

flush_stream_pending = |fold_state, right, byte_end, scalar_end| {
    if fold_state.cursor.pending.is_empty() {
        fold_state
    } else {
        replayed = fold_state.cursor.pending.fold(
            {
                result: fold_state.result,
                common: NoCommon,
                left: fold_state.cursor.left,
                right,
                policy: fold_state.cursor.policy,
            },
            consume_replay,
        )
        final = flush_common(replayed, byte_end, scalar_end, right)
        {
            ..fold_state,
            cursor: {
                ..fold_state.cursor,
                pending: [],
                left: final.left,
            },
            result: final.result,
        }
    }
}

empty_cluster : ClusterState
empty_cluster = {
    started: Bool.False,
    byte_start: 0,
    scalar_start: 0,
    primary_found: Bool.False,
    candidates: NoCandidates,
    primary: NoPrimary,
    has_unknown: Bool.False,
}

add_scalar : ClusterState, U32, U64, U64 -> ClusterState
add_scalar = |cluster, code_point, byte_start, scalar_index| {
    primary = InternalScriptData.lookup_private(code_point)
    explicit = private_explicit_members(private_extensions(code_point, primary))
    started = if cluster.started cluster else {
        started: Bool.True,
        byte_start,
        scalar_start: scalar_index,
        primary_found: Bool.False,
        candidates: NoCandidates,
        primary: NoPrimary,
        has_unknown: Bool.False,
    }

    if is_explicit_private(primary) and !started.primary_found {
        {
            ..started,
            primary_found: Bool.True,
            candidates: match explicit {
                Some(set) => Candidates(set)
                None => Candidates(private_singleton(primary))
            },
            primary: Primary(primary),
            has_unknown: started.has_unknown,
        }
    } else {
        next_candidates = match (started.candidates, explicit) {
            (NoCandidates, None) => NoCandidates
            (NoCandidates, Some(set)) => Candidates(set)
            (Candidates(current), None) => Candidates(current)
            (Candidates(current), Some(set)) => match private_intersection(current, set) {
                Some(intersection) => Candidates(intersection)
                None => Candidates(current)
            }
        }
        {
            ..started,
            candidates: next_candidates,
            has_unknown: started.has_unknown or primary == InternalScriptData.unknown_private_id,
        }
    }
}

finish_cluster : ClusterState, U64, U64 -> Unit
finish_cluster = |cluster, byte_end, scalar_end| {
    kind = match cluster.candidates {
        Candidates(set) => {
            if set.length == 1 {
                Definite(private_first(set))
            } else {
                Restricted({ candidates: set, primary: cluster.primary })
            }
        }
        NoCandidates => if cluster.has_unknown UnknownBarrier else BroadCommon
    }
    {
        byte_start: cluster.byte_start,
        byte_end,
        scalar_start: cluster.scalar_start,
        scalar_end,
        kind,
    }
}

consume_replay = |state, unit| {
    match unit.kind {
        BroadCommon => {
            next_common = match state.common {
                NoCommon => CommonSpan({
                    byte_start: unit.byte_start,
                    byte_end: unit.byte_end,
                    scalar_start: unit.scalar_start,
                    scalar_end: unit.scalar_end,
                })
                CommonSpan(span) => CommonSpan({
                    ..span, byte_end: unit.byte_end, scalar_end: unit.scalar_end
                })
            }
            { ..state, common: next_common }
        }
        Restricted(details) => {
            resolved = resolve_restricted(details, state.left, state.right, state.policy)
            right_neighbor = if is_explicit_private(resolved) Explicit(resolved) else NoExplicit
            before = flush_common(state, unit.byte_start, unit.scalar_start, right_neighbor)
            {
                ..before,
                result: append_run(before.result, unit, resolved),
                left: right_neighbor,
            }
        }
        Definite(script) => {
            right_neighbor = Explicit(script)
            before = flush_common(state, unit.byte_start, unit.scalar_start, right_neighbor)
            {
                ..before,
                result: append_run(before.result, unit, script),
                left: right_neighbor,
            }
        }
        UnknownBarrier => {
            before = flush_common(state, unit.byte_start, unit.scalar_start, UnknownBoundary)
            {
                ..before,
                result: append_run(before.result, unit, InternalScriptData.unknown_private_id),
                left: UnknownBoundary,
            }
        }
    }
}

flush_common = |state, byte_end, scalar_end, right| {
    match state.common {
        NoCommon => state
        CommonSpan(span) => {
            script = resolve_common(state.left, right)
            unit = {
                byte_start: span.byte_start,
                byte_end,
                scalar_start: span.scalar_start,
                scalar_end,
                kind: BroadCommon,
            }
            {
                ..state,
                result: append_run(state.result, unit, script),
                common: NoCommon,
                left: if is_explicit_private(script) Explicit(script) else NoExplicit,
            }
        }
    }
}

resolve_restricted = |details, left, right, policy| {
    left_member = match left {
        Explicit(script) => if private_contains(details.candidates, script) Some(script) else None
        NoExplicit => None
        UnknownBoundary => None
    }
    right_member = match right {
        Explicit(script) => if private_contains(details.candidates, script) Some(script) else None
        NoExplicit => None
        UnknownBoundary => None
    }
    match (left_member, right_member) {
        (Some(left_script), Some(right_script)) => {
            if left_script == right_script {
                left_script
            } else {
                preference_or_primary(details, policy)
            }
        }
        (Some(script), None) => script
        (None, Some(script)) => script
        (None, None) => preference_or_primary(details, policy)
    }
}

preference_or_primary = |details, policy| {
    for preferred in policy.preferred_scripts {
        preferred_id = InternalScriptData.private_id(preferred)
        if is_explicit_private(preferred_id) and private_contains(details.candidates, preferred_id) {
            return preferred_id
        }
    }
    match details.primary {
        Primary(script) => if private_contains(details.candidates, script) script else InternalScriptData.common_private_id
        NoPrimary => InternalScriptData.common_private_id
    }
}

resolve_common : ExplicitNeighbor, ExplicitNeighbor -> PrivateScript
resolve_common = |left, right| {
    match (left, right) {
        (UnknownBoundary, _) => InternalScriptData.common_private_id
        (_, UnknownBoundary) => InternalScriptData.common_private_id
        (Explicit(left_script), Explicit(right_script)) => if left_script == right_script left_script else InternalScriptData.common_private_id
        (Explicit(script), NoExplicit) => script
        (NoExplicit, Explicit(script)) => script
        (NoExplicit, NoExplicit) => InternalScriptData.common_private_id
    }
}

private_extensions : U32, PrivateScript -> PrivateSet
private_extensions = |code_point, primary| {
    override_id = InternalScriptExtensionsData.lookup_override(code_point)
    if override_id == 0 {
        private_singleton(primary)
    } else {
        InternalScriptExtensionsData.set_bits(override_id)
    }
}

private_singleton : PrivateScript -> PrivateSet
private_singleton = |script| {
    bit = 1.U64.shl_wrap(script % 64)
    match script / 64 {
        0 => { word0: bit, word1: 0, word2: 0, length: 1 }
        1 => { word0: 0, word1: bit, word2: 0, length: 1 }
        _ => { word0: 0, word1: 0, word2: bit, length: 1 }
    }
}

private_contains : PrivateSet, PrivateScript -> Bool
private_contains = |set, script| {
    bit = 1.U64.shl_wrap(script % 64)
    word = match script / 64 {
        0 => set.word0
        1 => set.word1
        _ => set.word2
    }
    word.bitwise_and(bit) != 0
}

private_intersection : PrivateSet, PrivateSet -> [Some(PrivateSet), None]
private_intersection = |left, right| {
    word0 = left.word0.bitwise_and(right.word0)
    word1 = left.word1.bitwise_and(right.word1)
    word2 = left.word2.bitwise_and(right.word2)
    length = U64.count_one_bits(word0) + U64.count_one_bits(word1) + U64.count_one_bits(word2)
    if length == 0 None else Some({ word0, word1, word2, length })
}

private_explicit_members : PrivateSet -> [Some(PrivateSet), None]
private_explicit_members = |set| {
    without_common = private_remove(set, InternalScriptData.common_private_id)
    without_inherited = private_remove(without_common, InternalScriptData.inherited_private_id)
    without_unknown = private_remove(without_inherited, InternalScriptData.unknown_private_id)
    if without_unknown.length == 0 None else Some(without_unknown)
}

private_remove : PrivateSet, PrivateScript -> PrivateSet
private_remove = |set, script| {
    if !private_contains(set, script) {
        set
    } else {
        mask = 1.U64.shl_wrap(script % 64).bitwise_not()
        match script / 64 {
            0 => { ..set, word0: set.word0.bitwise_and(mask), length: set.length - 1 }
            1 => { ..set, word1: set.word1.bitwise_and(mask), length: set.length - 1 }
            _ => { ..set, word2: set.word2.bitwise_and(mask), length: set.length - 1 }
        }
    }
}

private_first : PrivateSet -> PrivateScript
private_first = |set| {
    if set.word0 != 0 {
        U64.count_trailing_zero_bits(set.word0)
    } else if set.word1 != 0 {
        64 + U64.count_trailing_zero_bits(set.word1)
    } else if set.word2 != 0 {
        128 + U64.count_trailing_zero_bits(set.word2)
    } else {
        InternalScriptData.common_private_id
    }
}

is_explicit_private : PrivateScript -> Bool
is_explicit_private = |script| {
    script != InternalScriptData.common_private_id
        and script != InternalScriptData.inherited_private_id
        and script != InternalScriptData.unknown_private_id
}

append_run = |result, unit, script| {
    public_script = InternalScriptData.from_private_id(script)
    next_range = match TextRange.from_positions(
        TextPosition.from_offsets(unit.byte_start, unit.scalar_start),
        TextPosition.from_offsets(unit.byte_end, unit.scalar_end),
    ) {
        Ok(range) => range
        # Unit construction only advances monotonic byte/scalar coordinates.
        # This branch is therefore closed for every valid Str/cursor history;
        # keep an implementation-invariant failure private rather than adding
        # a spurious public Unicode error or emitting a partial result.
        Err(_) => ...
    }
    next = {
        range: next_range,
        script: public_script,
    }
    match result.last {
        NoRun => { ..result, last: LastRun(next) }
        LastRun(previous) => {
            previous_start = TextRange.start(previous.range)
            previous_end = TextRange.end(previous.range)
            if previous.script == public_script
                and TextPosition.byte_offset(previous_end) == unit.byte_start
                and TextPosition.scalar_offset(previous_end) == unit.scalar_start
            {
                combined = {
                    ..previous,
                    range: match TextRange.from_positions(
                        previous_start,
                        TextPosition.from_offsets(unit.byte_end, unit.scalar_end),
                    ) {
                        Ok(range) => range
                        Err(_) => ...
                    },
                }
                { ..result, last: LastRun(combined) }
            } else {
                emit = result.emit
                { ..result, state: emit(result.state, previous), last: LastRun(next) }
            }
        }
    }
}

emit_last = |result| match result.last {
    NoRun => result.state
    LastRun(run) => {
        emit = result.emit
        emit(result.state, run)
    }
}
