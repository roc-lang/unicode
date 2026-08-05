import ByteRange
import InternalGrapheme
import InternalUtf8
import Scalar
import Script

CandidateState : [NoCandidates, Candidates(Script.ScriptSet)]
PrimaryState : [NoPrimary, Primary(Script.Value)]

ClusterState : {
    started : Bool,
    byte_start : U64,
    scalar_start : U64,
    primary_found : Bool,
    candidates : CandidateState,
    primary : PrimaryState,
    has_unknown : Bool,
}

UnitKind : [Definite(Script.Value), Restricted({ candidates : Script.ScriptSet, primary : PrimaryState }), BroadCommon, UnknownBarrier]
Unit : {
    byte_start : U64,
    byte_end : U64,
    scalar_start : U64,
    scalar_end : U64,
    kind : UnitKind,
}

ScanState : {
    machine : InternalGrapheme.Machine,
    cluster : ClusterState,
    scalar_end : U64,
}

PendingStart : [NoPending, Pending({ byte_start : U64, scalar_start : U64 })]
ExplicitNeighbor : [NoExplicit, Explicit(Script.Value)]

RunState(state) : {
    state : state,
    last : [NoRun, LastRun(ScriptItemization.Run)],
}

OuterState(state) : {
    source : Str,
    policy : ScriptItemization.ConservativeScxV1,
    result : RunState(state),
    pending : PendingStart,
    left : ExplicitNeighbor,
    scalar_end : U64,
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

    Run : {
        byte_range : ByteRange,
        scalar_start : U64,
        scalar_end : U64,
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

    ## Construct explicit application preference order. Common, Inherited,
    ## Unknown, duplicates, and scripts absent from a candidate set have no
    ## effect; no language is inferred.
    with_preferred : List(Script.Value) -> ConservativeScxV1
    with_preferred = |preferred_scripts| ConservativeScxV1.{ preferred_scripts }

    ## Fold ordered, coalesced, half-open runs without collecting them.
    ##
    ## This complete-string exact policy performs interval-local replay:
    ## definite scripts/Unknown delimit ambiguous intervals in the first scan,
    ## and each such interval is classified once more from its grapheme-aligned
    ## start. No unresolved text, substring, or descriptor tape is copied.
    ## Every scalar is classified at most twice; auxiliary state is constant.
    fold_runs : Str, ConservativeScxV1, state, (state, Run -> state) -> state
    fold_runs = |source, policy, initial, emit| {
        outer = scan_units(
            source,
            0,
            0,
            {
                source,
                policy,
                result: { state: initial, last: NoRun },
                pending: NoPending,
                left: NoExplicit,
                scalar_end: 0,
            },
            |state, unit| consume_outer(state, unit, emit),
        )

        with_trailing = match outer.pending {
            NoPending => outer.result
            Pending(start) => replay_interval(
                source,
                start,
                source.count_utf8_bytes(),
                outer.scalar_end,
                outer.left,
                NoExplicit,
                policy,
                outer.result,
                emit,
            )
        }
        emit_last(with_trailing, emit)
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
                        result: { state: initial, last: cursor.last },
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
                                    (consume_stream_unit(fold_state, unit, emit), empty_cluster)
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
                            result: { state: initial, last: cursor.last },
                            error: NoError,
                            chunk_scalar_end: 0,
                        },
                        unit,
                        emit,
                    )
                } else {
                    { cursor, result: { state: initial, last: cursor.last }, error: NoError, chunk_scalar_end: 0 }
                }
                match with_cluster.error {
                    CursorFailure(error) => terminal_failure_finish(cursor, initial, error)
                    NoError => {
                        flushed = flush_stream_pending(with_cluster, NoExplicit, cursor.byte_offset, cursor.scalar_offset, emit)
                        match flushed.error {
                            CursorFailure(error) => terminal_failure_finish(cursor, initial, error)
                            NoError => {
                                final_state = emit_last(flushed.result, emit)
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

terminal_failure = |cursor, state, error| {
    failed = { ..cursor, status: FailedStatus(error) }
    Failed({ cursor: failed, state, error })
}

terminal_failure_finish = |cursor, state, error| {
    failed = { ..cursor, status: FailedStatus(error) }
    Failed({ cursor: failed, state, error })
}

consume_stream_unit = |fold_state, unit, emit| {
    match fold_state.error {
        CursorFailure(_) => fold_state
        NoError => match unit.kind {
            Definite(script) => {
                flushed = flush_stream_pending(
                    fold_state,
                    Explicit(script),
                    unit.byte_start,
                    unit.scalar_start,
                    emit,
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
                            result: append_run(flushed.result, unit, script, emit),
                        }
                    }
                }
            }
            UnknownBarrier => {
                flushed = flush_stream_pending(
                    fold_state,
                    NoExplicit,
                    unit.byte_start,
                    unit.scalar_start,
                    emit,
                )
                match flushed.error {
                    CursorFailure(_) => flushed
                    NoError => {
                        {
                            ..flushed,
                            cursor: {
                                ..flushed.cursor,
                                pending: [],
                                left: NoExplicit,
                            },
                            result: append_run(flushed.result, unit, Zzzz, emit),
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

flush_stream_pending = |fold_state, right, byte_end, scalar_end, emit| {
    if fold_state.cursor.pending.is_empty() {
        fold_state
    } else {
        resolved = fold_state.cursor.pending.map(|unit| {
            match unit.kind {
                Restricted(details) => {
                    script = resolve_restricted(
                        details,
                        fold_state.cursor.left,
                        right,
                        fold_state.cursor.policy,
                    )
                    { ..unit, kind: Definite(script) }
                }
                _ => unit
            }
        })
        replayed = resolved.fold(
            {
                result: fold_state.result,
                common: NoCommon,
                left: fold_state.cursor.left,
                right,
                policy: fold_state.cursor.policy,
            },
            |state, unit| consume_replay(state, unit, emit),
        )
        final = flush_common(replayed, byte_end, scalar_end, right, emit)
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

scan_units : Str, U64, U64, state, (state, Unit -> state) -> state
scan_units = |source, byte_base, scalar_base, initial, visit| {
    folded = InternalUtf8.fold_scalars(
        source,
        {
            scan: {
                machine: InternalGrapheme.init({}),
                cluster: empty_cluster,
                scalar_end: scalar_base,
            },
            state: initial,
        },
        |fold_state, scalar, local_start, _local_end, local_index| {
            byte_start = byte_base + local_start
            scalar_index = scalar_base + local_index
            transition = InternalGrapheme.push(fold_state.scan.machine, scalar, byte_start)
            (next_state, fresh_cluster) = match transition.boundary {
                NoBoundary => (fold_state.state, fold_state.scan.cluster)
                Boundary(_) => {
                    unit = finish_cluster(fold_state.scan.cluster, byte_start, scalar_index)
                    (visit(fold_state.state, unit), empty_cluster)
                }
            }
            {
                scan: {
                    machine: transition.machine,
                    cluster: add_scalar(fresh_cluster, scalar, byte_start, scalar_index),
                    scalar_end: scalar_index + 1,
                },
                state: next_state,
            }
        },
    )

    if folded.scan.cluster.started {
        unit = finish_cluster(
            folded.scan.cluster,
            byte_base + source.count_utf8_bytes(),
            folded.scan.scalar_end,
        )
        visit(folded.state, unit)
    } else {
        folded.state
    }
}

add_scalar : ClusterState, U32, U64, U64 -> ClusterState
add_scalar = |cluster, code_point, byte_start, scalar_index| {
    scalar = match Scalar.from_u32(code_point) {
        Ok(value) => value
        Err(_) => return cluster
    }
    primary = Script.of_scalar(scalar)
    explicit = Script.explicit_members(Script.extensions_of_scalar(scalar))
    started = if cluster.started cluster else {
        started: Bool.True,
        byte_start,
        scalar_start: scalar_index,
        primary_found: Bool.False,
        candidates: NoCandidates,
        primary: NoPrimary,
        has_unknown: Bool.False,
    }

    if Script.is_explicit(primary) and !started.primary_found {
        {
            ..started,
            primary_found: Bool.True,
            candidates: match explicit {
                Some(set) => Candidates(set)
                None => Candidates(Script.singleton(primary))
            },
            primary: Primary(primary),
            has_unknown: started.has_unknown,
        }
    } else {
        next_candidates = match (started.candidates, explicit) {
            (NoCandidates, None) => NoCandidates
            (NoCandidates, Some(set)) => Candidates(set)
            (Candidates(current), None) => Candidates(current)
            (Candidates(current), Some(set)) => match Script.intersection(current, set) {
                Some(intersection) => Candidates(intersection)
                None => Candidates(current)
            }
        }
        {
            ..started,
            candidates: next_candidates,
            has_unknown: started.has_unknown or Script.is_unknown(primary),
        }
    }
}

finish_cluster : ClusterState, U64, U64 -> Unit
finish_cluster = |cluster, byte_end, scalar_end| {
    kind = match cluster.candidates {
        Candidates(set) => {
            if Script.len(set) == 1 {
                Definite(match Script.at(set, 0) { Some(script) => script None => Zyyy })
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

consume_outer = |outer, unit, emit| {
    advanced = { ..outer, scalar_end: unit.scalar_end }
    match unit.kind {
        Definite(script) => {
            before = flush_pending_outer(advanced, unit.byte_start, unit.scalar_start, Explicit(script), emit)
            {
                ..before,
                result: append_run(before.result, unit, script, emit),
                pending: NoPending,
                left: Explicit(script),
            }
        }
        UnknownBarrier => {
            before = flush_pending_outer(advanced, unit.byte_start, unit.scalar_start, NoExplicit, emit)
            {
                ..before,
                result: append_run(before.result, unit, Zzzz, emit),
                pending: NoPending,
                left: NoExplicit,
            }
        }
        Restricted(_) => start_pending(advanced, unit)
        BroadCommon => start_pending(advanced, unit)
    }
}

start_pending = |outer, unit| {
    match outer.pending {
        NoPending => { ..outer, pending: Pending({ byte_start: unit.byte_start, scalar_start: unit.scalar_start }) }
        Pending(_) => outer
    }
}

flush_pending_outer = |outer, byte_end, scalar_end, right, emit| {
    match outer.pending {
        NoPending => outer
        Pending(start) => {
            {
                ..outer,
                result: replay_interval(
                    outer.source,
                    start,
                    byte_end,
                    scalar_end,
                    outer.left,
                    right,
                    outer.policy,
                    outer.result,
                    emit,
                ),
                pending: NoPending,
            }
        }
    }
}

replay_interval = |source, start, byte_end, scalar_end, left, right, policy, initial, emit| {
    range = ByteRange.from_bounds(start.byte_start, byte_end) ?? ...
    selected = ByteRange.slice(range, source) ?? ...
    replayed = scan_units(
        selected,
        start.byte_start,
        start.scalar_start,
        {
            result: initial,
            common: NoCommon,
            left,
            right,
            policy,
        },
        |state, unit| consume_replay(state, unit, emit),
    )
    flush_common(replayed, byte_end, scalar_end, right, emit).result
}

consume_replay = |state, unit, emit| {
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
            right_neighbor = if Script.is_explicit(resolved) Explicit(resolved) else NoExplicit
            before = flush_common(state, unit.byte_start, unit.scalar_start, right_neighbor, emit)
            {
                ..before,
                result: append_run(before.result, unit, resolved, emit),
                left: right_neighbor,
            }
        }
        Definite(script) => {
            right_neighbor = Explicit(script)
            before = flush_common(state, unit.byte_start, unit.scalar_start, right_neighbor, emit)
            {
                ..before,
                result: append_run(before.result, unit, script, emit),
                left: right_neighbor,
            }
        }
        UnknownBarrier => {
            before = flush_common(state, unit.byte_start, unit.scalar_start, NoExplicit, emit)
            {
                ..before,
                result: append_run(before.result, unit, Zzzz, emit),
                left: NoExplicit,
            }
        }
    }
}

flush_common = |state, byte_end, scalar_end, right, emit| {
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
                result: append_run(state.result, unit, script, emit),
                common: NoCommon,
                left: if Script.is_explicit(script) Explicit(script) else NoExplicit,
            }
        }
    }
}

resolve_restricted = |details, left, right, policy| {
    left_member = match left {
        Explicit(script) => if Script.contains(details.candidates, script) Some(script) else None
        NoExplicit => None
    }
    right_member = match right {
        Explicit(script) => if Script.contains(details.candidates, script) Some(script) else None
        NoExplicit => None
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
        if Script.is_explicit(preferred) and Script.contains(details.candidates, preferred) {
            return preferred
        }
    }
    match details.primary {
        Primary(script) => if Script.contains(details.candidates, script) script else Zyyy
        NoPrimary => Zyyy
    }
}

resolve_common : ExplicitNeighbor, ExplicitNeighbor -> Script.Value
resolve_common = |left, right| {
    match (left, right) {
        (Explicit(left_script), Explicit(right_script)) => if left_script == right_script left_script else Zyyy
        (Explicit(script), NoExplicit) => script
        (NoExplicit, Explicit(script)) => script
        (NoExplicit, NoExplicit) => Zyyy
    }
}

append_run = |result, unit, script, emit| {
    next_range = ByteRange.from_bounds(unit.byte_start, unit.byte_end) ?? ...
    next = {
        byte_range: next_range,
        scalar_start: unit.scalar_start,
        scalar_end: unit.scalar_end,
        script,
    }
    match result.last {
        NoRun => { ..result, last: LastRun(next) }
        LastRun(previous) => {
            if previous.script == script and ByteRange.end(previous.byte_range) == unit.byte_start {
                combined = {
                    ..previous,
                    byte_range: ByteRange.from_bounds(ByteRange.start(previous.byte_range), unit.byte_end) ?? ...,
                    scalar_end: unit.scalar_end,
                }
                { ..result, last: LastRun(combined) }
            } else {
                { state: emit(result.state, previous), last: LastRun(next) }
            }
        }
    }
}

emit_last = |result, emit| match result.last {
    NoRun => result.state
    LastRun(run) => emit(result.state, run)
}
