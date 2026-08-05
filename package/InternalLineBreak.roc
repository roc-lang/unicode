import InternalGrapheme
import InternalLineBreakData
import TextPosition

DecisionTag : [Mandatory, Allowed, Prohibited]
AuthorityTag : [NonTailorable, Tailorable]

TokenRecord : {
    line_class : InternalLineBreakData.Class,
    initial_quote : Bool,
    final_quote : Bool,
    east_asian : Bool,
    unassigned_extended_pictographic : Bool,
    dotted_circle : Bool,
}

OutcomeRecord : { decision : DecisionTag, authority : AuthorityTag }
EventRecord : {
    at : TextPosition,
    decision : [Mandatory, Allowed],
    authority : AuthorityTag,
}

NumericState : [NoNumeric, NumericBody, NumericClose]

MachineState : {
    started : Bool,
    left : TokenRecord,
    left2 : TokenRecord,
    has_left2 : Bool,
    space_base : TokenRecord,
    has_space_base : Bool,
    space_base_predecessor : TokenRecord,
    has_space_base_predecessor : Bool,
    numeric : NumericState,
    ri_odd : Bool,
    zw_space_run : Bool,
    raw_previous : InternalLineBreakData.Class,
    raw_previous_zwj : Bool,
    preserve_graphemes : Bool,
    grapheme : InternalGrapheme.Machine,
}

PreparedRecord : {
    raw : InternalLineBreakData.Props,
    token : TokenRecord,
    attached : Bool,
    grapheme : InternalGrapheme.Machine,
    grapheme_break_before : Bool,
}

FirstNeed : [FinalQuoteNeed, SpaceIsNeed, QuoteEastAsianNeed, NumericPrefixNeed, BrahmicNeed]
SecondNeed : [NumericPrefixAfterIs]
Classification : [NeedsFirst(FirstNeed), Resolved(OutcomeRecord)]
FirstResolution : [NeedsSecond(SecondNeed), Resolved(OutcomeRecord)]
FutureToken : [EndAhead, TokenAhead(TokenRecord)]

PendingRecord : {
    machine : MachineState,
    prepared : PreparedRecord,
    at : TextPosition,
}

PendingState : [
    NoPending,
    WaitingOne({ pending : PendingRecord, need : FirstNeed }),
    WaitingSecond({ pending : PendingRecord, need : SecondNeed }),
]
StreamState : { machine : MachineState, pending : PendingState }
EmittedEvents : [NoEvents, OneEvent(EventRecord), TwoEvents({ first : EventRecord, second : EventRecord })]

## Exact Unicode 17 UAX #14 rev-55 transition core.
##
## `classify` contains rule precedence and authority in one place. The complete
## string adapter supplies bounded lookahead when requested. The non-replayable
## stream adapter retains one candidate coordinate and emits opportunities
## only, which keeps `CM*` latency bounded without retaining source text.
InternalLineBreak :: [].{
    Decision : DecisionTag
    Authority : AuthorityTag
    Token : TokenRecord
    Outcome : OutcomeRecord
    Event : EventRecord
    Machine : MachineState
    Prepared : PreparedRecord
    FirstNeed : FirstNeed
    SecondNeed : SecondNeed
    FutureToken : FutureToken
    Classification : Classification
    FirstResolution : FirstResolution
    Stream : StreamState
    Emissions : EmittedEvents

    init : Bool -> Machine
    init = |preserve_graphemes| {
        empty = empty_token({})
        {
            started: Bool.False,
            left: empty,
            left2: empty,
            has_left2: Bool.False,
            space_base: empty,
            has_space_base: Bool.False,
            space_base_predecessor: empty,
            has_space_base_predecessor: Bool.False,
            numeric: NoNumeric,
            ri_odd: Bool.False,
            zw_space_run: Bool.False,
            raw_previous: AL,
            raw_previous_zwj: Bool.False,
            preserve_graphemes,
            grapheme: InternalGrapheme.init({}),
        }
    }

    stream_init : Bool -> Stream
    stream_init = |preserve_graphemes| {
        { machine: InternalLineBreak.init(preserve_graphemes), pending: NoPending }
    }

    prepare : Machine, U32, U64 -> Prepared
    prepare = |machine, scalar, byte_start| prepare_scalar(machine, scalar, byte_start)

    classify : Machine, Prepared -> Classification
    classify = |machine, prepared| classify_boundary(machine, prepared)

    resolve_end : Machine, Prepared, FirstNeed -> Outcome
    resolve_end = |machine, prepared, need| resolve_need_at_end(machine, prepared, need)

    resolve_first : Machine, Prepared, FirstNeed, Token -> FirstResolution
    resolve_first = |machine, prepared, need, token| resolve_need_with_first(machine, prepared, need, token)

    resolve_second : Machine, Prepared, SecondNeed, FutureToken -> Outcome
    resolve_second = |machine, prepared, need, future| resolve_second_need(machine, prepared, need, future)

    advance : Machine, Prepared -> Machine
    advance = |machine, prepared| advance_machine(machine, prepared)

    token_for_lookahead : Token, U32 -> [Attached, Significant(Token)]
    token_for_lookahead = |preceding, scalar| {
        raw = InternalLineBreakData.lookup(scalar)
        if is_combining(raw.class) and can_attach_after(preceding.line_class) {
            Attached
        } else if is_combining(raw.class) {
            Significant(al_token({}))
        } else {
            Significant(token_from_props(raw, scalar))
        }
    }

    stream_push : Stream, U32, TextPosition -> { stream : Stream, emissions : Emissions }
    stream_push = |stream, scalar, at| stream_push_scalar(stream, scalar, at)

    ## Fold one already-validated ASCII vector with line-break-owned property
    ## decoding. Homogeneous letter blocks collapse the fifteen internal LB28
    ## transitions after the first exact transition into one state update.
    fold_ascii_block : Stream, U8x16, U64, U64, state, (state, Event -> state) -> { stream : Stream, state : state }
    fold_ascii_block = |stream, vector, byte_start, scalar_start, initial, emit| {
        if !stream.machine.preserve_graphemes and is_ascii_letter_block(vector) {
            if is_stable_al_stream(stream) {
                # No retained coordinate changes within a settled AL run, and
                # every internal boundary is prohibited by LB28.
                return { stream, state: initial }
            }
            first = stream_push_with_props(
                stream,
                ascii_props(vector.get_lane(0)),
                vector.get_lane(0).to_u32(),
                TextPosition.from_offsets(byte_start, scalar_start),
            )
            state = fold_emitted_events(initial, first.emissions, emit)
            # Every remaining boundary is AL x AL (LB28), and no AL boundary
            # creates a right-context request. The summary is exactly the
            # state reached by fifteen more scalar transitions.
            al = al_token({})
            summarized = {
                started: Bool.True,
                left: al,
                left2: al,
                has_left2: Bool.True,
                space_base: al,
                has_space_base: Bool.True,
                space_base_predecessor: al,
                has_space_base_predecessor: Bool.True,
                numeric: NoNumeric,
                ri_odd: Bool.False,
                zw_space_run: Bool.False,
                raw_previous: AL,
                raw_previous_zwj: Bool.False,
                preserve_graphemes: Bool.False,
                grapheme: first.stream.machine.grapheme,
            }
            { stream: { machine: summarized, pending: NoPending }, state }
        } else {
            var next_stream = stream
            var state = initial
            var lane = 0.U64
            while lane < 16 {
                byte = vector.get_lane(lane)
                transition = stream_push_with_props(
                    next_stream,
                    ascii_props(byte),
                    byte.to_u32(),
                    TextPosition.from_offsets(byte_start + lane, scalar_start + lane),
                )
                next_stream = transition.stream
                state = fold_emitted_events(state, transition.emissions, emit)
                lane = lane + 1
            }
            { stream: next_stream, state }
        }
    }

    stream_finish : Stream, TextPosition -> { stream : Stream, emissions : Emissions }
    stream_finish = |stream, at| {
        pending_event = match stream.pending {
            NoPending => NoEvent
            WaitingOne({ pending, need }) => outcome_event(
                pending,
                resolve_need_at_end(pending.machine, pending.prepared, need),
            )
            WaitingSecond({ pending, need }) => outcome_event(
                pending,
                resolve_second_need(pending.machine, pending.prepared, need, EndAhead),
            )
        }
        end_event = {
            at,
            decision: Mandatory,
            authority: NonTailorable,
        }
        emissions = match pending_event {
            NoEvent => OneEvent(end_event)
            HasEvent(event) => TwoEvents({ first: event, second: end_event })
        }
        { stream: { machine: stream.machine, pending: NoPending }, emissions }
    }
}

MaybeEvent : [NoEvent, HasEvent(EventRecord)]

empty_token : {} -> TokenRecord
empty_token = |_| {
    {
        line_class: AL,
        initial_quote: Bool.False,
        final_quote: Bool.False,
        east_asian: Bool.False,
        unassigned_extended_pictographic: Bool.False,
        dotted_circle: Bool.False,
    }
}

al_token : {} -> TokenRecord
al_token = |_| empty_token({})

token_from_props : InternalLineBreakData.Props, U32 -> TokenRecord
token_from_props = |props, scalar| {
    {
        line_class: props.class,
        initial_quote: props.initial_quote,
        final_quote: props.final_quote,
        east_asian: props.east_asian,
        unassigned_extended_pictographic: props.unassigned_extended_pictographic,
        dotted_circle: scalar == 0x25CC,
    }
}

prepare_scalar : MachineState, U32, U64 -> PreparedRecord
prepare_scalar = |machine, scalar, byte_start| {
    raw = InternalLineBreakData.lookup(scalar)
    prepare_with_props(machine, raw, scalar, byte_start)
}

prepare_with_props : MachineState, InternalLineBreakData.Props, U32, U64 -> PreparedRecord
prepare_with_props = |machine, raw, scalar, byte_start| {
    attached = machine.started and is_combining(raw.class) and can_attach_after(machine.left.line_class)
    token = if attached {
        token_from_props(raw, scalar)
    } else if is_combining(raw.class) {
        # LB10 gives every remaining CM/ZWJ all of U+0041's relevant facts.
        al_token({})
    } else {
        token_from_props(raw, scalar)
    }
    grapheme_transition = if machine.preserve_graphemes {
        InternalGrapheme.push(machine.grapheme, scalar, byte_start)
    } else {
        { machine: machine.grapheme, boundary: NoBoundary }
    }
    grapheme_break_before = match grapheme_transition.boundary {
        NoBoundary => Bool.False
        Boundary(_) => Bool.True
    }
    {
        raw,
        token,
        attached,
        grapheme: grapheme_transition.machine,
        grapheme_break_before,
    }
}

advance_machine : MachineState, PreparedRecord -> MachineState
advance_machine = |machine, prepared| {
    raw_class = prepared.raw.class
    if prepared.attached {
        {
            started: machine.started,
            left: machine.left,
            left2: machine.left2,
            has_left2: machine.has_left2,
            space_base: machine.space_base,
            has_space_base: machine.has_space_base,
            space_base_predecessor: machine.space_base_predecessor,
            has_space_base_predecessor: machine.has_space_base_predecessor,
            numeric: machine.numeric,
            ri_odd: machine.ri_odd,
            zw_space_run: machine.zw_space_run,
            raw_previous: raw_class,
            raw_previous_zwj: raw_class == ZWJ,
            preserve_graphemes: machine.preserve_graphemes,
            grapheme: prepared.grapheme,
        }
    } else {
        current = prepared.token
        next_numeric = if current.line_class == NU {
            NumericBody
        } else if (current.line_class == SY or current.line_class == IS) and machine.numeric == NumericBody {
            NumericBody
        } else if (current.line_class == CL or current.line_class == CP) and machine.numeric == NumericBody {
            NumericClose
        } else {
            NoNumeric
        }
        next_ri_odd = if current.line_class == RI {
            if machine.started and machine.left.line_class == RI {
                !machine.ri_odd
            } else {
                Bool.True
            }
        } else {
            Bool.False
        }
        next_zw_space_run = if current.line_class == ZW {
            Bool.True
        } else {
            current.line_class == SP and machine.zw_space_run
        }
        update_space_base = current.line_class != SP
        {
            started: Bool.True,
            left: current,
            left2: machine.left,
            has_left2: machine.started,
            space_base: if update_space_base current else machine.space_base,
            has_space_base: if update_space_base Bool.True else machine.has_space_base,
            space_base_predecessor: if update_space_base machine.left else machine.space_base_predecessor,
            has_space_base_predecessor: if update_space_base machine.started else machine.has_space_base_predecessor,
            numeric: next_numeric,
            ri_odd: next_ri_odd,
            zw_space_run: next_zw_space_run,
            raw_previous: raw_class,
            raw_previous_zwj: raw_class == ZWJ,
            preserve_graphemes: machine.preserve_graphemes,
            grapheme: prepared.grapheme,
        }
    }
}

classify_boundary : MachineState, PreparedRecord -> Classification
classify_boundary = |machine, prepared| {
    raw_current = prepared.raw.class
    left = machine.left
    current = prepared.token
    left_class = left.line_class
    current_class = current.line_class
    base = if left_class == SP and machine.has_space_base {
        machine.space_base
    } else {
        left
    }
    base_has_predecessor = if left_class == SP {
        machine.has_space_base_predecessor
    } else {
        machine.has_left2
    }
    base_predecessor = if left_class == SP {
        machine.space_base_predecessor
    } else {
        machine.left2
    }

    result = if machine.raw_previous == BK {
        # LB4.
        Resolved({ decision: Mandatory, authority: NonTailorable })
    } else if machine.raw_previous == CR and raw_current == LF {
        # LB5.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if machine.raw_previous == CR or machine.raw_previous == LF or machine.raw_previous == NL {
        # LB5.
        Resolved({ decision: Mandatory, authority: NonTailorable })
    } else if is_hard(raw_current) {
        # LB6.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if raw_current == SP or raw_current == ZW {
        # LB7.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if machine.zw_space_run {
        # LB8.
        Resolved({ decision: Allowed, authority: NonTailorable })
    } else if machine.raw_previous_zwj {
        # LB8a.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if prepared.attached {
        # LB9. LB10 was applied while preparing a non-attached CM/ZWJ.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if left_class == WJ or current_class == WJ {
        # LB11.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if left_class == GL {
        # LB12.
        Resolved({ decision: Prohibited, authority: NonTailorable })
    } else if current_class == GL and left_class != SP and left_class != BA and left_class != HY and left_class != HH {
        # LB12a is the first tailorable rule in rev 55.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current_class == CL or current_class == CP or current_class == EX or current_class == SY {
        # LB13.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if base.line_class == OP {
        # LB14.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if base.initial_quote and initial_quote_context(base_has_predecessor, base_predecessor) {
        # LB15a.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current.final_quote {
        # LB15b has right context and precedes the broader LB19 fallback.
        NeedsFirst(FinalQuoteNeed)
    } else {
        classify_after_lb15b(machine, prepared, base, base_has_predecessor, base_predecessor)
    }
    apply_profile(machine, prepared, result)
}

classify_after_lb15b : MachineState, PreparedRecord, TokenRecord, Bool, TokenRecord -> Classification
classify_after_lb15b = |machine, prepared, base, base_has_predecessor, base_predecessor| {
    left = machine.left
    current = prepared.token
    left_class = left.line_class
    current_class = current.line_class

    if left_class == SP and current_class == IS {
        # LB15c must inspect the token following IS; LB15d handles the rest.
        NeedsFirst(SpaceIsNeed)
    } else if current_class == IS {
        # LB15d.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (base.line_class == CL or base.line_class == CP) and current_class == NS {
        # LB16.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if base.line_class == B2 and current_class == B2 {
        # LB17.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == SP {
        # LB18.
        Resolved({ decision: Allowed, authority: Tailorable })
    } else if current_class == QU and !current.initial_quote {
        # LB19, first expression.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == QU and !left.final_quote {
        # LB19, second expression.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current_class == QU and !left.east_asian {
        # LB19a, first expression.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current_class == QU {
        # LB19a, second expression.
        NeedsFirst(QuoteEastAsianNeed)
    } else {
        classify_after_quotes(machine, prepared, base_has_predecessor, base_predecessor)
    }
}

classify_after_quotes : MachineState, PreparedRecord, Bool, TokenRecord -> Classification
classify_after_quotes = |machine, prepared, _base_has_predecessor, _base_predecessor| {
    left = machine.left
    current = prepared.token
    left_class = left.line_class
    current_class = current.line_class

    if left_class == QU and !current.east_asian {
        # LB19a, third expression.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == QU and (!machine.has_left2 or !machine.left2.east_asian) {
        # LB19a, fourth expression.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current_class == CB or left_class == CB {
        # LB20, including its specified unresolved default.
        Resolved({ decision: Allowed, authority: Tailorable })
    } else if (left_class == HY or left_class == HH)
        and (!machine.has_left2 or word_initial_hyphen_context(machine.left2.line_class))
        and is_letter(current_class) {
        # LB20a.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current_class == BA or current_class == HH or current_class == HY or current_class == NS or left_class == BB {
        # LB21.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (left_class == HY or left_class == HH)
        and machine.has_left2
        and machine.left2.line_class == HL
        and current_class != HL {
        # LB21a.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == SY and current_class == HL {
        # LB21b.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if current_class == IN {
        # LB22.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (is_letter(left_class) and current_class == NU) or (left_class == NU and is_letter(current_class)) {
        # LB23.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (left_class == PR and is_ideographic_emoji(current_class))
        or (is_ideographic_emoji(left_class) and current_class == PO) {
        # LB23a.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if ((left_class == PR or left_class == PO) and is_letter(current_class))
        or (is_letter(left_class) and (current_class == PR or current_class == PO)) {
        # LB24.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else {
        classify_from_lb25(machine, prepared)
    }
}

classify_from_lb25 : MachineState, PreparedRecord -> Classification
classify_from_lb25 = |machine, prepared| {
    left = machine.left
    current = prepared.token
    left_class = left.line_class
    current_class = current.line_class
    numeric_suffix = (machine.numeric == NumericBody and (current_class == PO or current_class == PR or current_class == NU))
        or (machine.numeric == NumericClose and (current_class == PO or current_class == PR))

    if numeric_suffix {
        # LB25 suffix forms beginning with NU.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (left_class == PR or left_class == PO) and current_class == OP {
        # LB25 prefix forms require one or two following significant tokens.
        NeedsFirst(NumericPrefixNeed)
    } else if (left_class == PR or left_class == PO or left_class == HY or left_class == IS) and current_class == NU {
        # Remaining LB25 pair forms.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else {
        classify_after_lb25(machine, prepared)
    }
}

classify_after_lb25 : MachineState, PreparedRecord -> Classification
classify_after_lb25 = |machine, prepared| {
    left = machine.left
    current = prepared.token
    left_class = left.line_class
    current_class = current.line_class

    if left_class == JL and (current_class == JL or current_class == JV or current_class == H2 or current_class == H3) {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (left_class == JV or left_class == H2) and (current_class == JV or current_class == JT) {
        # LB26.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (left_class == JT or left_class == H3) and current_class == JT {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if is_hangul(left_class) and current_class == PO {
        # LB27.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == PR and is_hangul(current_class) {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if is_letter(left_class) and is_letter(current_class) {
        # LB28.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else {
        classify_from_lb28a(machine, prepared)
    }
}

classify_from_lb28a : MachineState, PreparedRecord -> Classification
classify_from_lb28a = |machine, prepared| {
    left = machine.left
    current = prepared.token
    left_base = is_brahmic_base(left)
    current_base = is_brahmic_base(current)

    if left.line_class == AP and current_base {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_base and (current.line_class == VF or current.line_class == VI) {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left.line_class == VI
        and machine.has_left2
        and is_brahmic_base(machine.left2)
        and (current.line_class == AK or current.dotted_circle) {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_base and current_base {
        # The last LB28a expression is the remaining right-context family.
        NeedsFirst(BrahmicNeed)
    } else {
        classify_after_lb28a(machine, prepared)
    }
}

classify_after_lb28a : MachineState, PreparedRecord -> Classification
classify_after_lb28a = |machine, prepared| {
    left = machine.left
    current = prepared.token
    left_class = left.line_class
    current_class = current.line_class

    if left_class == IS and is_letter(current_class) {
        # LB29.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (is_letter(left_class) or left_class == NU) and current_class == OP and !current.east_asian {
        # LB30.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == CP and !left.east_asian and (is_letter(current_class) or current_class == NU) {
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if left_class == RI and current_class == RI and machine.ri_odd {
        # LB30a.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else if (left_class == EB or left.unassigned_extended_pictographic) and current_class == EM {
        # LB30b.
        Resolved({ decision: Prohibited, authority: Tailorable })
    } else {
        # LB31.
        Resolved({ decision: Allowed, authority: Tailorable })
    }
}

apply_profile : MachineState, PreparedRecord, Classification -> Classification
apply_profile = |machine, prepared, classification| {
    match classification {
        NeedsFirst(need) => NeedsFirst(need)
        Resolved(outcome) => Resolved(profile_outcome(machine, prepared, outcome))
    }
}

profile_outcome : MachineState, PreparedRecord, OutcomeRecord -> OutcomeRecord
profile_outcome = |machine, prepared, outcome| {
    if machine.preserve_graphemes
        and outcome.decision == Allowed
        and outcome.authority == Tailorable
        and !prepared.grapheme_break_before {
        { decision: Prohibited, authority: Tailorable }
    } else {
        outcome
    }
}

# End-of-text resolution is separate from first-token resolution so the type
# cannot leave a second-token request outstanding when no token exists.
resolve_need_at_end : MachineState, PreparedRecord, FirstNeed -> OutcomeRecord
resolve_need_at_end = |machine, prepared, need| {
    outcome = match need {
        FinalQuoteNeed => { decision: Prohibited, authority: Tailorable }
        SpaceIsNeed => { decision: Prohibited, authority: Tailorable }
        QuoteEastAsianNeed => { decision: Prohibited, authority: Tailorable }
        NumericPrefixNeed => { decision: Allowed, authority: Tailorable }
        BrahmicNeed => { decision: Allowed, authority: Tailorable }
    }
    profile_outcome(machine, prepared, outcome)
}

resolve_need_with_first : MachineState, PreparedRecord, FirstNeed, TokenRecord -> FirstResolution
resolve_need_with_first = |machine, prepared, need, next| {
    resolution = match need {
        # If LB15b does not match, an intervening SP is allowed by LB18;
        # otherwise the same Pf quotation mark is prohibited by LB19.
        FinalQuoteNeed => if final_quote_follower(next.line_class) {
            Resolved({ decision: Prohibited, authority: Tailorable })
        } else if machine.left.line_class == SP {
            Resolved({ decision: Allowed, authority: Tailorable })
        } else {
            Resolved({ decision: Prohibited, authority: Tailorable })
        }
        SpaceIsNeed => if next.line_class == NU {
            Resolved({ decision: Allowed, authority: Tailorable })
        } else {
            Resolved({ decision: Prohibited, authority: Tailorable })
        }
        QuoteEastAsianNeed => if next.east_asian {
            Resolved({ decision: Allowed, authority: Tailorable })
        } else {
            Resolved({ decision: Prohibited, authority: Tailorable })
        }
        NumericPrefixNeed => if next.line_class == NU {
            Resolved({ decision: Prohibited, authority: Tailorable })
        } else if next.line_class == IS {
            NeedsSecond(NumericPrefixAfterIs)
        } else {
            Resolved({ decision: Allowed, authority: Tailorable })
        }
        BrahmicNeed => if next.line_class == VF {
            Resolved({ decision: Prohibited, authority: Tailorable })
        } else {
            Resolved({ decision: Allowed, authority: Tailorable })
        }
    }
    match resolution {
        NeedsSecond(second_need) => NeedsSecond(second_need)
        Resolved(outcome) => Resolved(profile_outcome(machine, prepared, outcome))
    }
}

resolve_second_need : MachineState, PreparedRecord, SecondNeed, FutureToken -> OutcomeRecord
resolve_second_need = |machine, prepared, need, future| {
    outcome = match need {
        NumericPrefixAfterIs => match future {
            TokenAhead(next) if next.line_class == NU => { decision: Prohibited, authority: Tailorable }
            _ => { decision: Allowed, authority: Tailorable }
        }
    }
    profile_outcome(machine, prepared, outcome)
}

stream_push_scalar : StreamState, U32, TextPosition -> { stream : StreamState, emissions : EmittedEvents }
stream_push_scalar = |stream, scalar, at| {
    stream_push_with_props(
        stream,
        InternalLineBreakData.lookup(scalar),
        scalar,
        at,
    )
}

stream_push_with_props : StreamState, InternalLineBreakData.Props, U32, TextPosition -> { stream : StreamState, emissions : EmittedEvents }
stream_push_with_props = |stream, props, scalar, at| {
    machine = stream.machine
    prepared = prepare_with_props(machine, props, scalar, TextPosition.byte_offset(at))
    advanced = advance_machine(machine, prepared)

    if !machine.started {
        return { stream: { machine: advanced, pending: stream.pending }, emissions: NoEvents }
    }

    match stream.pending {
        NoPending => process_current_boundary(machine, prepared, at, advanced, NoEvent)
        WaitingOne({ pending, need }) => {
            if prepared.attached {
                { stream: { machine: advanced, pending: stream.pending }, emissions: NoEvents }
            } else {
                pending_result = resolve_need_with_first(
                    pending.machine,
                    pending.prepared,
                    need,
                    prepared.token,
                )
                match pending_result {
                    NeedsSecond(second_need) => {
                        # Only `PR|OP IS NU` remains unresolved after one
                        # significant future token. OP|IS is LB15d-prohibited.
                        {
                            stream: {
                                machine: advanced,
                                pending: WaitingSecond({ pending, need: second_need }),
                            },
                            emissions: NoEvents,
                        }
                    }
                    Resolved(outcome) => process_current_boundary(
                        machine,
                        prepared,
                        at,
                        advanced,
                        outcome_event(pending, outcome),
                    )
                }
            }
        }
        WaitingSecond({ pending, need }) => {
            if prepared.attached {
                { stream: { machine: advanced, pending: stream.pending }, emissions: NoEvents }
            } else {
                pending_outcome = resolve_second_need(
                    pending.machine,
                    pending.prepared,
                    need,
                    TokenAhead(prepared.token),
                )
                process_current_boundary(
                    machine,
                    prepared,
                    at,
                    advanced,
                    outcome_event(pending, pending_outcome),
                )
            }
        }
    }
}

process_current_boundary : MachineState, PreparedRecord, TextPosition, MachineState, MaybeEvent -> { stream : StreamState, emissions : EmittedEvents }
process_current_boundary = |machine, prepared, at, advanced, earlier| {
    result = classify_boundary(machine, prepared)
    match result {
        NeedsFirst(need) => {
            emissions = match earlier {
                NoEvent => NoEvents
                HasEvent(event) => OneEvent(event)
            }
            {
                stream: {
                    machine: advanced,
                    pending: WaitingOne({ pending: { machine, prepared, at }, need }),
                },
                emissions,
            }
        }
        Resolved(outcome) => {
            current = outcome_event({ machine, prepared, at }, outcome)
            {
                stream: { machine: advanced, pending: NoPending },
                emissions: combine_events(earlier, current),
            }
        }
    }
}

outcome_event : PendingRecord, OutcomeRecord -> MaybeEvent
outcome_event = |pending, outcome| {
    match outcome.decision {
        Prohibited => NoEvent
        Mandatory => HasEvent({ at: pending.at, decision: Mandatory, authority: outcome.authority })
        Allowed => HasEvent({ at: pending.at, decision: Allowed, authority: outcome.authority })
    }
}

fold_emitted_events : state, EmittedEvents, (state, EventRecord -> state) -> state
fold_emitted_events = |initial, emissions, emit| {
    match emissions {
        NoEvents => initial
        OneEvent(event) => emit(initial, event)
        TwoEvents({ first, second }) => emit(emit(initial, first), second)
    }
}

is_ascii_letter_block : U8x16 -> Bool
is_ascii_letter_block = |vector| {
    lowercase = vector.gte_lanes(U8x16.splat(0x61)).bitwise_and(
        vector.lte_lanes(U8x16.splat(0x7A)),
    )
    uppercase = vector.gte_lanes(U8x16.splat(0x41)).bitwise_and(
        vector.lte_lanes(U8x16.splat(0x5A)),
    )
    lowercase.all_lanes_set() or uppercase.all_lanes_set()
}

is_stable_al_stream : StreamState -> Bool
is_stable_al_stream = |stream| {
    no_pending = match stream.pending {
        NoPending => Bool.True
        _ => Bool.False
    }
    machine = stream.machine
    no_pending
    and machine.started
    and machine.left.line_class == AL
    and machine.has_left2
    and machine.left2.line_class == AL
    and machine.has_space_base
    and machine.space_base.line_class == AL
    and machine.has_space_base_predecessor
    and machine.space_base_predecessor.line_class == AL
    and machine.numeric == NoNumeric
    and !machine.ri_odd
    and !machine.zw_space_run
    and machine.raw_previous == AL
    and !machine.raw_previous_zwj
    and !machine.preserve_graphemes
}

ascii_props : U8 -> InternalLineBreakData.Props
ascii_props = |byte| {
    line_class = if byte == 0x09 or byte == 0x7C {
        BA
    } else if byte == 0x0A {
        LF
    } else if byte == 0x0B or byte == 0x0C {
        BK
    } else if byte == 0x0D {
        CR
    } else if byte < 0x20 or byte == 0x7F {
        CM
    } else if byte == 0x20 {
        SP
    } else if byte == 0x21 or byte == 0x3F {
        EX
    } else if byte == 0x22 or byte == 0x27 {
        QU
    } else if byte == 0x24 or byte == 0x2B or byte == 0x5C {
        PR
    } else if byte == 0x25 {
        PO
    } else if byte == 0x28 or byte == 0x5B or byte == 0x7B {
        OP
    } else if byte == 0x29 or byte == 0x5D {
        CP
    } else if byte == 0x2C or byte == 0x2E or byte == 0x3A or byte == 0x3B {
        IS
    } else if byte == 0x2D {
        HY
    } else if byte == 0x2F {
        SY
    } else if byte >= 0x30 and byte <= 0x39 {
        NU
    } else if byte == 0x7D {
        CL
    } else {
        AL
    }
    {
        class: line_class,
        initial_quote: Bool.False,
        final_quote: Bool.False,
        east_asian: Bool.False,
        unassigned_extended_pictographic: Bool.False,
    }
}

combine_events : MaybeEvent, MaybeEvent -> EmittedEvents
combine_events = |first, second| {
    match (first, second) {
        (NoEvent, NoEvent) => NoEvents
        (HasEvent(event), NoEvent) => OneEvent(event)
        (NoEvent, HasEvent(event)) => OneEvent(event)
        (HasEvent(left), HasEvent(right)) => TwoEvents({ first: left, second: right })
    }
}

is_hard : InternalLineBreakData.Class -> Bool
is_hard = |line_class| line_class == BK or line_class == CR or line_class == LF or line_class == NL

is_combining : InternalLineBreakData.Class -> Bool
is_combining = |line_class| line_class == CM or line_class == ZWJ

can_attach_after : InternalLineBreakData.Class -> Bool
can_attach_after = |line_class| !is_hard(line_class) and line_class != SP and line_class != ZW

is_letter : InternalLineBreakData.Class -> Bool
is_letter = |line_class| line_class == AL or line_class == HL

is_ideographic_emoji : InternalLineBreakData.Class -> Bool
is_ideographic_emoji = |line_class| line_class == ID or line_class == EB or line_class == EM

is_hangul : InternalLineBreakData.Class -> Bool
is_hangul = |line_class| line_class == JL or line_class == JV or line_class == JT or line_class == H2 or line_class == H3

is_brahmic_base : TokenRecord -> Bool
is_brahmic_base = |token| token.line_class == AK or token.line_class == AS or token.dotted_circle

initial_quote_context : Bool, TokenRecord -> Bool
initial_quote_context = |has_predecessor, predecessor| {
    !has_predecessor
    or is_hard(predecessor.line_class)
    or predecessor.line_class == OP
    or predecessor.line_class == QU
    or predecessor.line_class == GL
    or predecessor.line_class == SP
    or predecessor.line_class == ZW
}

final_quote_follower : InternalLineBreakData.Class -> Bool
final_quote_follower = |line_class| {
    line_class == SP
    or line_class == GL
    or line_class == WJ
    or line_class == CL
    or line_class == QU
    or line_class == CP
    or line_class == EX
    or line_class == IS
    or line_class == SY
    or is_hard(line_class)
    or line_class == ZW
}

word_initial_hyphen_context : InternalLineBreakData.Class -> Bool
word_initial_hyphen_context = |line_class| {
    is_hard(line_class)
    or line_class == SP
    or line_class == ZW
    or line_class == CB
    or line_class == GL
}
