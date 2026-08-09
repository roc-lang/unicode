import InternalCase
import TextPosition
import TextRange
import UnicodeVersion

## Unicode 17 complete-string case mapping and folding.
##
## Every successful result owns both its transformed text and one source fact
## for every input scalar. Case conversion never normalizes the source or the
## result. Context-sensitive rules inspect the original source only.
Case :: [].{
	MappingProfile : [UnicodeDefault, Turkic, Lithuanian]
	FoldProfile : [Full, Simple, TurkicFull, TurkicSimple]
	MappingProfileRevision : [TurkicV1, LithuanianV1]
	FoldProfileRevision : [SimpleV1, TurkicV1]
	Shape : [Unchanged, Simple, Expanded, Removed]
	Fact : {
		input : TextRange,
		output : TextRange,
		shape : Shape,
		contextual : Bool,
	}
	Limits : {
		max_input_bytes : U64,
		max_input_scalars : U64,
		max_output_bytes : U64,
		max_output_scalars : U64,
		max_mapping_facts : U64,
	}
	Error : [
		LimitExceeded({ resource : [InputBytes, InputScalars, OutputBytes, OutputScalars, Facts], limit : U64, required : U64 }),
		CoordinateOverflow({ at : TextPosition }),
		InternalEncodingFault,
	]
	Result : {
		text : Str,
		facts : List(Fact),
		unicode_version : UnicodeVersion,
		operation : [Lower, Upper, Title, Fold],
		profile : [Mapping(MappingProfile), Folding(FoldProfile)],
		profile_revision : [None, Some([Mapping(MappingProfileRevision), Folding(FoldProfileRevision)])],
	}

	default_mapping_profile : MappingProfile
	default_mapping_profile = UnicodeDefault

	## Explicit profile selectors for consumers of this sealed module.
	## These semantic values intentionally do not expose generated data IDs.
	unicode_default : MappingProfile
	unicode_default = UnicodeDefault

	turkic : MappingProfile
	turkic = Turkic

	lithuanian : MappingProfile
	lithuanian = Lithuanian

	default_fold_profile : FoldProfile
	default_fold_profile = Full

	full : FoldProfile
	full = Full

	simple : FoldProfile
	simple = Simple

	turkic_full : FoldProfile
	turkic_full = TurkicFull

	turkic_simple : FoldProfile
	turkic_simple = TurkicSimple

	## Construct mandatory resource limits without exposing the sealed record
	## representation. Every Case operation requires this explicit budget.
	limits : U64, U64, U64, U64, U64 -> Limits
	limits = |max_input_bytes, max_input_scalars, max_output_bytes, max_output_scalars, max_mapping_facts| {
		max_input_bytes,
		max_input_scalars,
		max_output_bytes,
		max_output_scalars,
		max_mapping_facts,
	}

	unlimited_limits : Limits
	unlimited_limits = Case.limits(U64.highest, U64.highest, U64.highest, U64.highest, U64.highest)

	mapping_profile_revision : MappingProfile -> [None, Some(MappingProfileRevision)]
	mapping_profile_revision = |profile| match profile {
		UnicodeDefault => None
		Turkic => Some(TurkicV1)
		Lithuanian => Some(LithuanianV1)
	}

	fold_profile_revision : FoldProfile -> [None, Some(FoldProfileRevision)]
	fold_profile_revision = |profile| match profile {
		Full => None
		Simple => Some(SimpleV1)
		TurkicFull => Some(TurkicV1)
		TurkicSimple => Some(TurkicV1)
	}

	## Inspect opaque result and fact values through stable semantic tags.
	## The return tags are structural, so callers can pattern-match them without
	## gaining access to the generated lookup representation.
	result_text : Result -> Str
	result_text = |result| result.text

	result_facts : Result -> List(Fact)
	result_facts = |result| result.facts

	result_unicode_version : Result -> UnicodeVersion
	result_unicode_version = |result| result.unicode_version

	result_operation : Result -> [Lower, Upper, Title, Fold]
	result_operation = |result| result.operation

	result_profile : Result -> [MappingUnicodeDefault, MappingTurkic, MappingLithuanian, FoldFull, FoldSimple, FoldTurkicFull, FoldTurkicSimple]
	result_profile = |result| match result.profile {
		Mapping(UnicodeDefault) => MappingUnicodeDefault
		Mapping(Turkic) => MappingTurkic
		Mapping(Lithuanian) => MappingLithuanian
		Folding(Full) => FoldFull
		Folding(Simple) => FoldSimple
		Folding(TurkicFull) => FoldTurkicFull
		Folding(TurkicSimple) => FoldTurkicSimple
	}

	result_profile_revision : Result -> [NoProfileRevision, MappingTurkicV1, MappingLithuanianV1, FoldSimpleV1, FoldTurkicV1]
	result_profile_revision = |result| match result.profile_revision {
		None => NoProfileRevision
		Some(Mapping(TurkicV1)) => MappingTurkicV1
		Some(Mapping(LithuanianV1)) => MappingLithuanianV1
		Some(Folding(SimpleV1)) => FoldSimpleV1
		Some(Folding(TurkicV1)) => FoldTurkicV1
	}

	fact_input : Fact -> TextRange
	fact_input = |fact| fact.input

	fact_output : Fact -> TextRange
	fact_output = |fact| fact.output

	fact_shape : Fact -> [Unchanged, Simple, Expanded, Removed]
	fact_shape = |fact| fact.shape

	fact_contextual : Fact -> Bool
	fact_contextual = |fact| fact.contextual

	error_limit_resource : Error -> [NoLimitResource, InputBytes, InputScalars, OutputBytes, OutputScalars, Facts]
	error_limit_resource = |error| match error {
		LimitExceeded({ resource: InputBytes, .. }) => InputBytes
		LimitExceeded({ resource: InputScalars, .. }) => InputScalars
		LimitExceeded({ resource: OutputBytes, .. }) => OutputBytes
		LimitExceeded({ resource: OutputScalars, .. }) => OutputScalars
		LimitExceeded({ resource: Facts, .. }) => Facts
		CoordinateOverflow(_) => NoLimitResource
		InternalEncodingFault => NoLimitResource
	}

	error_kind : Error -> [LimitExceeded, CoordinateOverflow, InternalEncodingFault]
	error_kind = |error| match error {
		LimitExceeded(_) => LimitExceeded
		CoordinateOverflow(_) => CoordinateOverflow
		InternalEncodingFault => InternalEncodingFault
	}

	error_limit : Error -> [Some({ limit : U64, required : U64 }), None]
	error_limit = |error| match error {
		LimitExceeded({ limit, required, .. }) => Some({ limit, required })
		CoordinateOverflow(_) => None
		InternalEncodingFault => None
	}

	## `CoordinateOverflow` carries the absolute scalar-aligned position at
	## which checked coordinate arithmetic failed. Other errors have none.
	error_position : Error -> [NoErrorPosition, ErrorPosition(TextPosition)]
	error_position = |error| match error {
		LimitExceeded(_) => NoErrorPosition
		CoordinateOverflow({ at }) => ErrorPosition(at)
		InternalEncodingFault => NoErrorPosition
	}

	## Apply Unicode 17 full lowercase mappings under an explicit profile.
	to_lower : Str, MappingProfile, Limits -> Try(Result, Error)
	to_lower = |source, profile, budget| {
		match InternalCase.lower(source, profile, budget) {
			Err(error) => Err(error)
			Ok(result) => Ok(result_with_mapping(result, Lower, profile))
		}
	}

	## Apply Unicode 17 full uppercase mappings under an explicit profile.
	to_upper : Str, MappingProfile, Limits -> Try(Result, Error)
	to_upper = |source, profile, budget| {
		match InternalCase.upper(source, profile, budget) {
			Err(error) => Err(error)
			Ok(result) => Ok(result_with_mapping(result, Upper, profile))
		}
	}

	## Apply Unicode R3 titlecasing with the package's exact default Word core.
	## Scalars before a word segment's first cased scalar remain unchanged; that
	## scalar title-maps, and every later scalar lower-maps through the boundary.
	to_title : Str, MappingProfile, Limits -> Try(Result, Error)
	to_title = |source, profile, budget| {
		match InternalCase.title(source, profile, budget) {
			Err(error) => Err(error)
			Ok(result) => Ok(result_with_mapping(result, Title, profile))
		}
	}

	## Apply Unicode 17 case folding. Full uses C+F, simple uses C+S, and an
	## explicit Turkic profile lets a T mapping override that normal selection.
	fold : Str, FoldProfile, Limits -> Try(Result, Error)
	fold = |source, profile, budget| {
		match InternalCase.fold(source, profile, budget) {
			Err(error) => Err(error)
			Ok(result) => Ok(result_with_fold(result, profile))
		}
	}
}

result_with_mapping = |result, operation, profile| {
	{
		text: result.text,
		facts: result.facts,
		unicode_version: UnicodeVersion.current,
		operation,
		profile: Mapping(profile),
		profile_revision: match Case.mapping_profile_revision(profile) {
			None => None
			Some(revision) => Some(Mapping(revision))
		},
	}
}

result_with_fold = |result, profile| {
	{
		text: result.text,
		facts: result.facts,
		unicode_version: UnicodeVersion.current,
		operation: Fold,
		profile: Folding(profile),
		profile_revision: match Case.fold_profile_revision(profile) {
			None => None
			Some(revision) => Some(Folding(revision))
		},
	}
}
