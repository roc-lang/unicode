import InternalGrapheme
import InternalUtf8
import LineBreak
import TextPosition

# Zero refers to the root in the trie and means absent in child/sibling links.
TrieNode : { scalar : U32, first_child : U64, sibling : U64, terminal : Bool }

RunScalar : { scalar : U32, edge : [Interior, Edge(TextPosition)] }

Cluster : { start : InternalUtf8.Cursor, eligible : Bool }

Scan : { run : List(RunScalar), candidates : List(TextPosition) }

Choice : { unknown : U64, tokens : U64, end : U64, known : Bool }

## Opt-in Thai dictionary line breaking for complete strings.
##
## Uses the Unicode line-break decisions under PreserveGraphemes, then opens
## eligible Thai word boundaries where the default prohibition is tailorable.
## The caller owns the dictionary's vocabulary, provenance, and licensing.
ThaiLineBreak :: [].{
	ProfileRevision : [ThaiDictionaryV1]
	profile_revision : ProfileRevision
	profile_revision = ThaiDictionaryV1

	DictionaryLimits : { max_words : U64, max_total_bytes : U64, max_word_bytes : U64 }
	DictionaryError : [
		MissingRevision,
		TooManyWords({ limit : U64 }),
		DictionaryByteLimit({ word : U64, limit : U64 }),
		WordByteLimit({ word : U64, limit : U64 }),
		InvalidWord({ word : U64, byte_offset : U64 }),
	]

	Dictionary :: { revision : Str, nodes : List(TrieNode) }.{

		## Build an owned index without retaining the word strings. The caller
		## assigns a stable revision identifying the exact word set. Identical
		## spellings are idempotent and count toward input limits.
		from_words : List(Str), Str, DictionaryLimits -> Try(Dictionary, DictionaryError)
		from_words = |words, dictionary_revision, limits| {
			if dictionary_revision.count_utf8_bytes() == 0 {
				return Err(MissingRevision)
			}
			if words.len() > limits.max_words {
				return Err(TooManyWords({ limit: limits.max_words }))
			}
			var $total = 0.U64
			var $index = 0.U64
			var $nodes = [node(0, 0)]
			for word in words {
				size = word.count_utf8_bytes()
				if size > limits.max_word_bytes {
					return Err(WordByteLimit({ word: $index, limit: limits.max_word_bytes }))
				}
				if size > limits.max_total_bytes - $total {
					return Err(DictionaryByteLimit({ word: $index, limit: limits.max_total_bytes }))
				}
				$nodes = insert_word($nodes, InternalUtf8.init(word), $index)?
				$total = $total + size
				$index = $index + 1
			}
			Ok({ revision: dictionary_revision, nodes: $nodes })
		}

		revision : Dictionary -> Str
		revision = |dictionary| dictionary.revision
	}

	Limits : { max_input_bytes : U64, max_run_bytes : U64 }
	Error : [InputByteLimit({ limit : U64 }), RunByteLimit({ start : TextPosition, limit : U64 })]

	## Return ordered logical opportunities in the original byte and scalar
	## coordinates. A resource failure is atomic.
	##
	## Segment every maximal run of eligible graphemes independently. Prefer
	## fewer unknown graphemes, then fewer tokens, then the longest first
	## differing token. An unknown token consumes one grapheme, and only a
	## boundary between consecutive known tokens can add an opportunity.
	##
	## The eligibility pass drives the existing grapheme core and replays
	## eligible graphemes into a bounded scalar index for trie matching.
	## A second complete pass uses the existing line-break decision iterator.
	opportunities : Str, Dictionary, Limits -> Try(List(LineBreak.BreakOpportunity), Error)
	opportunities = |source, dictionary, limits| {
		if source.count_utf8_bytes() > limits.max_input_bytes {
			return Err(InputByteLimit({ limit: limits.max_input_bytes }))
		}
		nodes = dictionary.nodes
		candidates = find_candidates(source, nodes, limits.max_run_bytes)?
		Ok(merge_candidates(source, candidates))
	}
}

# Thai spelling alphabet for this policy. It excludes abbreviation and
# repetition punctuation, digits, and scalars outside these Thai SA ranges.
spelling_scalar : U32 -> Bool
spelling_scalar = |scalar| {
	(scalar >= 0x0E01 and scalar <= 0x0E3A and scalar != 0x0E2F)
		or (scalar >= 0x0E40 and scalar <= 0x0E4E and scalar != 0x0E46)
}

spelling_head : U32 -> Bool
spelling_head = |scalar| {
	spelling_scalar(scalar) and (scalar <= 0x0E30
		or scalar == 0x0E32 or scalar == 0x0E33
		or (scalar >= 0x0E40 and scalar <= 0x0E45))
}

node : U32, U64 -> TrieNode
node = |scalar, sibling| { scalar, first_child: 0, sibling, terminal: Bool.False }

trie_child : List(TrieNode), U64, U32 -> U64
trie_child = |nodes, parent, scalar| {
	var $at = (nodes.get(parent) ?? node(0, 0)).first_child
	while $at != 0 {
		candidate = nodes.get($at) ?? node(0, 0)
		if candidate.scalar == scalar {
			return $at
		}
		$at = candidate.sibling
	}
	0
}

insert_word : List(TrieNode), InternalUtf8.Cursor, U64 -> Try(List(TrieNode), ThaiLineBreak.DictionaryError)
insert_word = |initial_nodes, initial_cursor, word| {
	var $nodes = initial_nodes
	var $cursor = initial_cursor
	var $parent = 0.U64
	while Bool.True {
		match InternalUtf8.next($cursor) {
			Done => {
				if $parent == 0 {
					return Err(InvalidWord({ word, byte_offset: 0 }))
				}
				last = $nodes.get($parent) ?? node(0, 0)
				return Ok($nodes.set($parent, { ..last, terminal: Bool.True }) ?? [])
			}
			One({ item, rest }) => {
				valid = if $parent == 0 {
					spelling_head(item.scalar)
				} else {
					spelling_scalar(item.scalar)
				}
				if !valid {
					return Err(InvalidWord({ word, byte_offset: item.byte_start }))
				}
				child = trie_child($nodes, $parent, item.scalar)
				if child == 0 {
					parent = $nodes.get($parent) ?? node(0, 0)
					new_id = $nodes.len()
					$nodes = $nodes.set($parent, { ..parent, first_child: new_id }) ?? []
					$nodes = $nodes.append(node(item.scalar, parent.first_child))
					$parent = new_id
				} else {
					$parent = child
				}
				$cursor = rest
			}
		}
	}
}

find_candidates : Str, List(TrieNode), U64 -> Try(List(TextPosition), ThaiLineBreak.Error)
find_candidates = |source, nodes, max_run_bytes| {
	var $cursor = InternalUtf8.init(source)
	var $grapheme = InternalGrapheme.init({})
	var $cluster = { start: $cursor, eligible: Bool.False }
	var $scan = { run: [], candidates: [] }
	while Bool.True {
		match InternalUtf8.next($cursor) {
			Done => {
				scan = finish_cluster($scan, $cluster, $cursor.byte_offset, nodes, max_run_bytes)?
				return Ok(segment_run(scan.run, nodes, scan.candidates))
			}
			One({ item, rest }) => {
				transition = InternalGrapheme.push($grapheme, item.scalar, item.byte_start)
				match transition.boundary {
					Boundary(_) => {
						$scan = finish_cluster($scan, $cluster, item.byte_start, nodes, max_run_bytes)?
						$cluster = { start: $cursor, eligible: spelling_head(item.scalar) }
					}
					NoBoundary => {
						eligible = if $grapheme.started {
							$cluster.eligible and spelling_scalar(item.scalar)
						} else {
							spelling_head(item.scalar)
						}
						$cluster = { ..$cluster, eligible }
					}
				}
				$grapheme = transition.machine
				$cursor = rest
			}
		}
	}
}

finish_cluster : Scan, Cluster, U64, List(TrieNode), U64 -> Try(Scan, ThaiLineBreak.Error)
finish_cluster = |{ run, candidates }, cluster, end, nodes, max_run_bytes| {
	if !cluster.eligible {
		return Ok({ run: [], candidates: segment_run(run, nodes, candidates) })
	}
	at = TextPosition.from_offsets(cluster.start.byte_offset, cluster.start.scalar_index)
	run_start = match run.first() {
		Ok({ edge: Edge(start), scalar: _ }) => start
		_ => at
	}
	if end - run_start.byte_offset() > max_run_bytes {
		return Err(RunByteLimit({ start: run_start, limit: max_run_bytes }))
	}
	var $run = run
	var $cursor = cluster.start
	while $cursor.byte_offset < end {
		match InternalUtf8.next($cursor) {
			Done => break
			One({ item, rest }) => {
				edge = if item.byte_start == cluster.start.byte_offset {
					Edge(at)
				} else {
					Interior
				}
				$run = $run.append({ scalar: item.scalar, edge })
				$cursor = rest
			}
		}
	}
	Ok({ run: $run, candidates })
}

# DP scores fit U64: both counts are at most the number of indexed scalars,
# which is at most one third of the source byte count. Matching walks the
# trie only until a path fails or reaches the end of the current run.
segment_run : List(RunScalar), List(TrieNode), List(TextPosition) -> List(TextPosition)
segment_run = |run, nodes, candidates| {
	if run.is_empty() {
		return candidates
	}
	end = run.len()
	empty = { unknown: 0, tokens: 0, end, known: Bool.False }
	var $choices = List.repeat(empty, end + 1)
	var $at = end
	var $next_edge = end
	while $at > 0 {
		$at = $at - 1
		match run.get($at) {
			Ok({ edge: Edge(_), scalar: _ }) => {
				fallback = $choices.get($next_edge) ?? empty
				unknown = { unknown: fallback.unknown + 1, tokens: fallback.tokens + 1, end: $next_edge, known: Bool.False }
				best = choose_word(run, nodes, $choices, $at, unknown, empty)
				$choices = $choices.set($at, best) ?? []
				$next_edge = $at
			}
			_ => {}
		}
	}
	var $result = candidates
	var $previously_known = Bool.False
	while $at < end {
		choice = $choices.get($at) ?? empty
		if $previously_known and choice.known {
			match run.get($at) {
				Ok({ edge: Edge(at), scalar: _ }) => {
					$result = $result.append(at)
				}
				_ => {}
			}
		}
		$previously_known = choice.known
		$at = choice.end
	}
	$result
}

choose_word : List(RunScalar), List(TrieNode), List(Choice), U64, Choice, Choice -> Choice
choose_word = |run, nodes, choices, start, unknown, empty| {
	var $best = unknown
	var $at = start
	var $parent = 0.U64
	while $at < run.len() {
		item = run.get($at) ?? { scalar: 0, edge: Interior }
		child = trie_child(nodes, $parent, item.scalar)
		if child == 0 {
			break
		}
		$at = $at + 1
		$parent = child
		at_edge = match run.get($at) {
			Ok({ edge: Interior, scalar: _ }) => Bool.False
			_ => Bool.True
		}
		if at_edge and (nodes.get(child) ?? node(0, 0)).terminal {
			suffix = choices.get($at) ?? empty
			proposed = { unknown: suffix.unknown, tokens: suffix.tokens + 1, end: $at, known: Bool.True }
			if better(proposed, $best) {
				$best = proposed
			}
		}
	}
	$best
}

better : Choice, Choice -> Bool
better = |candidate, best| {
	if candidate.unknown != best.unknown {
		candidate.unknown < best.unknown
	} else if candidate.tokens != best.tokens {
		candidate.tokens < best.tokens
	} else {
		candidate.end > best.end
	}
}

merge_candidates : Str, List(TextPosition) -> List(LineBreak.BreakOpportunity)
merge_candidates = |source, candidates| {
	if candidates.is_empty() {
		return LineBreak.opportunities_with(source, PreserveGraphemes)
	}
	var $result = []
	var $index = 0.U64
	for boundary in LineBreak.iter_boundaries_with(source, PreserveGraphemes) {
		at_match = match candidates.get($index) {
			Ok(at) => at.is_eq(boundary.at)
			Err(_) => Bool.False
		}
		if at_match {
			$index = $index + 1
		}
		match boundary.decision {
			Mandatory => {
				$result = $result.append({ at: boundary.at, authority: boundary.authority, decision: Mandatory })
			}
			Allowed => {
				$result = $result.append({ at: boundary.at, authority: boundary.authority, decision: Allowed })
			}
			Prohibited => {
				if at_match and boundary.authority == Tailorable {
					$result = $result.append({ at: boundary.at, authority: Tailorable, decision: Allowed })
				}
			}
		}
	}
	$result
}
