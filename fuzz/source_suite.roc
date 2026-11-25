app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.8.0/BUNcJDHxCd7xLVSe3WBioyXqR6tm1MqXwzwGgtnmqK43.tar.zst",
	unicode: "../package/main.roc",
}

import fuzz.Fuzz
import Utf8Target
import Utf8Model
import Utf8LimitProof
import GraphemeTarget
import WordTarget
import SentenceTarget
import SentenceModel
import LineBreakTarget

target = Fuzz.suite([
	Utf8Target.target,
	Utf8Model.target,
	GraphemeTarget.target,
	WordTarget.target,
	SentenceTarget.target,
	LineBreakTarget.target,
])
