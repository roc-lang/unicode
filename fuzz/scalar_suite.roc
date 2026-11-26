app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.8.0/BUNcJDHxCd7xLVSe3WBioyXqR6tm1MqXwzwGgtnmqK43.tar.zst",
	unicode: "../package/main.roc",
}

import fuzz.Fuzz
import CaseTarget
import CollationTarget
import NormalizationTarget
import PropertyTarget

target = Fuzz.suite([
	CaseTarget.target,
	CollationTarget.target,
	NormalizationTarget.target,
	PropertyTarget.target,
])
