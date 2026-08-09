app [run!] {
	pf: platform "../../tests/platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Bidi
import unicode.ScalarRange
import unicode.TextRange

## Read `REPEATS\tLINE_REORDERS\nTEXT`, retaining a complete paragraph on
## every repeat. The checksum makes both a whole-line reorder and a partition
## into many logical line ranges observable.
run! : Str => Str
run! = |input| {
	match input.split_on("\n") {
		[repeat_text, source] => {
			match repeat_text.split_on("\t") {
				[repeats_text, line_reorders_text] => {
					repeats = U64.from_str(repeats_text) ?? return "FAIL\tinvalid repeat count"
					line_reorders = U64.from_str(line_reorders_text) ?? return "FAIL\tinvalid line reorder count"
					if line_reorders < 1 {
						"FAIL\tline reorder count must be positive"
					} else {
						var checksum = 0.U64
						var at = 0.U64
						while at < repeats {
							analysis = Bidi.analyze_paragraph(source, Auto, Bidi.default_limits) ?? return "FAIL\tanalysis"
							paragraph = TextRange.scalar_range(Bidi.paragraph_range(analysis))
							whole_line = Bidi.reorder_line(analysis, paragraph) ?? return "FAIL\twhole line"
							start = ScalarRange.start(paragraph)
							end = ScalarRange.end(paragraph)
							width = end - start
							var line_index = 0.U64
							checksum = checksum + Bidi.entries(analysis).len() + Bidi.visual_to_logical(whole_line).len()
							while line_index < line_reorders {
								line_start = start + width * line_index / line_reorders
								line_end = start + width * (line_index + 1) / line_reorders
								line_range = ScalarRange.from_bounds(line_start, line_end) ?? return "FAIL\tline range"
								line = Bidi.reorder_line(analysis, line_range) ?? return "FAIL\tline reorder"
								checksum = checksum + Bidi.visual_to_logical(line).len()
								line_index = line_index + 1
							}
							at = at + 1
						}
						"${checksum.to_str()}\t${source.count_utf8_bytes().to_str()}"
					}
				}
				_ => "FAIL\tmalformed benchmark header"
			}
		}
		_ => "FAIL\tmalformed input"
	}
}
