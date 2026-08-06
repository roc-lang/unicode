app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import pf.Host
import unicode.Script
import unicode.ScriptItemization

run! : Str => Str
run! = |prefixed| {
    input_len = prefixed.count_utf8_bytes()
    if input_len == 0 {
        return "invalid"
    }
    selector = prefixed.drop_last_bytes(input_len - 1) ?? return "invalid"
    source = prefixed.drop_first_bytes(1) ?? return "invalid"
    before = Host.alloc_count!({})
    checksum = if selector == "A" {
        match Script.from_alias(source) {
            Ok(script) => Script.short_alias(script).count_utf8_bytes()
            Err(_) => 0
        }
    } else {
        ScriptItemization.fold_runs(
            source,
            ScriptItemization.default,
            0.U64,
            |count, _run| count + 1,
        )
    }
    after = Host.alloc_count!({})
    "allocations=${(after - before).to_str()};checksum=${checksum.to_str()}"
}
