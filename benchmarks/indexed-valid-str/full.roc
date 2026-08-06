app [run!] {
    pf: platform "../../tests/platform/main.roc",
    unicode: "../../package/main.roc",
}

import pf.Host
import ScanProbe

run! : Str => Str
run! = |source| {
    before = Host.alloc_count!({})
    signature = ScanProbe.scan(source, U64.highest)
    after = Host.alloc_count!({})
    ScanProbe.render(signature, after - before)
}
