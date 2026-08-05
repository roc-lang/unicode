app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.1.0/ABFgWwu8SwPJfp7tzxDoTL41b1jFeHEac3RxUFSt1WWp.tar.zst",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.Scalar

## Get the number of Unicode scalar values in a valid Roc Str without
## allocating a list of them.
nr_of_scalars : Str -> U64
nr_of_scalars = |str| {
    var $count = 0.U64
    for _ in Scalar.iter(str) {
        $count = $count + 1
    }
    $count
}

main! = |_args| {
    word = "ẇ͓̞͒͟͡ǫ̠̠̉̏͠͡ͅr̬̺͚̍͛̔͒͢d̠͎̗̳͇͆̋̊͂͐"
    count = nr_of_scalars(word)
    Stdout.line!("String \"${word}\" consists of ${count.to_str()} code points.")?
    Ok({})
}
