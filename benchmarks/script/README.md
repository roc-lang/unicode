# Script probes

These opt-in probes exercise the generated Script alias matcher and the
`ConservativeScxV1` complete/chunked itemizers with the repository-pinned Roc
compiler and allocation-counting host. They are review evidence, not CI timing
gates.

`semantic.roc` covers the UAX44-LM3 initial `is` rule, tab whitespace, exact
Unknown/Common boundaries on both sides, and complete/cursor parity.
`allocation.roc` isolates dynamic alias lookup and heap-backed itemization from
input setup so their zero-allocation contract can be measured exactly.

```sh
zig build --build-file tests/platform/build.zig native -Doptimize=ReleaseFast
roc build benchmarks/script/semantic.roc --opt=speed \
    --output=.roc-unicode-tmp/benchmarks/script-semantic --no-cache
printf Greek | .roc-unicode-tmp/benchmarks/script-semantic

roc build benchmarks/script/allocation.roc --opt=speed \
    --output=.roc-unicode-tmp/benchmarks/script-allocation --no-cache
printf AisGreek | .roc-unicode-tmp/benchmarks/script-allocation
printf Iaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \
    | .roc-unicode-tmp/benchmarks/script-allocation
```
