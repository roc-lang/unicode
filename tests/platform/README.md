# Private test platform

This platform is test infrastructure, not part of the public package API. It
provides a deterministic stdin/stdout boundary and `Host.alloc_count!`, which
counts `roc_alloc` plus `roc_realloc` calls made by the Roc app.

The platform build, ABI bindings, and vendored musl runtime objects are adapted
from `roc-platform-template-zig` 1.1.0 (commit
`7aee8acf8cee0c6ab9fc37f615d68f39460361c6`) under the Universal Permissive
License. See `LICENSE-UPL`.

Build the host for the native target with:

```sh
zig build --build-file tests/platform/build.zig native -Doptimize=ReleaseFast
```
