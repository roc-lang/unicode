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

Windows MSVC links supply their own process entry point, PE TLS directory, and
Win32 import archive. The native build generates `windows_imports.lib` from the
selected Zig toolchain's bundled mingw-w64 definitions; it does not depend on
an installed Windows SDK. Python 3 is required for that build step. The generated
import definitions and mingw-w64 license remain beside the generated archive.
The TLS directory is adapted from Zig 0.16.0's `lib/std/os/windows/tls.zig` under
`LICENSE-ZIG`.

A Windows link can also be checked from another host:

```sh
zig build --build-file tests/platform/build.zig native -Dtarget=x86_64-windows-msvc -Doptimize=ReleaseFast
```
