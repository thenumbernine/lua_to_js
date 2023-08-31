### Lua to JavaScript Transpiler

Unfinished but easy to work with.  It works by adding a layer of serialization to the AST in my [lua-parser](https://github.com/thenumbernine/lua-parser) library. 

This has so much in common with my Lua to C project that I should really just find some generic AST superset of all languages and write an exporter to/from each language.
(Kind of like I am already doing in my SymMath project's equation exporter / code generation).

### Goals:

Porting my LuaJIT code to the Web.

Mind you that means for now I'm only targetting Lua 5.1 and 5.2.  But don't worry, the underlying `lua-parser` project supports up to Lua 5.4.

### Alternatives:

- All those Lua FFI libraries.  I'm not using these because in the `ffi.cdef` struct defining, none of them seem to support bitfields, and I'm betting they probably don't support attributes like `packed` which I depend on.
	- Also a tiny quirk, all of them must specify a userdata of the null pointer and implement it in their ffi library, usually as `ffi.null` or `ffi.nullptr`.  LuaJIT itself supports the comparison `ptr == nil`, which vanilla Lua would by default return `false`.  Rather than clean up all my code, I might as well try to implement my own fix.

- WebVM.  This runs LuaJIT, and runs it pretty fast, but from my small poking around at `ffi` loading `.so`'s, it looked a bit problematic. 
	- Also, in order to use this you have to modify a 600 MB disk image.  That means any app posted with this is at least a 600 MB download.

- Building LuaJIT for WebASM. Lots of folks say this would require disabling JIT because of the code-rewriting which some articles are saying aren't allowed on WebASM's design.
Though a few articles on latest WebASM versions / features claim there are new features enabled specifically for JIT's and to allow this.
I haven't jumped into the mix to sort it out yet.
	- Until then, I was thinking a quick fix might be to just disable the JIT, but from there you still have to deal with porting the target `lj_vm.S` from its supported architectures over to WebASM.
	- Or maybe rewrite `lj_vm` into C code, so Emscripten could handle it?  Either way, without JIT it is still facing a potential big performance loss.

### Features:

- All the Lua metatable stuff, as described in the manual 5.1.  Maybe I'll bump my target up to 5.2.  I know the Lua code doesn't 100% match the Lua manual description.  Maybe I'll dig into it and iron out the kinks.

- Multiple assignment statements work, even with the `__newindex` metamethod, all except for the final argument unpacking into multiple lhs's.

- All LuaJIT types except threads, cdata, userdata.

- Tables are being wrapped in a JS object so their metatable can be stored (without bumping into any fields in the table).
	- Keys and values are stored as an array-of-pairs instead of JS key/values, because JS does not support all possible types of keys that Lua does (functions, objects, etc).  I'll work on optimal implementations later, but for now the focus is correctness of behavior.

- For-loops.  Numeric `for ... = ...` loops work fine.  Iterator-based `for ... in ...` loops are still in the works, and contingent upon multiple-return assignment.

- Functions don't have a steady way of returning multiple values.  This might change by me wrapping all JS functions such that they always return arrays.

### Depends:

- [lua-parser](https://github.com/thenumbernine/lua-parser)
- [lua-ext](https://github.com/thenumbernine/lua-ext)
