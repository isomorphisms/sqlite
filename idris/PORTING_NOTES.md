# Porting notes: source organization, generators, and line budget

Analysis target: repository commit
`753df3d91adc227b9ae503458212efea12c44af7`.

## The honest line comparison

The amalgamation is not the maintained conceptual source.

| Scope | Physical lines |
|---|---:|
| Generated `sqlite3.c` | 269,839 |
| Hand-maintained non-test core implementation/header/grammar files | 179,754 |
| Approximate nonblank, noncomment core code | 115,980 |
| Extension inputs included by the default amalgamation | 69,719 |
| Core `test/*.test` scripts | 486,692 |

An honest point estimate for full core parity is about **90,000 maintained
Idriç lines**, with **75,000–115,000** a credible range. A Linux-only engine
that deliberately drops historical allocator, platform, and configuration
variants might be **60,000–80,000**. Reproducing the present default extension
set likely makes the total **125,000–155,000**.

The earlier idea of a 10,000-line full SQLite is not credible. SQL semantics,
code generation, and planning alone occupy 68,592 physical lines, and the VDBE
has about 190 opcode handlers. Idris types can remove invalid states and a lot
of C bookkeeping. They cannot remove the behavior.

## Human conceptual partition

This partition accounts for all 179,754 core physical lines without counting a
file twice.

| Conceptual area | Lines | Principal source |
|---|---:|---|
| Tokenizer and grammar | 3,433 | `tokenize.c`, `parse.y`, `complete.c` |
| SQL semantics, compiler, planner | 68,592 | `prepare.c`, `resolve.c`, `expr.c`, `build.c`, statement files, `where*.c` |
| VDBE abstract machine | 25,595 | `vdbe*.c`, `vdbe.h`, `vdbeInt.h` |
| B-tree | 13,919 | `btree.c`, `btree.h`, `btreeInt.h`, `backup.c` |
| Pager, WAL, page cache | 16,814 | `pager.c`, `wal.c`, `pcache*.c`, journals |
| VFS, OS, concurrency | 18,637 | `os*.c`, mutex, thread files |
| Core support and API machinery | 25,048 | `main.c`, `sqliteInt.h`, memory, utility, functions |
| Built-in JSON/dbstat/dbpage/carray | 7,716 | corresponding `src/*.c` files |
| **Total** | **179,754** | |

The operative architecture is:

```text
SQL
  -> tokenizer and Lemon parser
  -> semantic compiler and query planner
  -> VDBE bytecode
  -> B-tree cursors
  -> pager, cache, rollback/WAL
  -> VFS and operating system
```

The private headers—`vdbeInt.h`, `btreeInt.h`, and `whereInt.h`—confirm that
these are intentional human subsystem boundaries. `sqlite3.c` is a deployment
and C-optimization form, not the design model to translate.

## Generated-source dependency graph

### Public API

`src/sqlite.h.in`, `VERSION`, `manifest`, and `manifest.tags` feed
`tool/mksourceid.c` and `tool/mksqlite3h.tcl`, producing the 14,433-line
`sqlite3.h`. Extension API headers are incorporated into it.

### SQL parser

The authoritative grammar is the 2,163-line `src/parse.y`. `tool/lemon.c` with
`tool/lempar.c` produces:

- `parse.c`: 6,328 lines
- `parse.h`: 186 lines

The generated C is not a second source of semantics and should not be ported.

### VDBE opcodes

`tool/mkopcodeh.tcl` reads both `parse.h` and annotated `case OP_*` handlers in
`src/vdbe.c`. It derives the 241-line `opcodes.h`, including operand flags and
synopses. `tool/mkopcodec.tcl` turns that into the 208-line reverse-name table
`opcodes.c` used by `EXPLAIN`.

In Idris, one algebraic opcode definition should be authoritative; names,
operand metadata, interpreter cases, and `EXPLAIN` rendering should be derived
from it without generated headers.

### Keywords and PRAGMAs

- compiled `tool/mkkeywordhash.c` produces the 482-line `keywordhash.h`
- `tool/mkpragmatab.tcl` contains declarative PRAGMA definitions and produces
  the 660-line `pragma.h`
- `tool/mkctimec.tcl` produces the compile-option table `ctime.c`

In Idris these should remain declarations/data. A keyword lookup structure can
be derived by the compiler or at build time without becoming maintained code.

### Extensions and shell

- `ext/fts5/fts5parse.y` goes through Lemon; `mkfts5c.tcl` combines its output
  with hand-written FTS5 modules into a 28,073-line `fts5.c`
- `src/shell.c.in` plus CLI extension files go through `mkshellc.tcl` to produce
  a 39,570-line `shell.c`; the shell is not the database library

### Amalgamation

The internal `.target_source` rule copies 141 generated and hand-maintained
inputs into `tsrc/`, removes superseded templates/grammar, and may transform
the VDBE. `tool/mksqlite3c.tcl` then:

- places files in an intentional order
- recursively inlines internal headers once
- suppresses duplicate includes
- adjusts internal/public linkage
- emits `sqlite3.c`

It is therefore more deliberate than blind concatenation. The chosen order
helps the C compiler see inlining opportunities; SQLite's README reports a
performance benefit for the single translation unit. Idris modules and
whole-program optimization make a source amalgamation unnecessary.

At this commit, documentation still mentions `make target_source`, but that
public target does not exist. The working out-of-tree generation path is:

```sh
build_dir=$(mktemp -d)
cd "$build_dir"
/path/to/sqlite/configure
make -j2 sqlite3.c
```

`make sqlite3.c` triggers `.target_source` internally.

## Idris design consequences

- Preserve the conceptual layers, not the amalgamation order.
- Keep the grammar authoritative rather than translating `parse.c`.
- Represent opcodes, page kinds, serial types, cursor states, and transaction
  states as algebraic data.
- Put dependent proofs behind smart constructors returning `Either Corrupt a`;
  callers should manipulate validated pages rather than carry byte-offset
  proofs through ordinary query code.
- Treat untrusted bytes as untrusted until header, bounds, page-kind, varint,
  and record validation have succeeded.
- Keep a small C ABI shim only if binary `sqlite3_*` compatibility becomes a
  requirement.

## Why this first slice is read-only

A read-only table scan crosses the defining boundaries—file header, pager,
B-tree, overflow, records, schema, SQL, and results—without pretending that
pure in-memory rows are SQLite. Correct writes are a large independent problem:
locking, rollback journals, WAL, crash recovery, page allocation, indexes, and
the optimizer all remain.

The present code is smaller than a mature 4,000–6,000-line vertical slice
because its SQL grammar and integrity validation are intentionally narrow. Its
size is evidence only for these implemented behaviors, not a multiplier for a
full-port estimate.
