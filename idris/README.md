# SQLite in Idris: first pass

This is an executable first vertical slice, not a claim that SQLite has been
ported.

It currently opens real SQLite format-3 files, validates the database header,
walks ordinary rowid table B-trees (including interior and overflow pages),
decodes record serial types, reads `sqlite_schema`, parses a deliberately small
`SELECT` language, and returns rows.

```text
SQL text
  -> tokens and AST
  -> sqlite_schema
  -> table B-tree pages
  -> varints and record serial types
  -> projected result rows
```

The test fixtures are databases produced by the C SQLite CLI built from the
same repository commit. They exercise a single leaf, a 200-row interior tree,
UTF-8 and invalid text bytes, every ordinary value family, an added column
missing from an older physical record, an unrelated invalid-UTF-8 schema
identifier, and a record spread across overflow pages.

## Build and run

Idris 2 version 0.8.0 is the tested compiler.

```sh
cd idris
make test
make build
build/exec/sqlite-idris fixtures/basic.db \
  "SELECT rowid, name, age FROM people WHERE age = 85;"
```

Expected query output:

```text
["rowid", "name", "age"]
[2, 'Grace', 85]
```

## What is implemented

- SQLite format-3 signature, page size, usable-size, version, schema-format,
  UTF-8 encoding, payload-fraction, and page-count validation
- Safe page bounds and one-based page numbers
- SQLite one-to-nine-byte varints
- Table B-tree leaf and interior traversal in rowid order
- Interior separator ranges, including valid stale separators after deletes
- Exact table-leaf local-payload calculation
- Overflow chains with bounds, terminal-pointer, cycle, and shared-page checks
- Record serial types 0 through 9, blobs, text, signed big-endian integers,
  IEEE-754 doubles, NaN-to-NULL conversion, validated UTF-8, and preservation
  of invalid text bytes
- `sqlite_schema` decoding that losslessly retains invalid text bytes, so an
  unsupported identifier does not poison otherwise supported tables
- Ordinary rowid tables whose `CREATE TABLE` uses the four basic affinity names
- `SELECT * FROM table`
- Explicit column lists, including `rowid`
- One optional equality predicate: `WHERE column = literal`
- A small separate in-memory SQL-to-instruction-machine scaffold for
  `CREATE`, `INSERT`, and `SELECT`; this is architectural experimentation, not
  the on-disk execution path

## What is not implemented

- Writes, rollback journals, WAL snapshots, locking, or crash recovery
- Headerless zero-byte databases (initialized empty format-3 databases work)
- Index B-trees, query planning, joins, sorting, aggregation, or expressions
- `WITHOUT ROWID` tables
- Quoted identifiers, constraints, declared types beyond `INTEGER`, `REAL`,
  `TEXT`, and `BLOB`, or the full SQLite grammar
- Querying schema objects whose names or `CREATE` statements are not valid
  UTF-8 (their bytes are retained, and other supported tables remain usable)
- SQLite affinity coercions, collations, and three-valued comparison semantics;
  the current equality predicate compares decoded value constructors directly
- UTF-16 databases
- INTEGER PRIMARY KEY rowid-alias reconstruction
- Non-NULL defaults synthesized for fields added after older records were
  written
- Freelist, pointer-map, auto-vacuum, page-checksum, and exhaustive
  freeblock/cell-overlap/free-space integrity checks; the fragmented-byte
  field is range checked but not recomputed
- Cross-checking record serial types 8 and 9 against schema formats 1–3;
  valid files never use those compact constants before schema format 4
- Streaming or efficient large-file access; the first pass intentionally turns
  the file into a pure `List Integer`
- SQLite API or file-format compatibility beyond the behaviors explicitly
  tested here

The important honesty boundary is that the read-only query path reads actual
SQLite files, but it does not yet execute through the prototype VDBE module.

## Modules

| Module | Responsibility |
|---|---|
| `SQLite.File` | Validated immutable database image and pages |
| `SQLite.Page` | B-tree page-header decoding |
| `SQLite.Varint` | SQLite varint encoding and decoding |
| `SQLite.BTreeRead` | Table B-tree and overflow traversal |
| `SQLite.Record` | Serial types, values, doubles, and UTF-8 |
| `SQLite.Schema` | `sqlite_schema` rows and table columns |
| `SQLite.SQL` | Small tokenizer and AST parser |
| `SQLite.ReadOnly` | End-to-end read-only query path |
| `SQLite.VDBE` | Small experimental instruction machine |
| `SQLite.LeafBTree` | Experimental in-memory paged leaf store |

The source-generation and line-budget analysis that set this scope is in
[`PORTING_NOTES.md`](PORTING_NOTES.md).

## Idris and Idriç commits

The executable source is first committed in ordinary Idris and tested with
stock Idris 2. The following notation-only commit converts only:

```text
->  to  →
=>  to  ⇒
```

Those are the only two aliases present in the current Idriç source branch.
The existing Idriç fork cannot bootstrap and is based on Idris 2 0.1.1, so the
Unicode commit is verified by reverse-normalizing a temporary copy and running
the ordinary-Idris suite. It is not described as compiler-tested Idriç.
