# Test database provenance

These are ordinary SQLite files produced by the C CLI built from repository
commit `753df3d91adc227b9ae503458212efea12c44af7` (SQLite 3.54.0).
They are committed so the Idris test suite does not need a second database
engine at test time.

| File | Purpose |
|---|---|
| `basic.db` | One table leaf; NULL, integers, reals, UTF-8, and blobs |
| `multipage.db` | 200 rows and an interior/leaf table B-tree |
| `deleted.db` | Valid stale interior separators after boundary-row deletes |
| `overflow.db` | A 2,002-character value using overflow pages |
| `altered.db` | Old physical row missing a trailing column added later |
| `invalid-text.db` | TEXT storage containing the raw byte `0x80` |
| `invalid-schema.db` | One supported table plus a schema identifier containing raw `0x80` |
| `control-text.db` | Valid text containing NUL, newline, and quote bytes |
| `empty.db` | Initialized format-3 database with schema/encoding header values 0 |
| `empty-table.db` | Empty table used to check name validation without rows |

The suite also mutates copies in memory to test WAL-header rejection,
fragment-count bounds, and interior-separator range corruption. The committed
files themselves all pass the C engine's `PRAGMA integrity_check`.

To regenerate them with a C SQLite CLI built from this checkout:

```sh
cd idris
fixtures/regenerate.sh /path/to/sqlite3
```
