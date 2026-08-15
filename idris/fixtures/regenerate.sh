#!/bin/sh
set -eu

sqlite_cli=${1:?usage: regenerate.sh /path/to/sqlite3}
fixture_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

"$sqlite_cli" "$work_dir/basic.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE people(name TEXT, age INTEGER, score REAL, note BLOB);
   INSERT INTO people VALUES('Ada',36,9.5,x'00FF');
   INSERT INTO people VALUES('Grace',85,NULL,x'');
   INSERT INTO people VALUES('Évariste',20,-1.25,x'CAFEBABE');"

"$sqlite_cli" "$work_dir/multipage.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE nums(n INTEGER, label TEXT);
   WITH RECURSIVE c(x) AS
     (VALUES(1) UNION ALL SELECT x+1 FROM c WHERE x<200)
   INSERT INTO nums SELECT x, printf('row-%03d',x) FROM c;"

"$sqlite_cli" "$work_dir/deleted.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE nums(n INTEGER);
   WITH RECURSIVE c(x) AS
     (VALUES(1) UNION ALL SELECT x+1 FROM c WHERE x<200)
   INSERT INTO nums SELECT x FROM c;
   DELETE FROM nums WHERE rowid % 17 = 0;"

"$sqlite_cli" "$work_dir/overflow.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE docs(body TEXT);
   INSERT INTO docs VALUES(printf('%.*c',2000,'x') || ' λ');"

"$sqlite_cli" "$work_dir/altered.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE t(a INTEGER);
   INSERT INTO t VALUES(1);
   ALTER TABLE t ADD COLUMN c TEXT;
   INSERT INTO t VALUES(2,'new');"

"$sqlite_cli" "$work_dir/invalid-text.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE t(value TEXT);
   INSERT INTO t VALUES(CAST(x'80' AS TEXT));"

"$sqlite_cli" "$work_dir/invalid-schema.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE t(a INTEGER);
   INSERT INTO t VALUES(9);
   CREATE TABLE badname(x TEXT);"
"$sqlite_cli" "$work_dir/invalid-schema.db" \
  ".dbconfig defensive off" \
  "PRAGMA writable_schema=ON;
   UPDATE sqlite_schema
      SET name=CAST(x'626164806e616d65' AS TEXT),
          tbl_name=CAST(x'626164806e616d65' AS TEXT),
          sql=CAST(x'435245415445205441424c452022626164806e616d65222878205445585429' AS TEXT)
    WHERE name='badname';
   PRAGMA writable_schema=OFF;" >/dev/null

"$sqlite_cli" "$work_dir/control-text.db" \
  "PRAGMA page_size=512; VACUUM;
   CREATE TABLE t(s TEXT);
   INSERT INTO t VALUES(CAST(x'4100420A27' AS TEXT));"

"$sqlite_cli" "$work_dir/empty.db" "PRAGMA user_version=1;"
"$sqlite_cli" "$work_dir/empty-table.db" \
  "PRAGMA page_size=512; VACUUM; CREATE TABLE t(a INTEGER);"

for database in "$work_dir"/*.db; do
  result=$("$sqlite_cli" "$database" "PRAGMA integrity_check;")
  test "$result" = "ok"
done

install -m 0644 "$work_dir"/*.db "$fixture_dir"/
