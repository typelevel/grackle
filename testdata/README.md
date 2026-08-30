# Test data

Each directory here is one dataset. It holds a schema per dialect, as `pg.sql`, `oracle.sql`, `mssql.sql`,
`sqlite.sql` and `h2.sql`, and the dataset's rows once, as one `<table>.csv` per table. The schema stays per dialect
because column types and constraints legitimately differ between databases. Only the rows are shared.

At container-up time (see `GenTestData` in `project/`, called from `dockerUp` in `build.sbt`) the schema and the rows
are written together into `target/testdata/<dialect>/<dataset>.sql`, which is what docker compose mounts into the
container's init directory. Nothing is generated into the source tree, and the tests know nothing about any of this.
They just query a database that already has the data in it. SQLite and H2 are the exception to the container part:
neither has a server, so their suites build the scripts themselves and run them against a database they create.

A dataset does not have to be complete. One with no CSVs keeps its rows in the per-dialect scripts, which is where
data belongs when it genuinely cannot be shared, and one with no `<dialect>.sql` is simply skipped for that dialect.

## Format

- `|` separated, with a header naming the columns.
- `\N` means SQL NULL. An empty field means the empty string, except on Oracle, which does not distinguish the two, so
  a column that is `NOT NULL` there cannot hold an empty field.
- Most values are written into the generated script as a string literal and coerced by the database, so the CSV says
  nothing about their type. Numbers are just their text.
- A column whose values the dialects spell differently says so in the header, as `name:kind`:

  | kind          | in the CSV             | pg                     | oracle                                    | mssql                          | sqlite                         | h2                            |
  | ------------- | ---------------------- | ---------------------- | ----------------------------------------- | ------------------------------ | ------------------------------ | ----------------------------- |
  | `array`       | `drama,comedy`         | `'{"drama","comedy"}'` | `string_array2('drama', 'comedy')`        | `'["drama", "comedy"]'`        | `'["drama", "comedy"]'`        | `ARRAY['drama', 'comedy']`    |
  | `date`        | `1974-10-07`           | `'1974-10-07'`         | `DATE '1974-10-07'`                       | `'1974-10-07'`                 | `'1974-10-07'`                 | `'1974-10-07'`                |
  | `time`        | `19:35:00`             | `'19:35:00'`           | `INTERVAL '0 19:35:00' DAY TO SECOND (0)` | `'19:35:00'`                   | `'19:35:00'`                   | `'19:35:00'`                  |
  | `timestamptz` | `2020-05-22T19:35:00Z` | as written             | `TIMESTAMP '2020-05-22 19:35:00 +00:00'`  | `'2020-05-22 19:35:00 +00:00'` | `'2020-05-22 19:35:00 +00:00'` | `'2020-05-22 19:35:00+00:00'` |
  | `boolean`     | `true`                 | `'TRUE'`               | `'TRUE'`                                  | `1`                            | `1`                            | `1`                           |

  An array's elements are separated by commas and quoted like any other CSV field, so an element containing a comma is
  written `"a,b"`. Oracle builds an array by calling its collection type, so the constructor name is read out of the
  column's type in Oracle's own schema. The CSV never names a dialect's type.

- A table can be schema-qualified, as `union-order`'s is. The qualifier differs by dialect (`qualified.` on Postgres
  and SQL Server, `QUALIFIED.` on Oracle), so the CSV is named after the table alone:
  `union-order/union_order_entities.csv`.

## Adding a dataset

`sbt "newDataset foo"` creates the directory with a `CREATE TABLE` skeleton per dialect. Fill the schemas in and put
the rows in `<table>.csv` next to them. Rows are inserted in the order the schema creates the tables, which is also a
safe order for foreign keys.

`sbt checkTestData` renders every dataset without writing anything or starting a database, and reports what does not
line up: a CSV whose table no schema creates, a header naming an unknown kind, a ragged row, a dataset with no scripts
at all.

Recreate the containers before testing (`docker compose up --force-recreate --renew-anon-volumes`): a database image
only runs its init scripts on a first start, so an existing container will not pick up a change.

## Datasets that keep their rows in the scripts

- `mutation` has no rows at all. It only creates a sequence.
- `qualified-names` exists for Postgres only, because it tests schema-qualified names (`CREATE SCHEMA qualified;`).
  One dialect means no duplication to remove.

`qualified-names` and `union-order` have neither a `sqlite.sql` nor an `h2.sql`, so those two skip them. Both put
their tables in a schema, and SQLite has nothing to create one with.
