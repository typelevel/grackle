# Map a jsonb column with SqlJson

This how-to shows you how to serve a GraphQL object subtree from a single `jsonb` column. With `SqlJson` you map a GraphQL object or interface field to one JSON column; Grackle fetches the JSON once and navigates every nested selection — nested objects, enums, arrays, fragments, interface members and `__typename` — entirely in-process, with no further SQL. It is for developers who store semi-structured data in a JSON column and want to expose its shape through GraphQL without flattening it into tables. You will see how to declare the column codec, map it, descend into the JSON from a query, handle nullable versus non-null fields, and where the technique stops (you cannot join, filter or order *inside* the JSON). The last section covers `CursorFieldJson`, the cross-backend escape hatch for synthesising JSON from a non-`jsonb` column. This recipe assumes you can already build a SQL mapping with `SqlField`/`SqlObject` — see [Choose and configure a SQL backend](sql-backends.md) for the setup.

## Declare a jsonb column codec

A `jsonb` column is mapped like any other column: you declare it inside a `TableDef` with `col`, passing a codec for `io.circe.Json`. The backends differ only in how you spell the codec and its nullability:

- **doobie** — `col(name, jsonbMeta, nullable = …)`, where `jsonbMeta: Meta[Json]` comes from `doobie-postgres-circe` (`doobie.postgres.circe.jsonb.implicits`). Nullability is an explicit `nullable` flag on `col`.
- **skunk** — `col(name, ccodec.jsonb)`, where `ccodec.jsonb: skunk.Codec[Json]` comes from `skunk-circe`. Nullability is inferred from whether the codec is for `Option[Json]` (use the column's `nullable`/`.opt` wrapper).

In both cases the column carries `io.circe.Json`; the JSON sub-tree is decoded into a circe value and navigated by a `CirceCursor`. The test mapping below uses a `nullable(jsonb)` codec, so the underlying column may be SQL `NULL`.

## Map it with SqlJson to a GraphQL object

`SqlJson(fieldName, columnRef)` maps a GraphQL object- or interface-typed field to a single JSON column. Its signature is minimal:

```scala
case class SqlJson(fieldName: String, columnRef: ColumnRef)
```

`SqlJson` is always visible (`hidden = false`) and always a subtree (`subtree = true`); that flag tells the SQL compiler not to map the field's children to columns, because they are resolved from the decoded JSON instead. The following test mapping declares a `records` table with one `jsonb` column and maps a `Row` GraphQL object over it:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlJsonbMapping.scala", "#jsonb"))
```

The `records` table has an `id` column and a `record` column typed `nullable(jsonb)`. The schema's `Row` type has a scalar `id` plus two object-typed fields — `record: Record` (nullable) and `nonNullRecord: Record!` (non-null) — and the `Record` type itself nests scalars, an enum (`Choice`), an array (`arrary: [Int!]`), a sub-object (`object: A`) and an interface (`children: [Child!]!` over `interface Child` with members `A` and `B`). The `Row` `ObjectMapping` maps `id` with an ordinary `SqlField(key = true)` and then maps both `record` and `nonNullRecord` with `SqlJson` — pointing both at the *same* `records.record` column. None of the columns for `Record`, `Choice`, `A`, `B` or `Child` are mapped; Grackle does not need them, because everything below an `SqlJson` field comes out of the JSON document.

Note that the `Record`, `Choice`, `A`, `B` and `Child` types appear only in the schema — there are no `ObjectMapping`s for them. Their fields are matched against the decoded JSON by name, so the JSON stored in the column must use the same field names and shape as the GraphQL types.

## Nested objects, enums, arrays and fragments inside the JSON

Once a field is mapped with `SqlJson`, a client navigates the JSON purely through the GraphQL query. The query selects into `record` exactly as it would into any object field; Grackle resolves each step against the decoded circe value. Here is a query that descends two levels — into the JSON object and then into its nested `object` sub-object — together with the response:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlJsonbSuite.scala", "#jsonb_query"))
```

The query asks for `record { record { object { id aField } } }`: the outer `record` is the `Row.record` field (the whole JSON document), and the inner `object` is the `Record.object` field resolved from the JSON. The response pulls `id` and `aField` straight out of the nested JSON object — no join, no second query. The same descent works for the other nested shapes in the schema: a list field decodes the matching JSON array, selecting `choice` yields an enum value (the JSON string is matched against the `Choice` enum), and inline fragments or named fragments on `children` select interface members. The match is by field name, so each GraphQL field reads the JSON key of the same name. Because the navigation is in-process, anything circe can decode into the field's GraphQL type just works.

## Interface members and __typename inside the JSON

`children: [Child!]!` is an interface-typed list, and `Child` is resolved from the JSON like everything else under `SqlJson`. To pick out concrete members you use ordinary GraphQL type conditions, and `__typename` reports the concrete type read from the JSON:

```graphql
query {
  record(id: 1) {
    record {
      children {
        __typename
        id
        ... on A { aField }
        ... on B { bField }
      }
    }
  }
}
```

There is no `SqlDiscriminator` and no discriminator column here. Unlike a SQL-backed interface — where you map all subtypes onto a shared table and use an `SqlDiscriminator` to choose the concrete type per row (see [Map interfaces and unions to SQL](interfaces-unions.md)) — an interface *inside* JSON is discriminated by the circe cursor from the JSON itself. Each element of the array carries enough structure for Grackle to match it to `A` or `B`, and `__typename` plus the `... on A` / `... on B` fragments resolve against that. This is the same behaviour as the [circe JSON backend](circe-backend.md), because below an `SqlJson` field you are running a circe value mapping.

## Nullable vs non-null SqlJson, and the "expected jsonb value" error

The example maps the *same* `records.record` column to two GraphQL fields with different nullability: `record: Record` (nullable) and `nonNullRecord: Record!` (non-null). The same column backing both is legal — the GraphQL type of the field decides how the decoded value is wrapped. When resolving an `SqlJson` field Grackle inspects the fetched value:

- A `Json` value is wrapped in a `CirceCursor` and navigated as described above. This holds for both the nullable and the non-null field.
- A SQL `NULL` (no value fetched) decodes to a `Json.Null` cursor, which surfaces as GraphQL `null`.
- Anything else — a value that is neither JSON nor absent — is a contract violation. Grackle raises an internal error whose message is `expected jsonb value found …`.

That error is an `InternalError`, so — per Grackle's [error model](errors.md) — it is raised into the effect `F` rather than appearing in the response `errors` array. In practice this only happens when the column does not actually hold JSON (a codec or column-type mismatch). Map a column whose value is always valid `jsonb` to an `SqlJson` field, and use a nullable GraphQL field for a column that can be SQL `NULL`.

## Limitation: no SQL joins, filters or ordering inside the sub-tree

Everything below an `SqlJson` field is navigated by an in-memory `CirceCursor`, not compiled to SQL. The consequence is a hard boundary:

- You **cannot** `Join` out of a field inside the JSON to another table — there is no column for the SQL compiler to join on.
- You **cannot** `Filter`, `OrderBy` or page on a field inside the JSON. Those nodes compile to SQL `WHERE`/`ORDER BY`/`LIMIT` clauses (see [Filter, sort and page a field](filtering-ordering-paging.md)), and the JSON sub-tree is opaque to SQL.
- You **can** filter, order and join on the *row* that owns the column (`Row.id` here), because that is an ordinary `SqlField`.

If you need to filter or order by something stored inside the document, that something belongs in its own column or table, not in `jsonb`. `SqlJson` is for serving a self-contained JSON blob whole; use real `SqlObject` + `Join` mappings for anything you need the database to query across.

## Cross-backend: CursorFieldJson to synthesise JSON from a non-jsonb column

Sometimes the source column is not `jsonb` at all — it holds an encoded value (an integer, a packed string) that you want to *project* into JSON at read time. `CursorFieldJson` is the tool for that. It is a circe field mapping (`grackle.circe.CursorFieldJson`), reusable inside a SQL mapping, with this shape:

```scala
case class CursorFieldJson(
  fieldName: String,
  f: Cursor => Result[Json],
  required: List[String],
  hidden: Boolean = false)
```

You give it a function `Cursor => Result[Json]` that builds the JSON from already-fetched fields, plus the list of field names that function `required`s (so Grackle knows to fetch them). The following mapping stores a category code in an ordinary `int4` column and decodes it into a JSON array of `Category` objects:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlCursorJsonMapping.scala", "#cursor_json"))
```

The `brands` table has an `int4` `categories` column. The mapping maps it twice on the `Brand` type: once as a hidden `SqlField("encodedCategories", brands.category, hidden = true)` (so the raw integer is fetched but not exposed), and once as `CursorFieldJson("categories", decodeCategories, List("encodedCategories"))`. The `decodeCategories` function reads the hidden field with `c.fieldAs[Int]("encodedCategories")`, turns the integer into a `List[Category]` via `decodeCategoryInt`, and serialises it with circe's `.asJson` (using the `categoryEncoder` instance defined alongside). The `required = List("encodedCategories")` argument tells Grackle the function depends on that hidden field, so it is included in the fetch. The GraphQL `categories: [Category!]` field then resolves from the synthesised JSON, exactly as if it had come from a real `jsonb` column.

Like `SqlJson`, the resulting JSON is navigated in-process, so the same limitation applies: you cannot join, filter or order inside the JSON that `CursorFieldJson` produces.

## See also

- [Choose and configure a SQL backend](sql-backends.md) — the doobie/skunk setup, table defs and codecs that `SqlJson` builds on.
- [Map interfaces and unions to SQL](interfaces-unions.md) — the `SqlInterfaceMapping`/`SqlDiscriminator` approach for interfaces backed by real columns, contrasted with interfaces inside JSON.
- [Serve GraphQL from circe JSON](circe-backend.md) — the circe value mapping you are effectively running below every `SqlJson` field, including `CursorFieldJson`.
- [Filter, sort and page a field](filtering-ordering-paging.md) — the `Filter`/`OrderBy`/`Limit` nodes that work on real columns but not inside a JSON sub-tree.
- [Construct, accumulate and report errors](errors.md) — why the "expected jsonb value" internal error surfaces in `F`, not the GraphQL `errors` array.
- [SqlMapping reference](../reference/sql-mapping.md) — exact signatures for `SqlJson`, `SqlField`, `SqlObject` and the rest of the SQL field-mapping ADT.
