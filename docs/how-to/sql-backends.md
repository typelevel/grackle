# Choose and configure a SQL backend

This how-to is for developers building database-backed mappings. It shows you how to pick a SQL backend (Doobie or Skunk) for Grackle, how the two differ when you declare columns, how to wire a mapping to a connection pool, and how to model the common relational shapes — one-to-many and many-to-one joins, embedded value objects, shared embedded types, and composite-key and associative-table joins. The "why" behind keys, hidden columns and row assembly lives in the [SqlMapping reference](../reference/sql-mapping.md) and the [mapping types reference](../reference/mapping-types.md); this page is the recipe.

## Pick a backend

Grackle's SQL machinery lives in a database-agnostic core trait, `SqlMappingLike[F]`, which defines every field-mapping type (`SqlField`, `SqlObject`, `SqlJson`, `Join`, `ColumnRef`, `TableDef`) and the query-to-SQL compiler. A backend module fills in the concrete `Codec`, `Encoder` and `Fragment` types and a `col` helper. You choose one backend per mapping:

| Backend | Module | Effect / connection | Databases | Notes |
| --- | --- | --- | --- | --- |
| Doobie | `grackle-doobie-pg`, `grackle-doobie-oracle`, `grackle-doobie-mssql` | JDBC `Transactor[F]` | Postgres, Oracle, SQL Server | One artifact per database dialect. JVM only (JDBC). |
| Skunk | `grackle-skunk` | `Resource[F, Session[F]]` | Postgres | Non-blocking, Postgres native protocol. Cross-built for JVM, JS and Native. |

Both backends produce the same `Mapping[F]`, run the same queries, and accept the same `typeMappings`. The only differences you write by hand are the `col` declarations and the abstract base class you build on (a doobie dialect class such as `DoobiePgMapping[F]` over a `Transactor`, or `SkunkMapping[F]` over a `Resource[F, Session[F]]`). Pick Doobie if you need Oracle or SQL Server, or already run JDBC; pick Skunk for a non-blocking, Postgres-only stack.

## Declare columns: Doobie vs Skunk `col`

Every mapped column is a `ColumnRef`. You never build one directly — you call `col(...)` inside a `TableDef`, which captures the table name (via an implicit `TableName`), the column name and the codec. The signature of `col` is where the two backends part ways:

- **Doobie:** `col(name, Meta[T], nullable: Boolean = false)`. Nullability is an explicit flag; the codec is a doobie `Meta[T]`.
- **Skunk:** `col(name, Codec[T])`. Nullability is *inferred* from whether the codec is for `Option[T]` (use `.opt`); there is no `nullable` argument.

Here is a real Doobie set of `TableDef`s from the demo's Postgres world mapping. Each `object` extends `TableDef("...")` to bind its `col` calls to that table:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#db_tables"))
```

Note the nullable columns — `indepyear`, `lifeexpectancy`, `gnp`, `gnpold`, `headofstate`, `capital` — carry `nullable = true`; everything else is non-null. Under Skunk the same three tables would read `col("indepyear", int4.opt)` instead of `col("indepyear", Meta[Int], nullable = true)`, and `col("code", bpchar(3))` instead of `col("code", Meta[String])`; the `nullable = true` columns become `.opt` codecs and the rest stay as their plain codec. Under the hood both backends store a `(codec, Boolean)` tuple where the boolean is the nullability flag — Doobie sets it from your argument, Skunk derives it from `Option[T]`.

One subtlety: `ColumnRef` equality and `hashCode` use only `(table, column)` — the codec is ignored. The same physical column referenced twice is treated as one, which is what lets a parent and a child object both name the join column, but it also means referencing one column with two different codecs will not be flagged as a mismatch.

## Wire the mapping to a connection

`DoobiePgMapping[F]` (and `DoobieOracleMapping[F]`, `DoobieMSSqlMapping[F]`) and `SkunkMapping[F]` are abstract base classes, not traits: each takes its connection in its constructor — a `Transactor[F]` for the Doobie dialects, a `Resource[F, Session[F]]` (typically a pooled session resource) for Skunk. You write your own mapping as a trait that mixes in the corresponding `*Like` trait — `DoobiePgMappingLike[F]` or `SkunkMappingLike[F]`, the database-agnostic core each abstract class extends — then instantiate it against a concrete backend, `new DoobiePgMapping[F](transactor, monitor) with MyDoobieMapping[F]`. The body — `schema`, `TableDef`s and `typeMappings` — is identical between the two backends; only the base class you instantiate and the constructor argument change.

The doobie and skunk modules are not on this site's build classpath, so the sketch below is illustrative rather than compiled:

```scala
// Doobie (Postgres): mix in DoobiePgMappingLike, constructed over a Transactor
trait MyDoobieMapping[F[_]] extends DoobiePgMappingLike[F] {
  val schema = schema"""..."""
  val typeMappings = TypeMappings(/* ObjectMappings + TableDefs */)
}

object MyDoobieMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobiePgMapping[F](transactor, monitor) with MyDoobieMapping[F]
}

// Skunk: mix in SkunkMappingLike, constructed over a pooled Session resource
trait MySkunkMapping[F[_]] extends SkunkMappingLike[F] {
  val schema = schema"""..."""
  val typeMappings = TypeMappings(/* ObjectMappings + TableDefs */)
}

object MySkunkMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with MySkunkMapping[F]
}
```

Each backend ships a `*MappingCompanion` whose `mkMapping(transactor)` / `mkMapping(pool)` defaults the monitor to a no-op, so the common case is a one-liner. You build the `Transactor` (or session `Resource`) the way you normally would in cats-effect, hand it to `mkMapping`, and the resulting `Mapping[F]` is a complete GraphQL endpoint — see [Serve a mapping over HTTP](serve-over-http.md) for routing it. The end-to-end Postgres walkthrough, including running the schema against a live database, is the [DB-backed model tutorial](../tutorial/db-backed-model.md).

## Map a one-to-many and a many-to-one join

A sub-object field is an `SqlObject`. With one or more `Join`s, Grackle emits a SQL join from parent columns to child columns; `Join(parentCol, childCol)` is the single-condition form. The canonical relational mapping is the world schema — `Country` has many cities and many languages, each `City` has one country, and each `Language` joins back to its countries:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlWorldMapping.scala", "#world_typemappings"))
```

Reading the joins:

- **One-to-many** — `Country.cities` is `SqlObject("cities", Join(country.code, city.countrycode))`: from the parent's `code` column to the child's `countrycode`. The list field comes for free; Grackle assembles all matching `city` rows under each country.
- **Many-to-one** — `City.country` is `SqlObject("country", Join(city.countrycode, country.code))`: the *same* column pair in the opposite direction. You declare the join once per direction you want to traverse.

The leaf fields are `SqlField`s pointing at columns on the type's own table. Every object mapping above declares at least one `key = true` field — `Country.code`, `City.id`, `Language.language` — because Grackle needs an identity column to group and assemble rows. Where a column exists only to drive a join or as a key, it carries `hidden = true` so it is fetched and used but never appears in the GraphQL output: `country.code`, `city.id` and `city.countrycode` are all hidden.

`Language.language` is marked `associative = true` as well as `key = true`. An associative key is an identity column that is *not* a database primary key and may repeat across rows (a language code is shared by many countries); marking it associative tells Grackle to group rows correctly anyway. `associative = true` is only valid on a key field — putting it on a non-key triggers `AssocFieldNotKey`.

The GraphQL these joins serve looks like this:

```graphql
query {
  country(code: "GBR") {
    name
    cities {
      name
    }
    languages {
      language
      isOfficial
    }
  }
}
```

## Embed a value object on the parent row

Sometimes a GraphQL sub-object has no table of its own — it is only a grouping of columns that live on the parent's row. That is *embedding*: an `SqlObject` with **no** `Join`. The embedded type's `SqlField`s point back at the parent's table, and the type repeats the parent's key (hidden) so rows still group. The film/series schema embeds a `Synopses` object built from `synopsis_short` and `synopsis_long` columns:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlEmbeddingMapping.scala", "#embedding"))
```

In `FilmType`, `SqlObject("synopses")` has no `Join`, so the `Synopses` object is built from columns on the same `films` row — there is no second table and no extra SQL join. Be careful here: `SqlObject(name)` with no `Join` always means embedding. If you intended a separate table you *must* pass a `Join`, or you will silently get same-row semantics.

## Reuse a shared embedded type across parents

In that same mapping the `Synopses` type is embedded under three different parents — `Film`, `Series` and `Episode` — and each parent stores it in a different table (`films`, `series`, `episodes`). A plain `ObjectMapping` for `Synopses` would be ambiguous: which table's columns should it use? You disambiguate by the GraphQL result-path under which the type is reached, as shown at the bottom of the snippet above:

- `List("films", "synopses")` binds to the `films` columns,
- `List("series", "synopses")` to the `series` columns,
- `List("episodes", "synopses")` to the `episodes` columns.

Grackle picks the right `ObjectMapping` per occurrence by matching the path under which the `Synopses` type is reached. Reach for a path-keyed mapping whenever one embedded GraphQL type appears under multiple parents stored in different tables.

The inlined snippet uses `PrefixedMapping`, which is the legacy form of this: it is kept for backwards compatibility and builds a `PrefixedTypeMatch` predicate under the hood. In new code prefer the path-sensitive `ObjectMapping(path)(...)` constructor, which builds a `PathMatch` — the construct the reference recommends for context-sensitive mappings (see [Mappings and cursors](../concepts/mappings-cursors.md) and the [mapping types reference](../reference/mapping-types.md)). The example above predates `PathMatch`, which is why it still reads `PrefixedMapping`.

## Join on a composite key, and chain through an associative table

A `Join` holds a *list* of `(parentCol, childCol)` condition pairs. The single-pair form is sugar; the list form expresses a composite key, where every pair is `AND`ed into the `ON` clause. The composite-key mapping joins a parent identified by two columns (`key_1`, `key_2`) to its children:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlCompositeKeyMapping.scala", "#composite_key"))
```

`Parent.children` is `SqlObject("children", Join(List((compositeKeyParent.key1, compositeKeyChild.parent1), (compositeKeyParent.key2, compositeKeyChild.parent2))))`. Both parent columns are marked `key = true`, and both equalities are required to match a child row.

A different need — a **many-to-many relationship through an associative table** — is expressed by passing several `Join`s to one `SqlObject`: `SqlObject(name, j1, j2, ...)` chains the joins, parent → associative table → far side. (In the world mapping above, `Country.languages` reaches `countrylanguage` with a single join because the language code lives directly on the link row; when the far side is a third table you add a second `Join` whose parent columns are the associative table's and whose child columns are the target table's.) The two failure modes to watch for are an empty condition list (`NoJoinConditions`) and chained joins whose intermediate tables do not line up (`MisalignedJoins` / `InconsistentJoinConditions`).

## The flags you will reach for, and why join columns are hidden

`SqlField` carries four flags; you will use the first three constantly:

- **`key = true`** — an identity column used to group and assemble rows. Every object type mapping must have at least one (direct or inherited) key, or validation fails.
- **`hidden = true`** — the column is still fetched and used (for a join, a key, or a discriminator) but is omitted from the GraphQL response. Join and key columns are usually hidden so they do not leak into your schema's output; forgetting `hidden = true` on a join column either exposes it or trips the schema-vs-mapping consistency check.
- **`associative = true`** — marks a key that is not a DB primary key and may repeat (see `Language.language` above). Only valid together with `key = true`.
- **`discriminator = true`** — marks the column an interface/union discriminator reads. That is a separate recipe; see [Map interfaces and unions to SQL](interfaces-unions.md).

## Validation rules you will hit

The SQL mapping validator enforces a few structural rules. The two you are most likely to meet while modelling relationships:

- **At least one key per object mapping.** Every (non-interface, non-union) object type mapping must declare at least one `key = true` field, directly or inherited, or you get `NoKeyInObjectTypeMapping` ("Object type mappings must include at least one direct or inherited key field mapping").
- **One table per object mapping.** All `SqlField`/`SqlObject` columns of a single object type mapping must come from the *same* table, or validation fails with `SplitObjectTypeMapping` / `SplitEmbeddedObjectTypeMapping`. To span tables you do not add columns from another table — you add an `SqlObject` + `Join` so the second table is a genuine sub-object.

Validation is deferred to first use: the compiler is built lazily, so a mis-specified mapping will not blow up until the first query is compiled. For the full list of failures and how to force validation early, see [Validate a mapping and read the failures](validate-mappings.md).

## See also

- [Map interfaces and unions to SQL](interfaces-unions.md) — single-table polymorphism with an `SqlDiscriminator`.
- [Map a jsonb column with SqlJson](jsonb-columns.md) — serve a JSON sub-tree without further SQL.
- [Filter, sort and page a field](filtering-ordering-paging.md) — add arguments to your list joins.
- [Serve a mapping over HTTP](serve-over-http.md) — route the resulting `Mapping[F]`.
- [DB-backed model tutorial](../tutorial/db-backed-model.md) — the end-to-end Postgres walkthrough, with a live database.
- [SqlMapping reference](../reference/sql-mapping.md) — exhaustive `SqlField`/`SqlObject`/`Join`/`col` signatures.
