# Map interfaces and unions to SQL

This how-to shows you how to serve polymorphic GraphQL types — interfaces and unions — from a single relational table. You map every subtype's columns onto one shared table, mark the column that says which concrete type a row is with `discriminator = true`, and supply an `SqlDiscriminator` that both narrows fetched rows to a subtype and produces a `WHERE` predicate for a given subtype. It is for developers who already have `SqlMapping`s over their tables (see [Choose and wire a SQL backend](sql-backends.md)) and now need `... on T` inline fragments to resolve correctly. For the validation rules that enforce the single-table requirement, see [Validate your mappings](validate-mappings.md).

## Map an interface with `SqlInterfaceMapping`

An interface is mapped with `SqlInterfaceMapping`, which behaves like an `ObjectMapping` but carries a discriminator. All of the interface's fields, and all of its subtypes' extra fields, are columns on **one** shared table. The interface mapping holds the shared columns; each concrete `type … implements …` becomes an ordinary `ObjectMapping` that adds only its own extra columns on that same table.

The example below maps an `Entity` interface implemented by `Film` and `Series`, all stored in one `entities` table. The GraphQL schema is:

```graphql
interface Entity {
  id: ID!
  entityType: EntityType!
  title: String
  synopses: Synopses
  imageUrl: String
}
type Film implements Entity {
  id: ID!
  entityType: EntityType!
  title: String
  synopses: Synopses
  imageUrl: String
  rating: String
  label: Int
}
type Series implements Entity {
  id: ID!
  entityType: EntityType!
  title: String
  synopses: Synopses
  imageUrl: String
  numberOfEpisodes: Int
  episodes: [Episode!]!
  label: String
}
enum EntityType { FILM SERIES }
```

The mapping that backs it:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlInterfacesMapping.scala", "#interfaces"))
```

The key points:

- `SqlInterfaceMapping(tpe = EType, discriminator = entityTypeDiscriminator, fieldMappings = …)` maps the `Entity` interface. Its `fieldMappings` are exactly the interface's own fields: `id` (a `key`), `entityType` (the `discriminator = true` column), `title`, and the embedded `synopses` sub-object.
- The discriminating column carries `discriminator = true`. Grackle reads it per row to decide the concrete type; it is otherwise a normal `SqlField`. It must not itself be polymorphic.
- `FilmType` and `SeriesType` are plain `ObjectMapping`s that contribute each subtype's *extra* fields — `rating`/`label`/`imageUrl` for `Film`, `numberOfEpisodes`/`episodes`/`label`/`imageUrl` for `Series` — and every one of those columns comes from the same `entities` table (or a join out of it, as with `Series.episodes`). A subtype inherits the interface mapping's shared fields (`id`, `synopses`), so it need not repeat them; it *may* re-declare one when it wants its own mapping for it (here `Series` re-maps `title` to the same column, and supplies `imageUrl` via a `CursorField` over a hidden column rather than a plain `SqlField`).
- A `PrefixedMapping` is used here for the embedded `Synopses` type because it appears under several parents (`entities`, `films`, `episodes`) backed by different tables; that is orthogonal to the interface mechanism — see [Compose and reuse mappings](compose-mappings.md). `PrefixedMapping` is the legacy form, shown here only because this inlined test mapping predates the newer construct. For new code prefer a path-sensitive `PathMatch` (built by the `ObjectMapping(path)(…)` constructor); see [Mappings and cursors](../concepts/mappings-cursors.md) and the [mapping types reference](../reference/mapping-types.md).

## Write the `SqlDiscriminator`

`SqlInterfaceMapping` (and `SqlUnionMapping`) take an `SqlDiscriminator`, the strategy that connects a row to its concrete GraphQL type. It has two methods:

```scala
trait SqlDiscriminator {
  def discriminate(cursor: Cursor): Result[Type]   // concrete type of a fetched row
  def narrowPredicate(tpe: Type): Result[Predicate] // WHERE predicate selecting one subtype
}
```

`discriminate` runs **after** a row is fetched: given the `Cursor`, it reads the discriminator column and returns the matching subtype `Type`. `narrowPredicate` runs **before** the query is executed: given a subtype the query asked for (via `... on Film`), it returns a `Predicate` that restricts the SQL to rows of that subtype, so Grackle does not fetch and then discard non-matching rows. Both return a `Result`, so each cleanly signals failure for a subtype it does not recognise (see the catch-all below).

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlInterfacesMapping.scala", "#discriminator"))
```

Reading this:

- `discriminate` calls `c.fieldAs[EntityType]("entityType")` to pull the discriminator value out of the `Cursor` (decoded through the column's codec into the `EntityType` enum), then maps `Film`/`Series` enum cases to `FilmType`/`SeriesType`.
- `narrowPredicate` builds `Eql(EType / "entityType", Const(EntityType.Film))` for `FilmType` and the analogous predicate for `SeriesType`. `EType / "entityType"` is the path to the discriminator field; `Const(...)` is the value to match. `.success` lifts the predicate into `Result`.
- The catch-all returns `Result.internalError(...)`. An `InternalError` is raised into the effect `F` — it is not added to the GraphQL `errors` array — so reaching it signals a mapping bug, not a client error.

## Map a union with `SqlUnionMapping`

A union has no fields of its own, so its mapping is leaner — but the single-table rule and the discriminator still apply, and there are two extra constraints: every field mapping in the union mapping must be **hidden**, and the union mapping may hold only the key and the discriminator. Each member type maps its own column on the shared table through an ordinary `ObjectMapping`.

The example maps `union Item = ItemA | ItemB`, where both members live in one `collections` table:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlUnionsMapping.scala", "#unions"))
```

Note how this differs from the interface case:

- The `collections` table has one column per member's payload (`itema`, `itemb`) plus the `id` key and the `item_type` discriminator.
- `SqlUnionMapping(tpe = ItemType, …)` carries **only** two fields, and both are `hidden = true`: the `id` key and the `itemType` discriminator (`discriminator = true`). A non-hidden field in the union mapping, or a `SqlObject` sub-object or `SqlJson` field placed there, fails validation — the member payloads belong on the member `ObjectMapping`s, not here.
- Each member — `ItemAType`, `ItemBType` — is a normal `ObjectMapping` that repeats the hidden `id` key (so rows group correctly) and maps its own visible column (`itema`, `itemb`).
- `itemTypeDiscriminator` is an `SqlDiscriminator` exactly as before; here `discriminate` reads the `itemType` column as a `String` and `narrowPredicate` compares `ItemType / "itemType"` against `Const("ItemA")` / `Const("ItemB")`.

## Query with `... on T` and `__typename`

Clients select members through GraphQL inline fragments. Each `... on ItemA { … }` block applies only to rows whose discriminator resolves to `ItemA`; `__typename` reports the concrete type Grackle picked. The following query and its response come straight from the union test suite:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlUnionSuite.scala", "#union_query"))
```

The query selects `itema` only inside `... on ItemA` and `itemb` only inside `... on ItemB`; the response carries exactly the fields for whichever member each row turned out to be, plus the `__typename`. This works the same for interfaces: select shared interface fields directly on the field and put subtype-specific fields inside `... on Film` / `... on Series`. Internally these inline fragments compile to `Narrow(SubType, …)` nodes and `__typename` to an `Introspect` node, the same algebra produced by named fragments and resolved by the discriminator. For how fragments and `__typename` are elaborated in general, see [Introspection, fragments and variables](../concepts/introspection-fragments-variables.md).

## Validation errors to expect

Grackle validates interface and union mappings up front (see [Validate your mappings](validate-mappings.md)). The errors specific to polymorphic mappings are:

- **`SplitInterfaceTypeMapping` / `SplitUnionTypeMapping`** — a subtype's columns come from more than one table. All implementors/members must share the interface/union's table; to span tables, expose the extra data as a joined `SqlObject` sub-object rather than a bare column.
- **`NoDiscriminatorInObjectTypeMapping`** — the interface or union mapping has no field marked `discriminator = true`. Every polymorphic mapping needs a discriminator column.
- **`IllegalPolymorphicDiscriminatorFieldMapping`** — the discriminator field is itself polymorphic. The discriminator must resolve to a single concrete value per row.
- **`NonHiddenUnionFieldMapping`** — a field mapping inside a `SqlUnionMapping` is not `hidden = true`. The union mapping may expose only hidden key and discriminator fields; visible payload columns belong on the member `ObjectMapping`s.
- **`IllegalSubobjectInUnionTypeMapping` / `IllegalJsonInUnionTypeMapping`** — a `SqlObject` sub-object or a `SqlJson` field was placed directly in the union mapping. Map those on the member objects instead.

One more rule applies to every object mapping, including subtypes: each must declare at least one `key = true` field (directly or inherited), or it fails with `NoKeyInObjectTypeMapping`. That is why the union members above repeat the hidden `id` key.

## See also

- [Choose and wire a SQL backend](sql-backends.md) — the doobie/skunk plumbing the mappings here assume.
- [Compose and reuse mappings](compose-mappings.md) — embedding and context-sensitive mappings; the legacy `PrefixedMapping` used above for the shared `Synopses` type, and the `PathMatch` form preferred in new code.
- [Mapping types reference](../reference/mapping-types.md) — `PathMatch` vs the legacy `PrefixedTypeMatch`/`PrefixedMapping`, and the constructors that build each.
- [Map jsonb columns](jsonb-columns.md) — an alternative way to serve polymorphic data, navigated in-process rather than via SQL joins.
- [Validate your mappings](validate-mappings.md) — the full set of mapping validation errors and how to read them.
- [Mappings and cursors](../concepts/mappings-cursors.md) — what a `Cursor` is and how `discriminate` reads from it.
