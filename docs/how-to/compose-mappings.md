# Compose multiple mappings (federation)

This recipe shows how to serve one GraphQL schema from several independent `Mapping`s — for example a SQL-backed `world` schema joined to an in-memory `currencies` mapping. You extend [`ComposedMapping`](../reference/mapping-types.md), wire delegated fields with `Delegate`, and express the cross-mapping link with a `join` function. It assumes you can already build a single mapping (see [DB-backed Model](../tutorial/db-backed-model.md)); for the staged interpretation behind delegation, see [How composition executes](../concepts/composition.md).

## When to reach for composition

Compose mappings when one field's subtree lives in a different backend or service than its parent: a SQL `Country` whose `currency` comes from an in-memory cache or a remote API, or two value mappings owned by different teams that you want to expose behind a single endpoint. The parent mapping serves its own fields and *delegates* the foreign subtree to another mapping, which runs in its own interpreter stage.

A few things composition is **not**: there is no automatic schema merging, and `selectElaborator` does not compose across mappings — you re-declare what the composed level owns. Delegation is wired explicitly, field by field.

## Step 1: build the component mappings independently

Start with each component as an ordinary, standalone mapping. Here is a tiny in-memory currency mapping with a single `fx(code:)` root field.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/composed/ComposedData.scala", "#composed_currency"))
```

Its `selectElaborator` turns the `code` argument into `Unique(Filter(Eql(CurrencyType / "code", Const(code)), child))`, so `fx(code: "GBP")` resolves to a single `Currency`. The country component is the same shape, modelling `Country(code, name, currencyCode)` over an in-memory list.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/composed/ComposedData.scala", "#composed_country"))
```

Note what each component schema does **not** have: `CountryMapping`'s `Country` type exposes `code` and `name` but no `currency` field, and the two mappings know nothing about each other. The link between them is added only at the composed level.

## Step 2: define the composed schema as a superset

The composed mapping needs its **own** schema. This is usually a superset of the components that adds the join field — here `Country.currency: Currency!`, which appears in neither component schema. Forgetting it leaves nowhere to hang the `Delegate`.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/composed/ComposedData.scala", "#composed_mapping"))
```

Walk the three load-bearing pieces of `ComposedMapping`:

- **Its own schema and refs.** The `Query` type re-exposes `country`, `fx` and `countries`, and `Country` gains the new `currency: Currency!` field. `QueryType`, `CountryType` and `CurrencyType` are this mapping's refs, distinct from the components'.
- **Its own `selectElaborator`.** Elaboration does not compose: the composed mapping re-declares the `fx` and `country` root-argument cases, filtering on its **own** `CurrencyType` / `CountryType` refs. Drop these and the root arguments are never bound.
- **`Delegate` field mappings.** Inside ordinary `ObjectMapping`s, `Delegate("country", CountryMapping)`, `Delegate("countries", CountryMapping)` and `Delegate("fx", CurrencyMapping)` route those root fields straight through to the component that owns them. `Delegate("currency", CurrencyMapping, countryCurrencyJoin)` routes `Country.currency` through an explicit join.

A `Delegate` is a `FieldMapping` of the form `Delegate(fieldName, mapping, join)`, where `join` defaults to `ComponentElaborator.TrivialJoin` — the child query passes through unchanged. The first three delegates above use that default; only `Country.currency` supplies a custom join.

## Step 3: write the join

The `join` is a `(Query, Cursor) => Result[Query]`. It receives the **already-elaborated** child query and the parent's `Cursor` (focused on the raw parent model value), and returns the continuation query that the *other* mapping will run. This is where the cross-mapping JOIN is expressed.

In `countryCurrencyJoin` (the last method in the snip above), the focus is a `CountryData.Country` and the query is `Select("currency", _, child)`. The join rewrites it to `Select("fx", Unique(Filter(Eql(CurrencyType / "code", Const(c.currencyCode)), child)))` — that is, it reads `currencyCode` off the parent country and turns "give me this country's currency" into "run `fx` filtered to that code" against `CurrencyMapping`. Two details matter:

- The join emits the **other** mapping's field name (`currency` becomes `fx`); the interpreter realigns the result name afterwards, so the client still sees `currency` (or whatever alias it asked for).
- Always handle the mismatch case. The `case _` returns `Result.internalError(...)`. Per the [error model](errors.md), an internal error is raised into the effect `F` rather than surfacing in the response `errors` array, which is the right behaviour for a "this should never happen" cursor-shape violation.

With that wired, a nested query resolves end to end across both mappings:

```graphql
query {
  country(code: "GBR") {
    name
    currency { code exchangeRate }
  }
}
```

```json
{
  "data": {
    "country": {
      "name": "United Kingdom",
      "currency": { "code": "GBP", "exchangeRate": 1.25 }
    }
  }
}
```

## Step 4: list-valued joins return a `Group`

When the delegated field is a list, the join must return a `Group` of `Select`s — one delegated query per element — not a single `Select` that yields a list. This `collectionItemJoin` turns a `Collection` with an `itemIds: List[String]` into one `itemById` lookup per id.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/composed/ComposedListSuite.scala", "#composed_list_join"))
```

The parent focus is a `Collection`; the join maps each `id` in `itemIds` to `Select("itemById", Unique(Filter(Eql(ItemType / "id", Const(id)), child)))` and wraps the lot in `Group(...)`. The interpreter special-cases a `Group` inside a component and assembles the results into a JSON array. Returning a single `Select` where a list is expected (or vice versa) is a shape mismatch.

## Step 5: TrivialJoin when the key is already in context

If the target mapping can resolve the field from the key already present in the parent's context — a true root field, or a field whose key the target picks up from its inherited `Env` — you don't need a custom join at all. Leave the `join` argument off `Delegate` and it defaults to `ComponentElaborator.TrivialJoin`, passing the child query through unchanged.

The SQL-backed federation does exactly this for `Country.currencies`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlComposedWorldMapping.scala", "#composed_sql"))
```

`SqlComposedMapping` takes the two component mappings as constructor arguments (`world` and `currency`), so it composes a SQL `world` schema with an effectful in-memory `currency` mapping. The `Query` root fields `country` / `countries` / `cities` delegate to `world`; `Country.currencies` delegates to `currency` with the default `TrivialJoin`, because the join key (`Country.code`) is already in the parent country's context and the currency mapping resolves it from its own `Env`. As before, the composed level re-declares `selectElaborator` for the root arguments (`country`'s `code`, `cities`' `namePattern`) on its own refs.

To instantiate it, pass concrete component mappings to the constructor, for example with Skunk:

```scala
new SqlComposedMapping(
  new SkunkTestMapping(pool) with SqlWorldMapping[IO],
  currencyMapping
)
```

Building the SQL side (transactor or session pool, table defs, joins) is covered in [Choose and configure a SQL backend](sql-backends.md).

## Step 6: batch delegated queries with `combineAndRun`

By default each delegated subquery runs independently: a list of countries, each needing its currency, issues N separate currency lookups. To collapse them into one backend call, override `combineAndRun` on the delegated mapping.

```scala
def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]]
```

The default implementation runs each `(query, cursor)` pair independently. The effectful currency mapping overrides it to group sibling `currencies` lookups into a single call:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlComposedWorldMapping.scala", "#composed_combine"))
```

The override unpacks each delegated `Select("currencies", _, _)` together with its country `code`, partitions the groupable queries, and folds the codes into one `SimpleCurrencyQuery(codes, child)` that fetches every requested currency at once. It then repacks the batched results back into per-query `ProtoJson`s. The hard invariant: **the returned list must stay positionally aligned with the input list** — the `indexedQueries` / `repackedResults` machinery exists precisely to preserve that order. A two-city query that touches several countries issues exactly one `currencies(...)` lookup with this override in place, versus one per country without it.

Batching is opt-in per mapping; this is the only built-in way to coalesce cross-mapping calls — there is no automatic dataloader. See [Effects and batching](effects-batching.md) for the related root-effect batching pattern.

## How it executes

You never construct the boundary by hand. At compile time the `componentElaborator` phase rewrites each delegated `Select` into a `Component(targetMapping, join, child)` node. At interpretation time the parent runs its part, applies `join` to its `Cursor` and the child query, and emits a deferred `ProtoJson.component` tagged with the target mapping. `QueryInterpreter.completeAll` then gathers those deferred subtrees, groups them by mapping, calls each mapping's `combineAndRun` in a fresh stage, and recurses until nothing is deferred — which is why deep composition multiplies stages and why batching pays off. The full mechanism is in [How composition executes](../concepts/composition.md).

## See also

- [How composition executes](../concepts/composition.md) — the staged interpretation, `Component` nodes, and `completeAll` stitching behind delegation.
- [Mapping types](../reference/mapping-types.md) — `ComposedMapping`, `Delegate`, and the field-mapping reference.
- [Choose and configure a SQL backend](sql-backends.md) — building the SQL component of a federated mapping.
- [Effects and batching](effects-batching.md) — the effectful root and batching patterns that pair with `combineAndRun`.
- [Filter, sort and page a field](filtering-ordering-paging.md) — the `Filter`/`Unique`/`Eql` building blocks used inside joins.
