# Run effects and batch nested fields

This how-to is for developers who need a mapping to run a side effect — call an external service, hit a second database, update a `Ref` — as part of resolving a query, and who want nested fields to do that **once per query rather than once per row** (no N+1). Grackle gives you two attachment points: a `RootEffect` that fires once at a top-level field before the rest of the query runs, and an `EffectField` + `EffectHandler` pair that defers a nested field and batches every occurrence of it into a single call. This page is the recipe for both; the mechanism behind the batching — staged interpretation, `ProtoJson` placeholders, grouping by `(mapping, handler)` — lives in the [effects and batching concept](../concepts/effects-batching.md) and the [effects reference](../reference/effects.md).

## Run an effect at a root field

A `RootEffect` is a `FieldMapping` for a top-level `Query`/`Mutation`/`Subscription` field. It runs once, up front, before the interpreter walks the rest of the query, and yields a (possibly rewritten) query plus a root `Cursor`. Its primary constructor is private, so you always go through one of the companion factories, choosing by how much you need to influence the result:

| Factory | You provide | Use it when |
| --- | --- | --- |
| `computeUnit(name)(Env => F[Result[Unit]])` | a pure side effect | you only write (e.g. a mutation) and the query/cursor stay as-is |
| `computeCursor(name)((Path, Env) => F[Result[Cursor]])` | a custom root `Cursor` | you compute the data and build the cursor yourself |
| `computeChild(name)((Query, Path, Env) => F[Result[Query]])` | a replacement child query | you want the effect to decide *what* is queried (e.g. inject a `Filter`/`Limit`) |
| `apply(name)((Query, Path, Env) => F[Result[(Query, Cursor)]])` | both query and cursor | you need full control |

`computeUnit` and `computeCursor` leave the client's query untouched, so the only channel for passing effect results downstream is the `Env` attached to the returned `Cursor`. If you need to change what is queried, reach for `computeChild` or the full `apply`.

The circe backend adds two shortcuts on top of `computeCursor` so you can return data directly instead of building a cursor: `computeJson` (return raw `Json`) and `computeEncodable` (return any value with a circe `Encoder`). The following mapping puts all of these side by side on a `CirceMapping`, with a `SignallingRef[F, Int]` counter that each effect bumps so a test can prove it fired:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CirceEffectData.scala", "#circe_effects"))
```

Each field wires one `RootEffect` into the `Query` `ObjectMapping`:

- `foo` uses `computeCursor` and builds a `circeCursor(path, env, json)` focused on the field — the `Json` carries only the struct's fields (`n`, `s`).
- `bar` uses `computeJson` and returns the same shape of `Json`; the mapping builds the cursor for you.
- `baz` uses `computeEncodable` and returns a `Struct`; the implicit `Encoder[Struct]` turns it into `Json` and the mapping builds the cursor.
- `qux` shows the focus detail that trips people up: it builds the cursor with `Path.from(p.rootTpe)` (root-focused) and nests the field name *inside* the `Json` (`"qux" -> {...}`). A field-focused cursor like `foo` must not include the field name; a root-focused cursor must. Choosing the wrong focus yields a shape mismatch against the schema.

One thing to note before you rely on this for batching: **independent root fields each run their own effect.** A query asking for `foo`, `bar` and `baz` together runs three separate `RootEffect`s, so the counter ends at `3`, not `1`. Root effects are *not* batched across sibling fields — only nested `EffectField` occurrences are. That batching is the next section.

To drive a root effect and observe it from a test, compile and run the query with `Mapping.compileAndRun` and read the counter back out. The pattern (schematic):

```text
for {
  ref <- SignallingRef[IO, Int](0)
  map  = new TestCirceEffectMapping(ref)
  res <- map.compileAndRun("""query { foo { s n } }""")
  n   <- ref.get        // == 1 for a single root effect
} yield (res, n)
```

`compileAndRun(text)` compiles the query through the [`QueryCompiler`](../concepts/compiler-elaboration.md) and runs it, returning `F[Json]`. The `SignallingRef` is the observable side effect that proves the `RootEffect` fired the expected number of times.

## Batch a nested field with an `EffectField`

When the field that needs an effect is *nested* — say `Country.currencies`, backed by an external currency service — running the effect inline would call the service once per country, the classic N+1. Grackle solves this by deferring the field: the `EffectElaborator` compiler phase wraps it in an `Effect` algebra node, the interpreter turns every occurrence into a deferred placeholder, and at the end of the stage it gathers all of them and hands them to your handler in **one** call. You implement that one call.

### Step 1 — declare the `EffectField` and its required columns

In the parent object's `ObjectMapping`, replace what would be an `SqlObject` with an `EffectField(fieldName, handler, required)`. The `required` list names sibling columns that must be fetched in the parent query so your handler can read them from each parent `Cursor`. Here is the world mapping's `typeMappings`, where `Country.currencies` is an `EffectField` declaring `List("code2")`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlNestedEffectsMapping.scala", "#effect_typemappings"))
```

Two lines carry the recipe. `SqlField("code2", country.code2)` is an ordinary mapped column on `Country`, and `EffectField("currencies", CurrencyQueryHandler, List("code2"))` attaches the handler to the `currencies` field while declaring `code2` as required. That `required` list is what makes `SqlMapping` add `code2` to the parent `SELECT`; without it, `parentCursor.fieldAs[String]("code2")` inside the handler would fail. The `Currency` mapping at the bottom shows that effect fields nest: `Currency.country` is itself an `EffectField` backed by a second handler.

This corresponds to the GraphQL schema:

```graphql
type Country {
  name: String!
  code2: String!
  currencies: [Currency!]!
}

type Currency {
  code: String!
  exchangeRate: Float!
  countryCode: String!
  country: Country!
}
```

### Step 2 — implement `runEffects`: read parents, make ONE call

`EffectHandler[F]` has a single method, `runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]]`. The interpreter passes you one `(continuation-query, parent-cursor)` pair for **every** occurrence of the field across the whole result, and you must return **exactly one continuation `Cursor` per input pair, in the same order**. Read the keys off the parent cursors, collapse them to a distinct set, make a single service call, then build one cursor per input:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlNestedEffectsMapping.scala", "#currency_handler"))
```

Walking the handler:

- It reads `code2` off each parent cursor with `fieldAs[String]("code2")` — the column it required in Step 1 — keeping both the per-input list (`countryCodes`, order-preserving, with `None` for missing) and the de-duplicated `distinctCodes`.
- For each pair it derives the child GraphQL `Context` with `Query.childContext(parentCursor.context, query)`. The `query` it receives is the *continuation child* — the selection set under `currencies` — not the original field select, so you must derive the context rather than reuse the parent's.
- `currencyService.get(distinctCodes)` is the **single** call for the entire batch, regardless of how many countries are in the result. `ResultT` threads the `F[Result[...]]` for you.
- `unpackResults` splits the one service result back out per country (in `countryCodes` order), and the final `zip` builds one `CirceCursor(childContext, json, Some(parentCursor), parentCursor.env)` per input — preserving order.

Because the interpreter groups deferred effects by `(mapping, handler)` *identity*, the batching only happens if the **same handler object instance** is used for every occurrence. `CurrencyQueryHandler` is a single `object`, so every `Country.currencies` in the result shares it and collapses into one `runEffects` call. Constructing a fresh handler per field or per row would split the batch and reintroduce N+1.

### Step 3 — verify batching with a call counter

The `CurrencyService` in this example holds a `Ref[F, Int]` counter that increments on each `get`. Running a query that returns several countries and asserting the counter goes from `0` to `1` is the proof that the effect ran once for the whole query, not once per row. That assertion is exactly what `SqlNestedEffectsSuite` checks. If you build your own service, expose a counter (or a recorded-calls list) and assert on it the same way — it is the cheapest regression guard against an accidental N+1.

## Re-query the same mapping (doubly-nested effects)

When the nested field's result is itself produced by the *same* SQL mapping — here `Currency.country`, which needs a `Country` — the handler re-queries Grackle. The challenge is that the interpreter still hands you a flat batch of `(query, cursor)` pairs, so you group identical continuation queries, run each group once, and restore the original input order afterwards. The `runGrouped` helper packages that pattern:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlNestedEffectsMapping.scala", "#country_handler"))
```

The shape to copy:

- `runGrouped` zips every input with its position, groups by the continuation `Query` (`.groupMap(_._1._1)(...)`), runs `op` once per group, then `flatten`s and `sortBy(_._2)` to put the cursors back into the order the interpreter expects. This is the order-restoration the contract demands.
- Inside `op`, the handler reads the parent `countryCode`s, maps them to their ISO codes, and builds **one** `combinedQuery` — `Select("country", None, Filter(In(CountryType / "code", codes), child))` — that fetches every needed country in a single SQL round-trip via `sqlCursor(combinedQuery, Env.empty)`.
- `mkListCursor` focuses the resulting cursor on the `country` list and `preunique`s it; `partitionCursor` splits that list back out per input code so each input pair gets its own cursor; `.zip(indices)` reattaches the captured positions.
- The `case _` arm returns `Result.internalError("Continuation query has the wrong shape")` — a reminder that this lands in the effect `F`, not the GraphQL `errors` array (see [error handling](errors.md)).

Each distinct nested effect field costs one extra interpreter stage, so a doubly-nested effect (`Currency.country` inside `Country.currencies`) resolves over successive stages — correct, and still one call per field-level per stage, but worth knowing when you reason about round-trips.

## Wire it to a real backend

`SqlNestedEffectsMapping` is backend-agnostic. To run it you combine it with a concrete SQL backend (see [Choose and configure a SQL backend](sql-backends.md)) and inject the external service. The test suite constructs the `CurrencyService` *first* — so it can inspect the call counter — then builds the mapping over a Doobie/Postgres transactor and binds the abstract `def currencyService` to the constructed instance. Schematic (the real, DB-backed version lives in the test suite):

```text
for {
  service <- CurrencyService[IO]                 // construct first; holds the counter
} yield {
  val mapping =
    new DoobiePgTestMapping(transactor) with SqlNestedEffectsMapping[IO] {
      lazy val currencyService = service          // inject the external service
    }
  (service, mapping)
}
```

The same recipe works for Skunk, or for a service that is an HTTP client rather than a second database — the only requirement is that `currencyService.get` returns an `F[...]` you can run inside `runEffects`. Substituting a stub service with a counter is also how you test batching without a live database.

## A note on subscriptions

A `RootStream` is the streaming analogue of `RootEffect`: its effect returns `Stream[F, Result[(Query, Cursor)]]` and emits one result per stream element. It is **only** valid in a `Subscription` — a normal query or mutation that reaches one raises an internal error, `RootStream only permitted in subscriptions`. Mutations served through the subscription transport use `RootEffect.toRootStream` to lift a single-shot effect into a one-element stream. The end-to-end subscription walkthrough is in the [mutations and subscriptions tutorial](../tutorial/mutations-subscriptions.md).

## See also

- [Effects and batching](../concepts/effects-batching.md) — why effects are deferred, staged interpretation, and how `(mapping, handler)` grouping eliminates N+1.
- [Effects reference](../reference/effects.md) — exact signatures for `RootEffect`, `RootStream`, `EffectField` and `EffectHandler`.
- [Mutations and subscriptions tutorial](../tutorial/mutations-subscriptions.md) — root effects and `RootStream` in a working server.
- [Filtering, ordering and paging](filtering-ordering-paging.md) — assembling the `Filter`/`Limit`/`In` predicates a `computeChild` or re-query handler injects.
- [Error handling](errors.md) — why `internalError` is raised into `F` and never appears in the response `errors` array.
- [The cursor and query interpreter](../concepts/query-interpreter.md) — what a `Cursor` is and how continuation queries are evaluated.
