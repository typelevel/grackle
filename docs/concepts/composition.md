# How cross-mapping delegation executes

Grackle can stitch several independent [`Mapping`](mappings-cursors.md)s together behind a single GraphQL schema, so that one field of a type is served by a SQL backend while another is served by an in-memory or effectful one. This page explains the *mechanism* behind that composition: why it has to run in several stages, how a `Delegate` field mapping becomes a `Component` boundary in the query algebra, how the interpreter defers each delegated subtree, and how those deferred subtrees are grouped, batched, and stitched back into the final JSON. It is aimed at developers who already use `ComposedMapping` and want to understand what happens when they run a nested cross-mapping query. For the step-by-step recipe, see [Compose mappings](../how-to/compose-mappings.md); this page is the "why".

## Why composition is multi-stage

A single mapping resolves a query in one pass: the [query interpreter](query-interpreter.md) walks the elaborated [`Query`](../reference/query-algebra.md) against that mapping's `Cursor`s, producing `ProtoJson` and then `Json`. Composition cannot work that way, because **the parent mapping has no local data for a delegated field**.

Consider a `Country` type whose `currency` field is served by a *different* mapping. When the parent resolves `country(code: "GBR") { name currency { code exchangeRate } }`, it can produce `name` from its own `Country` value, but it has nothing in scope to produce `currency` — that subtree belongs to the currency mapping, which has its own schema, its own `Cursor`s, and possibly its own effect. So the parent does what it *can* do (resolve `name`), and **defers** the `currency` subtree to be run later, by the other mapping's interpreter, in a separate stage. Once that stage produces the currency JSON, it is substituted back into the hole the parent left. Composition is therefore inherently staged: a parent stage that emits placeholders, followed by one stage per target mapping that fills them in, recursing until nothing is left deferred.

This is reflected in `ComposedMapping[F]`, the base class a parent mapping extends. Its whole job is to supply a placeholder cursor for delegated fields:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/main/scala/composedmapping.scala", "#composed_base"))
```

The override of `mkCursorForMappedField` returns a `ComposedCursor` whose `focus` is `null` and whose `parent` is `None` — there genuinely is no parent data for a delegated subtree, so any logic that reads `parent.focus` inside a delegated field would see `null`. The cursor exists only to carry the `Context` and `Env` forward into the delegated stage. Everything else a `ComposedMapping` needs — its own `schema`, `typeMappings`, and `selectElaborator` — is ordinary `Mapping` machinery; composition adds only the delegation behaviour on top.

## From `Delegate` to a `Component` boundary

You mark a field for delegation with a `Delegate` field mapping inside the parent's `ObjectMapping`:

```scala
case class Delegate(
    fieldName: String,
    mapping: Mapping[F],
    join: (Query, Cursor) => Result[Query] = ComponentElaborator.TrivialJoin
)
```

`Delegate(fieldName, mapping, join)` declares that `fieldName` on this object type is served by `mapping`. It is always `hidden = false` and `subtree = true`. The optional `join` is a `(Query, Cursor) => Result[Query]` and defaults to `ComponentElaborator.TrivialJoin`, which passes the child query through unchanged (`(q, _) => q.success`).

`Delegate` is *not* itself a query-algebra node. It is rewritten into one during query compilation by the `componentElaborator` phase. Every mapping lazily derives this phase from its own `Delegate` field mappings: it walks the elaborated query and, for any `Select` whose `(objectType, fieldName)` matches a registered delegation, wraps that `Select` in a `Component` node from the [query algebra](../reference/query-algebra.md):

```scala
case class Component[F[_]](
    mapping: Mapping[F],
    join: (Query, Cursor) => Result[Query],
    child: Query) extends Query
```

`Component` simply marks a component boundary: the subtree `child` is to be evaluated by `mapping`, after `join` has been applied. You never construct `Component` by hand — it exists only as the output of `componentElaborator`.

Phase ordering matters here. A mapping's `compilerPhases` are `List(selectElaborator, componentElaborator, effectElaborator)`. Because `componentElaborator` runs **after** `selectElaborator`, the child query inside a `Component` is *already elaborated* — root arguments have been turned into `Filter`/`Unique`/etc. by the parent's own `selectElaborator`. This is also why the composed mapping must re-declare `selectElaborator` cases for the root fields it owns: phases do not compose automatically across mappings, so the composed parent elaborates root arguments against its *own* type refs before delegation ever happens. See [compiler & elaboration](compiler-elaboration.md) for the phase pipeline in general.

## Interpretation: the join builds a continuation, deferred as a component

When the interpreter reaches a `Component(mapping, join, child)` node, it does not recurse into `child` itself. Instead it applies `join(child, cursor)` — handing the join both the (elaborated) child query and the parent's `Cursor`, focused on the parent's raw model value. The join's result is a **continuation query**: the query the *other* mapping will actually run.

This is where a cross-mapping join is expressed. A `Country.currency` join, for example, pattern-matches the parent's focus and rewrites the child:

```text
join(Select("currency", _, child), cursor where focus = Country("GBR", "United Kingdom", "GBP"))
  => Select("fx", Unique(Filter(Eql(CurrencyType / "code", Const("GBP")), child)))
```

The join reads the parent's `currencyCode` out of the focused `Country` and produces a query the currency mapping understands — selecting `fx` (the currency mapping's own field) filtered by that code. The join is also where you express the error contract: if the cursor focus is not the type you expect, return `Result.internalError(...)`. An [internal error](../reference/result-problem.md) is raised into the effect `F`; it does not appear in the response `errors` array.

Having computed the continuation, the interpreter wraps it as a **deferred subtree** rather than running it. `runValue` emits `ProtoJson.component(mapping, continuation, cursor)` — an opaque placeholder in the proto-JSON tree, tagged with the target mapping. The current stage finishes with that placeholder sitting in the spot where the delegated field's value will go. Nothing in the parent's stage knows the currency mapping's data; it only knows *which* mapping owes a result for *which* query.

One subtlety: the continuation selects the other mapping's field name (`fx`, not `currency`), but the client asked for `currency`. The interpreter realigns the result name via `alignResultName(child, cont)` so that the stitched-in JSON appears under the original field name or alias the client used. You do not have to manage this in the join.

## `completeAll`: group by mapping, run, recurse

Deferred subtrees are resolved by `QueryInterpreter.completeAll`, which runs at the end of a stage. It does three things:

1. **Gather** every deferred subtree (`DeferredJson`) anywhere in the proto-JSON tree.
2. **Group** them by `(mapping, handler)` — all the placeholders owed by the same target mapping are collected together.
3. **Run** each group by calling that mapping's `combineAndRun(queries)`, where `queries` is the list of `(continuation, cursor)` pairs for the group. The resulting `Json` for each placeholder is substituted back into its hole.

Because each delegated stage may *itself* contain delegated fields, this process recurses: `completeAll` re-runs per target mapping until no deferred subtrees remain. Composition is fully recursive — you can compose a mapping that is itself composed — but each level of nesting is a distinct interpreter stage, so deep delegation multiplies the number of stages.

The staging looks like this for `country(code: "GBR") { name currency { code exchangeRate } }`:

```text
Stage 0  parent (ComposedMapping)
         resolve country -> { name: "United Kingdom", currency: <deferred: CurrencyMapping> }
                                                                 │
                                                                 ▼  completeAll groups by mapping
Stage 1  CurrencyMapping.combineAndRun([ fx(code:"GBP") { code exchangeRate } ])
         => { code: "GBP", exchangeRate: 1.25 }
                                                                 │
                                                                 ▼  substitute back
Result   { country: { name: "United Kingdom",
                      currency: { code: "GBP", exchangeRate: 1.25 } } }
```

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

## Why batching matters: `combineAndRun`

`completeAll` passes the *whole group* of a mapping's deferred queries to `combineAndRun` in one call, which is the hook that makes cross-mapping batching possible. The default implementation runs each query independently:

```scala
def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]] =
  queries
    .map { case (q, c) => (q, schema.queryType, c) }
    .traverse((interpreter.runOneShot _).tupled)
    .map(ProtoJson.combineResults)
```

That is correct but unbatched: a list-valued delegated field over *N* parent rows becomes *N* independent backend calls. Because all *N* continuations arrive in the *same* `combineAndRun` call, a mapping can override the method to collapse them into a single backend lookup — provided the returned list stays **positionally aligned** with the input list. `completeAll` zips a group's results back onto its placeholders in order, so the *i*-th `ProtoJson` you return must answer the *i*-th `(query, cursor)` you were handed.

The effectful currency mapping in Grackle's SQL-composed world example does exactly this. It groups the per-country `currencies` continuations and issues one combined lookup instead of one per country:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlComposedWorldMapping.scala", "#composed_combine"))
```

The shape to notice: continuations that match the `SimpleCurrencyQuery` pattern are partitioned out, grouped by their child query and context, merged into a single `currencies(countryCodes)` query carrying all the codes, run once via `super.combineAndRun`, and then `unpackResults` redistributes the rows of that one response back to the individual placeholders by their original indices (`indexedQueries`). Anything that does not group falls through to the default per-query path. The corresponding suite asserts the currency mapping is called exactly once for a multi-country query — proof that the *N*-into-1 collapse happened. Batching is strictly opt-in: without an override you get *N* calls, which is correct but slower. This is the same staged-effect machinery described in [effects & batching](effects-batching.md), reused for cross-mapping delegation.

## Scalars vs lists, and result-name realignment

A join can produce either a single value or a list, and the *shape* of what it returns selects between them:

- **Scalar / single object** — return a single `Select` (as the `Country.currency` join does). The interpreter emits one `ProtoJson.component` and realigns its result name to the client's field/alias.
- **List** — return a `Group` of `Select`s, one per element. The interpreter special-cases a `Group` inside a `Component`: it builds a JSON array, one deferred component per element, under the delegated field's name.

The list case looks like this — a `Collection.items` join that fans the parent's `itemIds` out into one delegated query per id:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/composed/ComposedListSuite.scala", "#composed_list_join"))
```

Each `id` produces a `Select("itemById", Unique(Filter(...)))`, and the surrounding `Group` tells the interpreter to assemble an array from the per-element results. Returning a single `Select` where a list is expected — or a `Group` where a scalar is expected — is a shape mismatch the interpreter cannot stitch. In both cases the join targets the *other* mapping's field names (`fx`, `itemById`), and the result-name realignment ensures the client still sees the field or alias it actually requested (`currency`, `items`).

A final note on `TrivialJoin`: it is sufficient only when the delegated field needs no parent-specific key transform — a genuine root field, or one the target mapping can resolve from inherited `Env`/[context](../reference/query-algebra.md) without rewriting the query. As soon as the continuation depends on a value from the parent's focus (a foreign key, a list of ids), you must supply an explicit join.

## See also

- [Compose mappings](../how-to/compose-mappings.md) — the task-oriented recipe, with the full runnable code.
- [Effects & batching](effects-batching.md) — the staged-effect machinery `combineAndRun` builds on.
- [The query interpreter](query-interpreter.md) — how a single mapping resolves a query into JSON.
- [Compiler & elaboration](compiler-elaboration.md) — the phase pipeline that turns query text into the `Query` algebra.
- [Mappings & cursors](mappings-cursors.md) — the `Mapping`/`Cursor` model that composition coordinates.
- [Architecture overview](architecture.md) — where composition sits in Grackle's compiler/interpreter design.
