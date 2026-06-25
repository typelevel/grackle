# Effects and batching internals

Most of Grackle's interpreter is a pure walk over the [`Query`](../reference/query-algebra.md) algebra: it threads a [`Cursor`](mappings-cursors.md) through the tree and reads data out of whatever your `Mapping` is backed by. Some fields, though, need to *do* something — run a database query, call an HTTP service, update a `Ref`. This page explains how Grackle keeps those side effects out of the pure tree-walk. It covers where an effect can attach (root vs. nested), how the compiler marks a field as effectful, how the interpreter *defers* each occurrence into a placeholder and resolves it in a later stage, and how it batches every occurrence of a nested effect field into a single call so a query over N rows does not turn into N+1 service calls.

It is for developers reasoning about the cost and ordering of effects. The recipes for *writing* them live in the [effects how-to](../how-to/effects-batching.md), and the exact signatures in the [effects reference](../reference/effects.md).

## Separating side-effecting fields from pure tree-walking

The interpreter's job is to produce JSON from a `Query` and a root `Cursor`. If a field needed to perform IO *inline* while that walk was happening, the walk would have to be effectful everywhere, ordering would be hard to reason about, and — worse — every row in a list would trigger its own call. Grackle avoids all of this by treating an effectful field as a *deferral*: when the interpreter reaches such a field it does not run the effect, it records a placeholder describing the effect to run and the continuation to evaluate afterwards. The pure walk finishes and produces a `ProtoJson` — a possibly-partial JSON tree with holes where the deferrals are. A separate stage then runs the deferred effects and fills the holes.

This is the same deferral machinery that handles cross-`Mapping` composition (component joins): both produce holes in the `ProtoJson` that are resolved later. The difference is who fills the hole — a delegated interpreter for a component, or an `EffectHandler` for an effect field. Keeping the effects in their own stage is what makes batching possible at all, because by the time the stage runs the interpreter can see *every* occurrence of the field at once.

## Root effects vs. nested effects: two attachment points

An effect can attach to a field at one of two very different points in execution.

A **root effect** attaches to a *top-level* field — a `Query`, `Mutation` or `Subscription` field — and runs *once, up front, before the rest of the query is interpreted*. It is a `FieldMapping` (`RootEffect`, or its streaming sibling `RootStream`) whose job is to perform some IO and hand back a `(Query, Cursor)` to interpret. This is how mutations do their write, and how a query can do per-request setup. In `runOneShot` the interpreter partitions the root selections into effectful and pure: each effectful root runs its `RootEffect` first, then the returned query is interpreted against the returned cursor. Root effects are *not* batched — each independent root field fires its own effect, so a request selecting three effectful root fields runs three effects.

A **nested effect** attaches to a field *deep inside the result tree* — for example `Country.currencies`, where each country in a list needs its currencies fetched from an external service. It is an `EffectField` naming an `EffectHandler[F]`. Because the field can occur many times (once per country, across many countries), the interpreter defers *every* occurrence and resolves them together. This is the case batching exists for.

```text
   query { ... }
        │
        ▼
  ┌─────────────┐
  │ root level  │   RootEffect / RootStream
  │             │   ── runs ONCE, before interpretation (runOneShot)
  │             │   ── one effect per root field, never batched
  └──────┬──────┘
         │ interpret the (query, cursor) it returns
         ▼
  ┌──────────────────────────────────────────────┐
  │ nested level (inside the result tree)         │
  │   country[0].currencies  ─┐                   │
  │   country[1].currencies  ─┤  EffectField      │
  │   country[2].currencies  ─┘  + EffectHandler  │
  │            └──────────────► deferred, then ONE batched runEffects call
  └──────────────────────────────────────────────┘
```

`RootEffect`, `RootStream` and `EffectField` share the supertype `EffectMapping`, whose `subtree = true` marks the field as owning its entire selection subtree. `RootStream` is the streaming form used by subscriptions; reaching one during a normal query or mutation is an internal error — `runOneShot` checks for it and raises `RootStream only permitted in subscriptions`. (Mutations served over the subscription transport go the other way, via `RootEffect.toRootStream`, which lifts a one-shot effect into a single-element stream. Subscriptions themselves are covered under [mutations and subscriptions](../tutorial/mutations-subscriptions.md).)

## `EffectElaborator` wraps a field's `Select` in an `Effect` node

For a nested effect field, the marking happens during compilation. `EffectElaborator` is a `Phase` that the `Mapping` installs after the usual select/component elaboration. It looks up each selected field in the type mappings, and where it finds an `EffectField` it rewrites the field's `Select` so the selection subtree is wrapped in an `Effect` algebra node:

```text
Select(fieldName, resultName, child)
        │  EffectElaborator: an EffectField exists for this field
        ▼
Select(fieldName, resultName, Effect(handler, Select(fieldName, _, child)))
```

`Effect` is an ordinary `Query` node — `case class Effect[F[_]](handler: EffectHandler[F], child: Query)` — so it travels through the algebra like any other. Nothing has run yet; the elaborator has only *marked* the field. When the interpreter later reaches `Select(_, _, Effect(handler, cont))`, it does not interpret `cont` inline. Instead it builds a deferred placeholder describing the handler, the continuation query and the parent cursor, and moves on. (The `Effect` node is one of the algebra cases listed in the [architecture overview](architecture.md); the elaboration machinery that inserts it is the subject of [compiler and elaboration](compiler-elaboration.md).)

## Staged interpretation: `ProtoJson`, `EffectJson`, scatter/gather

The placeholder is a `ProtoJson.EffectJson`, built by `ProtoJson.effect(mapping, handler, query, cursor)`. It captures four things: the owning `mapping`, the `handler` (as an `Option` — `None` for component deferrals, `Some(handler)` for effect fields), the continuation `query`, and the parent `cursor` the handler will read from. The interpreter wraps that into the `ProtoJson` tree exactly where the field's value belongs, leaving a typed hole.

Resolution happens in `completeAll`, which runs as the *next stage*. It works in two passes — a *gather* and a *scatter* — around the batched effect calls in the middle:

```text
   stage N produces a ProtoJson tree with EffectJson holes
        │
        ▼  GATHER: walk the whole tree, collect every deferred EffectJson
        │  GROUP:  by (mapping, handler)  →  one batched call per group
        │  RUN:    handler.runEffects(batch)   (the next stage)
        │  EVAL:   interpret each continuation child against its returned cursor
        ▼  SCATTER: substitute each resolved Json back into its own hole (by identity)
   fully-resolved Json (recursing while new holes appear)
```

The gather pass (`gatherDeferred`) walks the partial tree collecting every `EffectJson`. The scatter pass (`scatterResults`) walks it again and substitutes each resolved `Json` back into the exact placeholder it came from, keyed by object identity through an `IdentityHashMap`. In between, the deferred effects are run and their continuations interpreted — and because that interpretation can itself produce fresh `EffectJson` holes, `completeAll` recurses, one extra stage per layer of effect.

## Batching by `(mapping, handler)` in `completeAll` and why it kills N+1

The heart of the mechanism is the grouping in the middle of `completeAll`. Here is the whole method:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/main/scala/queryinterpreter.scala", "#complete_all"))
```

Read the middle of it. After `gatherDeferred` has flattened every deferral out of `pjs`, they are grouped with `.groupMap(ej => (ej.mapping, ej.handler))(identity)`. The grouping key is the pair of the owning `mapping` and the `handler`. Every `EffectJson` that shares that key lands in one `batch`, and each group becomes exactly *one* call:

- for a component deferral (`handler` is `None`) the batch goes to `mapping.combineAndRun(queries)`;
- for an effect field (`handler` is `Some(handler)`) the batch goes to `handler.runEffects(queries)`, where `queries` is the full `List[(Query, Cursor)]` — one pair per occurrence of the field across the entire result.

That single `runEffects` call is what collapses N+1. Without deferral, a result with 200 countries would call the currency service 200 times. With it, all 200 `Country.currencies` placeholders are gathered, grouped under the one handler, and passed as a 200-element batch to a single `runEffects` — which can collect the distinct country codes and issue one service call for the lot.

The grouping key is the crux, and it is *identity*-based: two occurrences batch together only if they carry the **same handler instance**. This is why effect handlers are written as a single shared `object` (for example `object CurrencyQueryHandler extends EffectHandler[F]`) referenced from the field mapping. Here is how the `EffectField` is declared in the mapping, alongside the ordinary `SqlField`/`SqlObject` mappings:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlNestedEffectsMapping.scala", "#effect_typemappings"))
```

`EffectField("currencies", CurrencyQueryHandler, List("code2"))` attaches the shared `CurrencyQueryHandler` to `Country.currencies`. Construct a fresh handler per field, or per row, and the keys would differ, the batch would split, and the N+1 would come straight back. The `List("code2")` is the field's *required* columns: it forces the parent SQL `SELECT` to include `code2` so the handler can read each country's code off the parent cursor when it builds its batched call.

On the way back, `runEffects` must return exactly one continuation `Cursor` per input pair, in the same order — `completeAll` zips the returned cursors against the continuations positionally (`(conts, cs).parMapN`), so a length or order mismatch silently corrupts the result. The interpreter calls `Query.extractChild` on each input query to recover the continuation child to interpret, and errors with `Continuation query has the wrong shape` if it cannot. The how-to covers the order-preservation patterns (`runGrouped`, captured indices, `sortBy`) in detail.

## Cost model: one extra stage per distinct effect field

The deferral is what makes the cost model simple. A nested effect field is resolved in its own `completeAll` stage, so **each distinct effect field costs one extra interpreter stage** — but *all* occurrences of that field share that one stage. Ten thousand rows of `Country.currencies` is still one extra stage and one `runEffects` call, not ten thousand.

Stages compose by nesting. A *doubly*-nested effect — say `Currency.country` selected underneath `Country.currencies` — runs in a *second* stage on top of the first: `completeAll` interprets the first handler's continuations, those produce fresh `EffectJson` holes for the inner field, and `completeAll` recurses to resolve them. Each layer is still batched within itself; you pay one stage per layer of effect nesting, not one per row at any layer. Root effects sit outside this entirely: they run before interpretation begins and are never batched, so their cost is simply one effect per effectful root field.

## See also

- [Run side effects with `RootEffect` and batch nested fields](../how-to/effects-batching.md) — the task-focused recipes, with the full handler implementations.
- [Effects and batching reference](../reference/effects.md) — exact signatures for `EffectField`, `RootEffect`, `RootStream`, `EffectHandler` and the circe constructors.
- [The query interpreter](query-interpreter.md) — how `ProtoJson`, `Cursor` and `completeAll` fit into the wider interpretation pass.
- [The compiler and elaboration](compiler-elaboration.md) — where `EffectElaborator` runs among the other phases.
- [Architecture overview](architecture.md) — the three-layer pipeline these stages live in.
