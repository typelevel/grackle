# What is Grackle?

Grackle is a GraphQL server library for Scala, built on the Typelevel stack (cats-effect and fs2). You give it a GraphQL schema and a *mapping* that explains how each type and field is served from your data — an in-memory value, a derived Scala ADT, a circe JSON document, or a SQL database — and it answers GraphQL queries against it. This page orients you to the core model and vocabulary so you can pick the right backend and the right next page; it assumes you are a Scala developer comfortable with cats-effect basics but does not assume deep GraphQL knowledge.

## Grackle in one paragraph

Grackle is a compiler and an interpreter. A query arrives as GraphQL text; the **`QueryCompiler`** parses it, validates variables, fragments and field merging, then runs a sequence of *elaboration* phases that rewrite the parsed tree into an executable **`Query`** algebra — a small sealed ADT of nodes such as `Select`, `Filter`, `Unique`, `Limit`, `OrderBy` and `Component`. The **`QueryInterpreter`** then walks that algebra against your data using a **`Cursor`**, a read-only navigator over your backing model, producing a `ProtoJson` (a possibly-partial JSON tree) that is finally resolved into an `io.circe.Json` response. Everything that ties those two halves together — the schema, the catalog of mappings, and the interpreter — lives in a single object: the `Mapping`.

```text
  GraphQL query text
         │
         ▼
  ┌────────────────────────────────────────────────────┐
  │  QueryCompiler                                      │
  │  parse → validate → elaborate (SelectElaborator,    │
  │  fragments, skip/include, components, …)            │
  └────────────────────────────────────────────────────┘
         │
         ▼
   Query algebra      (Select, Filter, Unique, Limit, OrderBy, Component, …)
         │
         ▼
  ┌────────────────────────────────────────────────────┐
  │  QueryInterpreter  +  Mapping / Cursor              │
  │  walk the algebra, navigate your data               │
  └────────────────────────────────────────────────────┘
         │
         ▼
   ProtoJson  ──(resolve deferred / batched subtrees)──▶  JSON response
```

The compiler half is covered in depth under [the compiler and elaboration](../concepts/compiler-elaboration.md); the interpreter half under [how the query interpreter works](../concepts/query-interpreter.md). For the whole picture in one place, see the [architecture overview](../concepts/architecture.md).

## The central idea: a `Mapping` ties a schema to data

The pivotal abstraction is `Mapping[F[_]]`. A mapping holds three things:

- a `schema` — the GraphQL type system your server exposes;
- a `typeMappings` catalog — for each GraphQL type, how its fields are served from your data;
- a `selectElaborator` — how field arguments (filters, ids, paging) are translated into the query algebra.

From those, the mapping derives the `QueryCompiler` and `QueryInterpreter` for you, and exposes the top-level entry points `compileAndRun` (for queries and mutations) and `compileAndRunSubscription` (for subscriptions). Validation of the catalog against the schema happens automatically the first time the compiler is built.

Here is a complete, minimal mapping — an in-memory list of books served over GraphQL. It is the [quick-start](quick-start.md) example, shown here only to make the shape concrete:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/docs/src/main/scala/grackle/QuickStartMapping.scala", "#quickstart"))
```

Read it top to bottom and you see the three parts: the `schema` literal (validated at compile time by the `schema"""..."""` interpolator from `grackle.syntax._`, which yields a bare `Schema`); the `typeMappings` list, where a `ValueObjectMapping` describes each object type and a `ValueField` wires each field to a function over the backing value; and the `selectElaborator`, which turns the argument `book(id: 2)` into a `Filter`/`Unique` over the data. Run a query against it with `QuickStartMapping.compileAndRun(...)`.

The catalog distinguishes two kinds of entry. A **`TypeMapping`** maps a whole GraphQL type: `ObjectMapping` for object, interface and union types (it holds the field mappings), and `LeafMapping[T]` for scalar and enum types (it holds a circe `Encoder[T]`). Within an object mapping, a **`FieldMapping`** — `ValueField`, `CursorField`, `Delegate`, `EffectField` and others — wires one field to your data. Built-in leaf mappings for `String`, `Int`, `Float`, `Boolean` and `ID` are appended for you, so you only declare `LeafMapping`s for *custom* scalars and enums. (There is no `PrimitiveMapping` type; `LeafMapping` is the leaf construct.) The full catalog is documented in the [mapping types reference](../reference/mapping-types.md), and the mechanism by which a mapping produces a cursor over your data is explained in [mappings and cursors](../concepts/mappings-cursors.md).

## The backend menu: which `Mapping` to extend

You do not implement `Mapping` from scratch. You extend one of the backend-specific subclasses, each of which knows how to build a `Cursor` over a particular kind of data. Choose by where your data lives:

| Backend | Extend | Use it when your data is… |
| --- | --- | --- |
| In-memory values | `ValueMapping[F]` | ordinary Scala values (case classes, lists) you hold in memory or fetch yourself. |
| Generic derivation | `GenericMapping[F]` | Scala ADTs you would rather not wire field-by-field — cursors are derived from the data types. |
| circe JSON | `CirceMapping[F]` | already-assembled `io.circe.Json` documents (e.g. a response from another service). |
| SQL | `SqlMapping[F]` | rows in a relational database; the mapping compiles the query algebra down to SQL and batches joins. |

The starting points are:

- **`ValueMapping`** — the simplest backend. You describe each field with `ValueField`/`CursorField` over an in-memory value. Good for prototypes, small fixed datasets, and data you assemble in your own effect before mapping. The [quick start](quick-start.md) builds one from scratch, and [filter, sort and page a field](../how-to/filtering-ordering-paging.md) extends it.
- **`GenericMapping`** — derives cursors directly from your Scala data types, so you write less field-by-field wiring. It is the basis of the [in-memory model tutorial](../tutorial/in-memory-model.md) (the Star Wars model); see also [serve Scala ADTs with generic derivation](../how-to/generic-derivation.md).
- **`CirceMapping`** — serves GraphQL straight from `io.circe.Json`, which is convenient when you already have JSON in hand. See [serve GraphQL from circe JSON](../how-to/circe-backend.md).
- **`SqlMapping`** — the database backend. It is the most powerful: it turns the `Query` algebra into SQL, assembles joins and embeddings, and batches nested fields into single round-trips. This is the [DB-backed model tutorial](../tutorial/db-backed-model.md) and the [SqlMapping reference](../reference/sql-mapping.md).

These are not mutually exclusive. A `ComposedMapping` can *federate* several mappings — delegating different parts of one schema to different backends and stitching the results together — which is how Grackle joins, say, a SQL table to a value-backed lookup. See [compose multiple mappings](../how-to/compose-mappings.md).

A note on paging and subscriptions while you weigh backends: Grackle has **no built-in Relay `Connection` type**. You assemble paging by hand from the `Limit`, `Offset`, `OrderBy` and `Count` algebra nodes (plus cursor predicates) — see [filter, sort and page a field](../how-to/filtering-ordering-paging.md). Likewise there is **no built-in websocket transport**; subscriptions are plain `fs2.Stream`s that you wire to a transport of your choosing.

## Effects and `F[_]`

Every mapping is parameterised by an effect type `F[_]`. A `Mapping[F[_]]` requires `implicit val M: MonadThrow[F]` — `F` must be a `cats.MonadThrow`, so it can sequence effects and raise errors. In practice `F` is `cats.effect.IO` (or any cats-effect-compatible effect), which is why the examples extend, for instance, `ValueMapping[IO]`.

Two consequences are worth keeping straight from the start:

- **Subscriptions are fs2 streams.** `compileAndRunSubscription` returns a `Stream[F, Json]`. `compileAndRun` runs the same machinery but expects exactly one result, so use the subscription entry point for anything streaming.
- **Internal errors are raised into `F`, not returned as GraphQL `errors`.** Grackle accumulates expected, query-level problems in a four-armed `Result` type (`Success`, `Warning`, `Failure`, `InternalError`); `Failure` and `Warning` carry the `Problem`s that surface in the response `errors` array, while `InternalError` is raised into the effect `F` instead and never appears in the JSON. How to construct and report errors is covered in [construct, accumulate and report errors](../how-to/errors.md).

The effect model — including how nested effectful fields are batched — is explained in [effects and batching internals](../concepts/effects-batching.md).

## Where to go next

You now have the vocabulary: a **schema**, a **mapping** that ties it to a backend, a **compiler** that elaborates queries into a **`Query` algebra**, and an **interpreter** that walks that algebra with a **`Cursor`**. From here:

1. [Installation](install.md) — add Grackle to your build.
2. [Quick start: your first query](quick-start.md) — run the book example above end to end.
3. [In-memory model tutorial](../tutorial/in-memory-model.md) then the [DB-backed model tutorial](../tutorial/db-backed-model.md) — build a real service step by step.

## See also

- [Quick start: your first query](quick-start.md)
- [In-memory model tutorial](../tutorial/in-memory-model.md)
- [Architecture overview](../concepts/architecture.md)
- [Mappings and cursors](../concepts/mappings-cursors.md)
- [Mapping types reference](../reference/mapping-types.md)
