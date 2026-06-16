# Architecture overview

Grackle is a GraphQL server built as a *compiler* and an *interpreter*. A query string is parsed and rewritten into a small algebra, that algebra is interpreted against your data, and the result is assembled into JSON. This page is the mental model the rest of the Concepts section hangs off: it walks the three layers end to end, shows how a `Mapping` bundles the pieces together, and points at the deeper page for each part. It assumes you are a Scala developer comfortable with cats-effect; it does not re-teach GraphQL.

## The three layers

Every operation moves through three stages. Nothing in between is magic — each stage has a concrete type, and you can inspect the value at each boundary.

```text
  GraphQL text
       │
       ▼
┌──────────────────────┐   parse → validate → elaborate (phases)
│    QueryCompiler      │   in the Elab monad: StateT[Result, ElabState, _]
└──────────┬───────────┘
           │  Operation(query: Query, rootTpe: NamedType)
           ▼
┌──────────────────────┐   the Query algebra — a sealed ADT:
│     Query (algebra)   │   Select, Filter, Unique, Limit, Offset,
└──────────┬───────────┘   OrderBy, Count, Component, Effect, Environment, …
           │
           ▼
┌──────────────────────┐   walks the algebra against a root Cursor,
│   QueryInterpreter    │   driven by the Schema type at each step
│   (+ Mapping/Cursor)  │
└──────────┬───────────┘
           │  ProtoJson  (a possibly-partial JSON tree)
           ▼      └── completeAll: batch & resolve deferred subtrees, stage by stage
        io.circe.Json
                                          ┌─────────────────────────────┐
   every step above threads a  ───────────│  Result[+T]                 │
   Result, so errors/warnings             │  Success | Warning |        │
   accumulate instead of throwing         │  Failure | InternalError    │
                                          └─────────────────────────────┘
```

**Layer 1 — compile.** A [`QueryCompiler`](compiler-elaboration.md) turns the query string into an executable `Operation`. It parses the text into an *untyped* tree, validates variables, fragments and field mergeability, then folds a sequence of `Phase`s over the tree. The result is an `Operation` carrying a `Query` value and the root `NamedType`.

**Layer 2 — the query algebra.** `Query` is a sealed ADT — a tree of interpretable nodes such as `Select`, `Filter`, `Unique`, `Limit`, `Offset`, `OrderBy`, `Count`, `Component`, `Effect` and `Environment`. It is the contract between the two halves of the system: the compiler's only job is to *produce* a `Query`, and the interpreter's only job is to *consume* one. Because it is an ordinary data structure, it is inspectable and testable in isolation.

**Layer 3 — interpret.** The [`QueryInterpreter`](query-interpreter.md) walks the `Query` against a root [`Cursor`](mappings-cursors.md), dispatching on each node *and* the GraphQL type it is expected to produce. It builds a `ProtoJson` — a possibly-partial JSON tree whose deferred subtrees (component joins, effectful fields) are resolved later. A final `completeAll` pass batches and resolves those deferred subtrees and substitutes the results back in, yielding `io.circe.Json`.

Alongside all three layers runs a single error channel: every step returns a [`Result`](../reference/result-problem.md), so failures *accumulate* rather than throw. More on that [below](#result-the-error-channel).

## A Mapping bundles the whole pipeline

You do not construct compilers and interpreters yourself. A [`Mapping[F]`](mappings-cursors.md) is the one abstraction you write, and it derives everything else. A mapping holds three things:

- a `schema` — the GraphQL `Schema` it serves;
- a `typeMappings` catalog — how each GraphQL type and field is backed by data;
- a `selectElaborator` — the query-rewriting phase that turns field *arguments* into algebra.

From those, the trait builds the `compiler` and `interpreter` and exposes the top-level entry points `compileAndRun` and `compileAndRunSubscription`. Here is a complete in-memory mapping over an ordinary list of Scala values, showing all three pieces in one object.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/docs/src/main/scala/grackle/QuickStartMapping.scala", "#quickstart"))
```

Read it as the pipeline in miniature. The `schema""" … """` interpolator validates the SDL at compile time and yields a bare `Schema` (its runtime sibling, `Schema(text)`, returns a `Result[Schema]` instead — use the interpolator when the text is fixed). The `typeMappings` list pairs each GraphQL type with a way to read it: a `ValueObjectMapping` for the `Query` and `Book` object types, whose `ValueField`s project a function out of the focused Scala value (`_.title`, `_.author`). The `selectElaborator` is the rewriting step: it matches the `book(id:)` field and emits `Elab.transformChild(child => Unique(Filter(Eql(BookType / "id", Const(id)), child)))`, replacing the field's argument with a predicate the interpreter can run.

That last move is the heart of the compiler half. A `selectElaborator` is a `PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]`: given the type, field name and parsed arguments, it returns an action in the `Elab` monad that rewrites the field's subtree. Most of the work of writing a mapping is deciding, field by field, how an argument becomes a `Filter`, `Limit`, `OrderBy` or `Env` entry. The [compiler and elaboration](compiler-elaboration.md) page covers the `Elab` monad, the built-in phases (introspection, variable substitution, fragment expansion, field merging) and the order they run in.

## Cursors navigate the backing data

Where the compiler produces a `Query`, the interpreter needs something that knows how to *read* your data. That is the [`Cursor`](mappings-cursors.md): a read-only navigator pointing at one position in an arbitrary backing model during execution. A cursor carries a `focus` (the current value, typed `Any`), a `Context` (its path through the schema plus the GraphQL `Type` it is expected to represent), an optional parent, and an `Env`. Its navigation methods — `field`, `narrow`, `asLeaf`, `asList`, `asNullable` — are *type-directed*: each pattern-matches on the GraphQL type **and** the runtime value together.

The interpreter's `runValue` makes this concrete. It dispatches on the pair `(query, tpe.dealias)`: a scalar or enum type becomes `cursor.asLeaf` (a `Json` leaf); an object, interface or union recurses through its fields; a `ListType` drives `asList`; a `NullableType` unwraps via `asNullable` (which returns a two-layer `Result[Option[Cursor]]` — `None` maps to JSON `null`). Because the navigation is driven by the schema type rather than the value's class, the very same algebra runs unchanged whether the focus is an in-memory case class, a circe `Json`, or a row materialised from SQL.

That is also why Grackle has many backends behind one interpreter: each backend supplies its own `Cursor` and overrides one extension point. `Mapping.mkCursorForMappedField` is the protected hook that produces a backend-specific cursor for a field — `ValueMapping` applies a function to the parent focus, `CirceMapping` reads a JSON subtree, `SqlMapping` reads a column or follows a join, `ComposedMapping` returns a cursor that delegates to another mapping. Swap the cursor and you swap the backend; the algebra and the interpreter are untouched.

## Staging: deferred subtrees and `completeAll`

Not every field can be resolved in one pass. A `Component` node (a field served by a *different* mapping) or an `Effect` field (one that performs an `F` effect) cannot be filled in immediately, so the interpreter leaves a placeholder in the `ProtoJson` tree instead of a finished value. `ProtoJson` is exactly that: an opaque, possibly-partial JSON value that is either complete `io.circe.Json` or a wrapper holding deferred leaves.

`completeAll` resolves them in stages. It gathers every deferred leaf, **groups them by the mapping (or handler) responsible**, and evaluates each group in one batch — calling that mapping's `combineAndRun`, which is the override point a backend uses to turn N deferred sub-queries into a single round trip (SQL mappings batch a stage into one statement here). It then recurses to complete any newly-revealed deferrals, and finally scatters the results back into the enclosing JSON by object identity. This staged, batch-per-mapping design is what makes cross-mapping [composition](composition.md) and [effectful fields and batching](effects-batching.md) efficient rather than N+1.

A consequence worth keeping in mind: Grackle has no built-in Relay `Connection` type and no built-in websocket transport. Paging is assembled by hand from `Limit` / `Offset` / `OrderBy` / `Count` nodes (plus cursor predicates), and subscriptions are plain `fs2.Stream`s you wire to a transport yourself.

## `Result`: the error channel

Running parallel to all three layers is `Result[+T]`, a four-armed type that lets Grackle accumulate problems instead of throwing:

- `Success(value)` — a value, no problems.
- `Warning(problems, value)` — a value **and** non-fatal `Problem`s. The interpreter uses this to keep partial data while still reporting field-level errors.
- `Failure(problems)` — no value; GraphQL errors.
- `InternalError(throwable)` — a programming/infrastructure fault.

The crucial distinction for the response shape: the `Problem`s carried by `Failure` and `Warning` surface in the GraphQL response's `errors` array, but an `InternalError` does **not** — it is raised into the effect `F` instead. Never assume an internal error comes back as JSON. (At the root, a top-level `Failure` is even converted to `Warning(errs, null)` so a hard failure still yields the `data: null` + `errors` shape GraphQL clients expect.) The full type, its combinators and `Problem` are covered in the [Result reference](../reference/result-problem.md).

## Where to go next

Each layer has a dedicated concept page that picks up where this overview stops:

- **[The compiler and elaboration](compiler-elaboration.md)** — the `Elab` monad, `SelectElaborator`, and the built-in phase pipeline.
- **[Mappings and cursors](mappings-cursors.md)** — the `Mapping` catalog, the cursor abstraction, and `mkCursorForMappedField`.
- **[How the query interpreter works](query-interpreter.md)** — `runValue`/`runFields`, `ProtoJson`, and type-directed dispatch.
- **[How cross-mapping delegation executes](composition.md)** — `Component` nodes, staging and `combineAndRun`.
- **[Effects and batching internals](effects-batching.md)** — `Effect` fields, deferred leaves and per-mapping batching.

## See also

- [What is Grackle?](../getting-started/overview.md) — the one-screen orientation and backend menu.
- [Quick start: your first query](../getting-started/quick-start.md) — build and run a mapping end to end.
- [The schema model](schema-model.md) — how the GraphQL `Type` ADT is represented internally.
- [Query algebra reference](../reference/query-algebra.md) — every node of the `Query` ADT.
- [Result, Problem & ResultT reference](../reference/result-problem.md) — the error channel in full.
