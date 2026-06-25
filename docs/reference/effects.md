# Effects reference (RootEffect / RootStream / EffectHandler)

This page is the authoritative signature reference for Grackle's effects subsystem: the `Effect`
algebra node, the `EffectMapping` field mappings (`RootEffect`, `RootStream`, `EffectField`), every
root-effect factory (including the circe extensions), and the `EffectHandler` contract with its
ordering guarantee. It is aimed at developers wiring effects into a `Mapping`; for the narrative of
*why* effects are deferred and batched see [How effects and batching work](../concepts/effects-batching.md),
and for task-oriented recipes see [the effects how-to](../how-to/effects-batching.md). All signatures
are taken from `modules/core/src/main/scala` (and `modules/circe/src/main/scala` for the circe
extensions).

## `Effect` node and `EffectMapping`

Effects attach to a field at one of two points: a **root** field (`RootEffect`/`RootStream`), which runs
once before the rest of the query is interpreted, or a **nested** field (`EffectField`), whose effect is
deferred into an `Effect` algebra node and resolved (batched) at the end of a stage.

| Type | File | Signature |
| --- | --- | --- |
| `Effect` | `modules/core/src/main/scala/query.scala` | `case class Effect[F[_]](handler: EffectHandler[F], child: Query) extends Query` |
| `EffectMapping` | `modules/core/src/main/scala/mapping.scala` | `trait EffectMapping extends FieldMapping { def subtree: Boolean = true }` |

`Effect` is the query-algebra node that embeds a possibly-batched deferred effect; the
[`EffectElaborator`](query-algebra.md) compiler phase inserts it around any field backed by an
`EffectField`. `EffectMapping` is the common supertype of `EffectField`, `RootEffect`, and `RootStream`;
`subtree = true` marks the field as owning its entire selection subtree.

## `RootEffect`

A `FieldMapping` for a top-level (`Query`/`Mutation`/`Subscription`) field that performs an initial effect
and yields an effect-specific `Query` plus a root `Cursor`. The primary constructor is **private** — always
construct one through the companion-object factories below (the bare `apply` additionally takes a
`DummyImplicit` to disambiguate its overloads).

| Member | Signature |
| --- | --- |
| type | `case class RootEffect private (fieldName: String, effect: (Query, Path, Env) => F[Result[(Query, Cursor)]]) extends EffectMapping` |
| `hidden` | `def hidden = false` |
| `toRootStream` | `def toRootStream: RootStream` |
| `apply` | `def apply(fieldName: String)(effect: (Query, Path, Env) => F[Result[(Query, Cursor)]])(implicit pos: SourcePos, di: DummyImplicit): RootEffect` |
| `computeUnit` | `def computeUnit(fieldName: String)(effect: Env => F[Result[Unit]])(implicit pos: SourcePos): RootEffect` |
| `computeCursor` | `def computeCursor(fieldName: String)(effect: (Path, Env) => F[Result[Cursor]])(implicit pos: SourcePos): RootEffect` |
| `computeChild` | `def computeChild(fieldName: String)(effect: (Query, Path, Env) => F[Result[Query]])(implicit pos: SourcePos): RootEffect` |

Factory semantics:

- **`apply`** — full control: perform the effect and return both an (effect-specific) query and its
  corresponding root cursor.
- **`computeUnit`** — run a side effect only (e.g. a mutation write); the elaborated client query and the
  mapping's default root cursor are left unchanged. The only channel for passing results downstream is the
  `Env` attached to the returned cursor (the factory calls `qc.map(_.withEnv(env))`).
- **`computeCursor`** — run the effect and yield a custom root `Cursor`; the elaborated query is used
  unchanged. This is the form used by `ValueMapping`/`CirceMapping` (via `valueCursor`/`circeCursor`).
- **`computeChild`** — run the effect to compute a replacement *child* query, which is then executed
  against the mapping's default root cursor (e.g. to inject a `Filter`/`Limit` derived from the effect).

`toRootStream` lifts a `RootEffect` into a single-element `RootStream` (`Stream.eval(effect(...))`) so a
mutation can be served through the subscription path.

## `RootStream`

The streaming analogue of `RootEffect`, used for **subscriptions**. The effect returns
`Stream[F, Result[(Query, Cursor)]]`, emitting one result per stream element. It is **only** valid in a
subscription: for a normal query/mutation `runOneShot` detects a reachable `RootStream` and returns
`Result.internalError("RootStream only permitted in subscriptions")`.

| Member | Signature |
| --- | --- |
| type | `case class RootStream private (fieldName: String, effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]]) extends EffectMapping` |
| `hidden` | `def hidden = false` |
| `apply` | `def apply(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]])(implicit pos: SourcePos, di: DummyImplicit): RootStream` |
| `computeCursor` | `def computeCursor(fieldName: String)(effect: (Path, Env) => Stream[F, Result[Cursor]])(implicit pos: SourcePos): RootStream` |
| `computeChild` | `def computeChild(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[Query]])(implicit pos: SourcePos): RootStream` |

`RootStream` has no `computeUnit` (a unit effect would emit no values); use `computeCursor` or
`computeChild`. There is no built-in websocket/`graphql-ws` transport — a subscription is just an
`fs2.Stream` you wire to a transport of your choosing.

### Worked example: `get` / `put` / `watch` on one `ValueMapping`

The subscription test mapping uses all three root constructors side by side — `RootEffect.computeCursor`
for the `get` query field, another for the `put` mutation field, and `RootStream.computeCursor` for the
`watch` subscription field, all backed by a single `SignallingRef[IO, Int]`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/subscription/SubscriptionSuite.scala", "#subscription_mapping"))
```

Each factory builds its root cursor with `valueCursor(path, env, n)`. The `put` field reads its argument
from the `Env` (populated by the `SelectElaborator`), runs `ref.set(n)`, and yields a cursor over the new
value; `watch` turns `ref.discrete` into a stream that emits a cursor per change.

## circe extensions: `computeJson` / `computeEncodable`

`grackle-circe` adds two convenience constructors to each of `RootEffect` and `RootStream`, via the
implicit classes `CirceMappingRootEffectSyntax` / `CirceMappingRootStreamSyntax` (in
`modules/circe/src/main/scala/circemapping.scala`). They are in scope inside any `CirceMapping`/`SqlMapping`
body. Each is layered on `computeCursor` and builds the cursor for you with `circeCursor`.

| Constructor | Signature |
| --- | --- |
| `RootEffect.computeJson` | `def computeJson(fieldName: String)(effect: (Path, Env) => F[Result[Json]])(implicit pos: SourcePos): RootEffect` |
| `RootEffect.computeEncodable` | `def computeEncodable[A](fieldName: String)(effect: (Path, Env) => F[Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootEffect` |
| `RootStream.computeJson` | `def computeJson(fieldName: String)(effect: (Path, Env) => Stream[F, Result[Json]])(implicit pos: SourcePos): RootStream` |
| `RootStream.computeEncodable` | `def computeEncodable[A](fieldName: String)(effect: (Path, Env) => Stream[F, Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootStream` |

- **`computeJson`** — return raw `io.circe.Json`; the mapping wraps it in a `circeCursor`.
- **`computeEncodable`** — return any `A` with an `Encoder[A]` in scope; it is encoded to `Json` and then
  handled as `computeJson`.

The circe effects test mapping shows `computeCursor` (field-focussed and root-focussed), `computeJson`,
and `computeEncodable` together:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CirceEffectData.scala", "#circe_effects"))
```

Independent root fields are **not** batched: a query selecting `foo`, `bar`, and `baz` runs three separate
root effects (the counter ends at 3). Note also that `qux` focuses its cursor on the root with
`Path.from(p.rootTpe)` and nests the field name inside the `Json`, whereas `foo` builds a field-focussed
cursor over the bare struct — choosing the wrong focus produces a shape mismatch against the schema.

## `EffectField` and `EffectHandler`

`EffectField` is a `FieldMapping` for a **nested** field. It names an `EffectHandler[F]` and an optional
list of `required` sibling columns that must be present in the parent query so the handler can read them
off the parent `Cursor`.

| Type | File | Signature |
| --- | --- | --- |
| `EffectField` | `modules/core/src/main/scala/mapping.scala` | `case class EffectField(fieldName: String, handler: EffectHandler[F], required: List[String] = Nil, hidden: Boolean = false)(implicit val pos: SourcePos) extends EffectMapping` |
| `EffectHandler` | `modules/core/src/main/scala/query.scala` | `trait EffectHandler[F[_]] { def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] }` |

`EffectField` parameters:

- **`fieldName`** — the nested field this effect backs.
- **`handler`** — the `EffectHandler` invoked once per stage with the full batch for this field.
- **`required`** — sibling fields/columns to fetch in the parent so the handler can read them via
  `parentCursor.fieldAs[T](name)`. In `SqlMapping` this drives `columnsForLeaf`, adding the columns to the
  parent `SELECT`. Omitting a needed column makes the corresponding `fieldAs` fail at runtime.
- **`hidden`** — whether the field is suppressed from introspection.

### `runEffects` contract and the one-cursor-per-input rule

`runEffects` receives one `(continuation-query, parent-cursor)` pair per **occurrence** of the field across
the whole result, and must return **exactly one continuation `Cursor` per input pair, in the same order**.
The interpreter pairs the returned cursors back against the inputs by position (`(conts, cs).parMapN`) and
scatters the resulting JSON into the original tree; a mismatched length or order silently corrupts the
result. Handlers that group/batch internally must restore the original order (the SQL examples capture
indices and `sortBy(_._2)`).

The `Query` handed to the handler is the **continuation child** (the selection set under the effect field),
not the original field `Select`. The interpreter runs `Query.extractChild` on each input and raises
`"Continuation query has the wrong shape"` if the returned cursor's query is not a single `Select`-shaped
child.

A minimal handler that batches a service call over a nested SQL field, reading the `required` `code2`
column off each parent cursor and making a single `currencyService.get(distinctCodes)` call for the whole
batch:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlNestedEffectsMapping.scala", "#currency_handler"))
```

The matching `ObjectMapping` declares the field with `EffectField("currencies", CurrencyQueryHandler, List("code2"))`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlNestedEffectsMapping.scala", "#effect_typemappings"))
```

> **Batch key is `(mapping, handler)` identity.** To collapse all occurrences into one `runEffects` call you
> must reuse the *same* handler object instance across them (e.g. `object CurrencyQueryHandler extends EffectHandler[F]`).
> Constructing a fresh handler per field or per row splits the batch and reintroduces the N+1.

## Supporting helpers

| Helper | File | Signature | Role |
| --- | --- | --- | --- |
| `Query.childContext` | `modules/core/src/main/scala/query.scala` | `def childContext(c: Context, query: Query): Result[Context]` | Derives the GraphQL [`Context`](context-env.md) for the continuation query a handler must produce (e.g. to build a `CirceCursor` child). |
| `Query.extractChild` | `modules/core/src/main/scala/query.scala` | `def extractChild(query: Query): Option[Query]` | Pulls the inner query out of a `Select`/`Environment`; the interpreter uses it to get the continuation each returned cursor is run against. |
| `Mapping.combineAndRun` | `modules/core/src/main/scala/mapping.scala` | `def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]]` | Default batch runner for *component*-delegated deferrals; overridable (e.g. SQL mappings combine queries). `EffectHandler`-based deferrals bypass this and call `handler.runEffects` directly. |

## Where effects run

- **Compilation.** `EffectElaborator` (added by `Mapping.compilerPhases` after the select and component
  elaborators) rewrites `Select(fieldName, resultName, child)` into
  `Select(fieldName, resultName, Effect(handler, Select(fieldName, resultName, child)))` for every field that
  has an `EffectField` mapping — the `Effect` node wraps the field's own (elaborated) `Select`, not just its
  bare child, which is why the handler later recovers the continuation with `Query.extractChild`. The
  interpreter then defers that field instead of evaluating it inline.

- **Roots.** `QueryInterpreter.runOneShot` partitions the ungrouped root queries into effectful (those with
  a `RootEffect`) and pure. Each effectful root runs its `RootEffect` first, then `runValue` is applied to
  the returned `(query, cursor)`. If any reachable root is a `RootStream` outside a subscription, it
  returns `Result.internalError("RootStream only permitted in subscriptions")`.

- **Nested fields.** Interpreting `Effect(handler, cont)` produces a deferred `ProtoJson.effect(mapping, handler, query, cursor)`
  placeholder (an `EffectJson`) rather than evaluating inline. At the end of the stage, `completeAll`
  gathers every deferred placeholder across the `ProtoJson` tree and groups them with
  `.groupMap(ej => (ej.mapping, ej.handler))(identity)`. Each group becomes exactly one
  `handler.runEffects` call carrying the whole batch — this is where N+1 collapses into a single call. The
  continuation child of each input is then run against its returned cursor, and the JSON is scattered back
  into the placeholders by identity.

Each distinct nested effect field therefore costs **one extra interpreter stage**, but every occurrence of
that field shares the stage. Doubly-nested effects (an effect field inside another effect field's subtree)
run in successive stages, each handler's continuation being interpreted as a fresh sub-run. Note that
`runEffects` returning `Result.failure`/`Warning` surfaces as `Problem`s in the response `errors`, whereas
`Result.internalError` is raised into `F` and never appears in the JSON `errors` array — see
[`Result` and `Problem`](result-problem.md).

## See also

- [How effects and batching work](../concepts/effects-batching.md) — the mechanism and rationale behind deferred, batched effects.
- [Effects and batching how-to](../how-to/effects-batching.md) — recipes for root effects and nested `EffectHandler` batching.
- [Mutations and subscriptions tutorial](../tutorial/mutations-subscriptions.md) — `RootEffect`/`RootStream` end to end.
- [Cursor reference](cursor.md) and [Context & Env reference](context-env.md) — what a handler reads from and builds.
- [Query algebra reference](query-algebra.md) — the `Effect` node among the other algebra nodes and elaboration phases.
- [SQL mapping reference](sql-mapping.md) and [circe mapping reference](circe-mapping.md) — the backends that supply `computeJson`/`computeEncodable` and `combineAndRun` overrides.
