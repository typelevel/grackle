# How the query interpreter works

This page explains how Grackle turns an elaborated `Query` and a root [`Cursor`](mappings-cursors.md) into a JSON response. It is for developers debugging execution — especially the "Mismatched query and cursor type" internal errors — who want a mental model of the interpreter's type-directed dispatch, its partial-result tree (`ProtoJson`), how deferred and effectful subtrees are batched, and how field-level errors are accumulated rather than thrown. It assumes you have already met the [query algebra and elaboration](compiler-elaboration.md) phases that produce the `Query` the interpreter consumes; here we pick up where elaboration leaves off.

## From query to JSON: the staged pipeline

`QueryInterpreter[F]` is constructed from a `Mapping[F]` and exposes a single public driver:

```scala
def run(query: Query, rootTpe: Type, env: Env): Stream[F, Result[Json]]
```

`run` builds a synthetic root `Cursor` — `RootCursor(Context(rootTpe), None, env)`, whose focus is `()` — and then branches on whether `rootTpe` is the schema's subscription type:

- For an ordinary query or mutation it calls `runOneShot`, evaluated once via `Stream.eval`.
- For a subscription it calls `runSubscription`, which yields one element per emitted stream value.

Either path produces a `ProtoJson` — a *possibly-incomplete* result tree (see [below](#protojson-a-partial-result-tree)). `run` then threads each `ProtoJson` through `QueryInterpreter.complete`, which resolves any deferred subtrees into a fully-concrete `io.circe.Json`. So the overall shape is:

```text
run
 ├─ runSubscription   (subscription root)   ─┐
 └─ runOneShot        (query / mutation root)─┤
                                             ▼
                          runRootValue ─► runValue ─► ProtoJson  (one stage)
                                             ▼
                                  complete / completeAll          (resolve deferred subtrees)
                                             ▼
                                         io.circe.Json
```

`runOneShot` does a little orchestration before it touches the model. It `ungroup`s the top-level selections and partitions them into *pure* queries, *effectful* queries (root effects, e.g. mutations), and *introspection* queries, runs each group, and merges the results with the `Result` semigroup. A pure, non-introspection selection is handed to `runRootValue`, which obtains the mapping's default root cursor (`mapping.defaultRootCursor`) and then calls the recursive core, `runValue`. An effectful selection first runs its `RootEffect` to obtain a `(query, cursor)` pair and then calls `runValue` directly. Introspection is interpreted by a separate introspection interpreter against the same root cursor.

Two guards live here. If a `RootStream` field (only meaningful for subscriptions) appears in a one-shot query, `runOneShot` returns `Result.internalError("RootStream only permitted in subscriptions")`. Symmetrically, `runSubscription` rejects more than one root selection with `"Only one root selection permitted for subscriptions"`. There is no built-in graphql-ws transport; `runSubscription` produces a plain `fs2.Stream` you wire to a transport yourself (see [effects and batching](effects-batching.md)).

## Type-directed dispatch in `runValue`

`runValue` is the heart of the interpreter. It interprets one `Query` node against one `Cursor` at an expected GraphQL `Type`, and it decides what to do by pattern-matching on the pair `(query, tpe.dealias)`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/main/scala/queryinterpreter.scala", "#run_value"))
```

Read this as a dispatch table keyed first on the structural query node, then on the dealiased type:

- **`Environment(childEnv, child)`** — push the extra `Env` bindings onto the cursor (`cursor.withEnv`) and recurse. This is how `Query.Environment` nodes inserted during elaboration make values visible to predicates and joins downstream.
- **Scalar / enum types (`ScalarType | EnumType`)** — terminal. Call `cursor.asLeaf`, which renders the focus as circe `Json`, and wrap it with `ProtoJson.fromJson`.
- **Object / interface / union types** — delegate to `runFields`, then assemble the named field results into an object with `ProtoJson.fromDisjointFields`.
- **`NullableType(tpe)`** — call `cursor.asNullable` (a `Result[Option[Cursor]]`). `Some(child)` recurses on the inner type; `None` becomes `ProtoJson.fromJson(Json.Null)`. This is where absent optional values turn into JSON `null`.
- **`ListType(tpe)`** — delegate to `runList` (with `unique = false`).
- **`Unique(child)`** — call `cursor.preunique` to re-type the focus as a list, then `runList(child, tpe.nonNull, c, unique = true, nullable = tpe.isNullable)`. `Unique` is how an elaborated query collapses a list down to its single matching element.
- **`Component(...)`** and **`TransformCursor(f, child)`** — the staged and cursor-rewriting cases, covered under [deferred evaluation](#deferred-and-staged-evaluation) and list transforms below.

If no arm matches, `runValue` returns `Result.internalError(s"Stuck at type $tpe for ${query.render}")` — a sign the elaborated query and the schema type disagree about shape.

Note the inversion to keep in mind when reading this code: in Grackle's internal `Type` model a bare type is already non-null, and optionality is the explicit `NullableType` wrapper matched above — the opposite of SDL, where `String` is nullable and `String!` is not. See the [schema model](schema-model.md) for the full story.

### Object selections in `runFields`

`runFields` produces the `List[(resultName, ProtoJson)]` pairs that `runValue` folds into an object. It dispatches on the query node:

- **`Group`** — flatten and concatenate the sub-results; a `Group` that contains type-case branches additionally `mergeFields` so that selections spread across fragments on the same field are merged.
- **`Select` on a nullable type** — unwrap with `asNullable`; an absent value short-circuits the whole field to `Json.Null`.
- **`Select(_, _, Count(...))`** — resolve the counted child, take its `listSize` (or `1` for a non-list), and emit a JSON number. There is no `Connection` machinery here; counting is `Count` over a field, nothing more.
- **`Introspect(__typename)`** — for an object yield its name; for an interface or union, narrow the cursor against each candidate implementation/member and report the first that matches.
- **`Select(fieldName, resultName, child)`** — the common case: descend with `cursor.field(fieldName, resultName)` and recurse into `runValue` at the field's type. The field type defaults to `ScalarType.AttributeType` for synthetic attributes not present in the schema.
- **`Narrow(subtpe, child)`** — narrow the cursor to `subtpe`; if the runtime value is not a member, contribute no fields (the fragment does not apply).
- **`Component`**, **`Effect`**, **`Environment`**, **`TransformCursor`** — produce deferred field values or rewrite the cursor, as in `runValue`.

### List handling in `runList`

`runList` iterates the parent cursor as a list (`parent.asList(Iterator)`) and applies, in order, the operations an elaborated `FilterOrderByOffsetLimit` carries:

1. **filter** — keep elements for which the `Predicate` holds (via `filterA`, so a predicate failure aborts the list);
2. **order** — apply `OrderSelections`;
3. **offset / limit** — `drop`, `take`, or `slice` the sorted iterator.

These are the primitives behind [filtering, ordering, and paging](../how-to/filtering-ordering-paging.md) — there is no ready-made cursor-paging or Relay `Connection` helper; you assemble paging from `Limit`/`Offset`/`OrderBy`/`Count` by hand. After slicing, each surviving element is interpreted with `runValue`. When `unique = true` (the `Unique` case), `runList` enforces cardinality on the result: exactly one match returns that element; zero matches returns `Json.Null` if the type is nullable, otherwise `Result.internalError("No match")`; more than one returns `Result.internalError("Multiple matches")`. A leading `TransformCursor` is peeled off and applied to the whole list via a `ListTransformCursor`, letting a backend re-project the element set (the SQL backend uses this to substitute query-shaped rows).

## `cursorCompatible`: why mismatches are internal errors

Before walking, `runValue`, `runFields`, and each element loop in `runList` call `cursorCompatible(tpe, cursor.tpe)`. It strips nullable and list wrappers off both the query's expected type and the cursor's reported type, then accepts only if both strip down to leaves, or if the stripped types are nominally equal (`nominal_=:=`):

```scala
def cursorCompatible(tpe: Type, cursorTpe: Type): Boolean = {
  def strip(tpe: Type): Type =
    tpe.dealias match {
      case NullableType(tpe) => strip(tpe)
      case ListType(tpe) => strip(tpe)
      case _ => tpe
    }

  (strip(tpe).isLeaf && strip(cursorTpe).isLeaf) ||
  (strip(tpe) nominal_=:= strip(cursorTpe))
}
```

When this guard fails you get `"Mismatched query and cursor type in runValue"` (or `runFields` / `runList`). This is deliberately an **internal error**, not a GraphQL error: it almost always means a custom `Cursor` reported a `tpe` that does not line up with the query, usually because its `mkChild`/`context.asType` re-typing is wrong. Per Grackle's error model, internal errors are raised into the effect `F` rather than surfaced in the response `errors` array — so a `cursorCompatible` failure is a bug to fix in your mapping, not something a client ever sees as a field error. If you are implementing your own cursor, keeping the focus value and `Context` type in lockstep is what keeps this guard happy.

## `ProtoJson`: a partial-result tree

`runValue` does not build `io.circe.Json` directly. It builds `ProtoJson`, an opaque type (`type ProtoJson <: AnyRef`) representing a *partially-constructed* result. A `ProtoJson` is either a complete `io.circe.Json`, or — when some subtree is deferred — one of the package-private wrappers `ProtoObject`, `ProtoArray`, `ProtoSelect`, or a `DeferredJson` leaf (`EffectJson`).

The crucial property is **eager collapse**: the constructors `fromJson`, `fromFields`, `fromDisjointFields`, `fromValues`, and `select` collapse straight to a plain `Json` node the moment all of their children are already `Json`. Deferral wrappers therefore appear only when a subtree genuinely needs later resolution. You can build a `ProtoJson` only through these constructors and inspect it only via `ProtoJson.isDeferred` — its internal cases are private to `QueryInterpreter`, so you cannot pattern-match its structure from outside the module.

For a query with no components or effects, this means the entire result is already concrete `Json` by the time `runValue` returns. You can see the collapse directly:

```scala mdoc:silent
import grackle.QueryInterpreter
import grackle.QueryInterpreter.ProtoJson
import io.circe.Json
import cats.effect.IO
import cats.effect.unsafe.implicits.global

val pj: ProtoJson = ProtoJson.fromFields(Seq(
  "a" -> ProtoJson.fromJson(Json.fromInt(1)),
  "b" -> ProtoJson.fromJson(Json.fromString("x"))
))
```

```scala mdoc
ProtoJson.isDeferred(pj)

QueryInterpreter.complete[IO](pj).unsafeRunSync()
```

Because every field above is already `Json`, `fromFields` collapses to a plain object, `isDeferred` is `false`, and `complete` returns it unchanged. `fromDisjointFields` (used for the object case in `runValue`) is the same idea but assumes field names are already disjoint and does not re-merge them, whereas `fromFields` merges first.

## Deferred and staged evaluation

Two query nodes produce a `DeferredJson` leaf instead of an immediate value:

- **`Component(mapping, join, child)`** — a join into a *composed* sub-mapping (see [composition](composition.md)). `runValue` runs the `join` to derive the continuation query for the related mapping and wraps it as `ProtoJson.component(mapping, cont, cursor)`. For a list-typed component it does this per element.
- **`Effect(handler, cont)`** — a field whose value comes from an effectful, often batched, resolver. `runFields` wraps it as `ProtoJson.effect(mapping, handler, cont, cursor)`.

Both produce an `EffectJson(mapping, handler, query, cursor)` leaf embedded in the surrounding `ProtoJson`. Nothing has run yet; the work is staged. `complete`/`completeAll` then resolves these leaves stage by stage:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/main/scala/queryinterpreter.scala", "#complete_all"))
```

Three sub-steps do the work:

- **gather** — `gatherDeferred` walks each `ProtoJson` and collects every `DeferredJson` leaf. (A `ProtoJson` that is already plain `Json` has none, so it is returned untouched.)
- **batch** — the gathered leaves are grouped by `(mapping, handler)` and each batch is run *once*: a component batch via `mapping.combineAndRun`, an effect batch via `handler.runEffects`. Grouping is what makes batched resolution possible — N sibling deferred fields targeting the same handler become one call, which is how you avoid N+1 queries (the basis of the [effects and batching](effects-batching.md) story). Each batch's results are then `completeAll`-ed recursively, so deferred subtrees nested inside deferred results are resolved in subsequent stages.
- **scatter** — `scatterResults` rebuilds the concrete `Json`, replacing each `DeferredJson` leaf with its resolved value. The substitution uses a `java.util.IdentityHashMap` keyed on the leaf nodes, so results are matched by **object identity**, not equality. Copying or re-creating an `EffectJson` after it has been gathered would break the lookup.

## Error accumulation: `Result`, `Warning` vs `Failure`

The interpreter never throws for query-level problems; it threads everything through `Result[+T]`, which has four arms:

- **`Success(value)`** — a value, no problems.
- **`Warning(problems, value)`** — a value *and* non-fatal `Problem`s. This is how the interpreter keeps partial data while still collecting field-level errors.
- **`Failure(problems)`** — problems, no value. These `Problem`s become the GraphQL `errors` array.
- **`InternalError(throwable)`** — a programming/internal error, raised into the effect `F`. It does **not** appear in the response `errors` array.

`runList` shows the `Warning` mechanism concretely: as it interprets each list element, a per-element `Warning` contributes its `Problem`s to an accumulator but still keeps the element's value, so one bad row in a list does not discard the others. An `InternalError` or `Failure`, by contrast, aborts the list immediately.

At the root, `runOneShot` performs one important conversion. If the merged result of the root selections is a top-level `Result.Failure(errs)`, it rewrites it to `Result.Warning(errs, ProtoJson.fromJson(Json.Null))`. That is what produces the GraphQL `data: null` + `errors` response for a hard root failure, rather than dropping the whole response — the data becomes JSON `null` while the `errors` array carries the problems. The practical consequence when you inspect interpreter output: a root failure shows up as a `Warning` carrying problems, not a bare `Failure`, so check for `Warning` problems too. For the full `Result` / `Problem` model and how problems are formatted into the response, see the [error handling how-to](../how-to/errors.md).

## See also

- [Mappings and cursors](mappings-cursors.md) — what a `Cursor` is and how a `Mapping` builds the cursors this interpreter walks.
- [The compiler and elaboration](compiler-elaboration.md) — how the `Query` the interpreter consumes is produced.
- [Effects and batching](effects-batching.md) — the deferred/batched resolution machinery from the mapping author's side.
- [Composition of mappings](composition.md) — how `Component` joins stage work across mappings.
- [Architecture overview](architecture.md) — where the interpreter sits in the overall pipeline.
- [Cursor reference](../reference/cursor.md) and [query algebra reference](../reference/query-algebra.md) — exact signatures for the types named here.
