# Elab monad & compiler phases reference

This page is the API reference for the elaboration layer of the [query compiler](../concepts/compiler-elaboration.md): the `Elab` monad and its combinators, the `Phase` trait, the `SelectElaborator` you override on a `Mapping`, and the built-in phases the compiler runs. It is for developers writing custom elaborators or phases. For the `Query` ADT these phases rewrite, see the [query algebra reference](query-algebra.md); for the prose walkthrough of how a query becomes an `Operation`, see [compiler & elaboration](../concepts/compiler-elaboration.md). All types here live in `object QueryCompiler` (`modules/core/src/main/scala/compiler.scala`); import them with `import grackle.QueryCompiler._`.

## The `Elab` monad

```scala
type Elab[T] = StateT[Result, ElabState, T]
```

`Elab` is `cats.data.StateT` over [`Result`](result-problem.md), threading an `ElabState` through the traversal of a query and accumulating GraphQL warnings/errors via `Result`. A phase's `transform` walks the [`Query`](query-algebra.md) tree inside `Elab`; elaborators almost never touch `ElabState` directly — they compose the combinators on the `Elab` object below.

### `ElabState`

`ElabState` is the state threaded by `Elab`. It is copied (not mutated) as the traversal descends, with `push`/`pop` saving and restoring it across child selections.

| Field | Type | Holds |
| --- | --- | --- |
| `parent` | `Option[ElabState]` | The saved state of the enclosing node (set by `push`, restored by `pop`). |
| `schema` | `Schema` | The schema the query is compiled against. |
| `context` | `Context` | The [`Context`](context-env.md) (schema position / `TypeRef`) of the node being elaborated. |
| `vars` | `Vars` | Resolved query variables, as `Map[String, (Type, Value)]`. |
| `fragments` | `Map[String, UntypedFragment]` | The query's fragment definitions, by name. |
| `query` | `Query` | The query at the current node (used by `hasField`/`hasSibling`/`resultName`). |
| `localEnv` | `Env` | The [`Env`](context-env.md) bound *at this node* via `Elab.env`; materialised as an `Environment` node. |
| `attributes` | `List[(String, Query)]` | Synthetic attribute selects added via `Elab.addAttribute`. |
| `childTransform` | `Query => Elab[Query]` | The pending transform applied to the *elaborated* child (set by `Elab.transformChild`). |

`push` resets `localEnv`, `attributes`, and `childTransform` for the new node and chains the old state onto `parent`; `pop` fails with `"Cannot pop root state"` if there is no parent.

### Reader combinators

These inspect `ElabState` without changing it.

| Combinator | Type | Returns |
| --- | --- | --- |
| `Elab.schema` | `Elab[Schema]` | The schema being elaborated. |
| `Elab.context` | `Elab[Context]` | The current node's `Context`. |
| `Elab.vars` | `Elab[Vars]` | The resolved variables map. |
| `Elab.fragments` | `Elab[Map[String, UntypedFragment]]` | All fragment definitions. |
| `Elab.fragment(nme: String)` | `Elab[UntypedFragment]` | The named fragment, **failing** with `"Fragment '$nme' is not defined"` if absent. |
| `Elab.hasField(name: String)` | `Elab[Boolean]` | `true` if the current node has a child selection named `name`. |
| `Elab.hasSibling(name: String)` | `Elab[Boolean]` | `true` if the *parent* has a child named `name` (i.e. a sibling of this node). |
| `Elab.fieldAlias(name: String)` | `Elab[Option[String]]` | The alias, if any, of the child named `name`. |
| `Elab.resultName` | `Elab[Option[String]]` | The result name (alias, else field name) of the current node. |
| `Elab.localEnv` | `Elab[Env]` | The env bound directly at this node (not inherited). |
| `Elab.env[T: ClassTag](nme: String)` | `Elab[Option[T]]` | The env value bound to `nme`, searching this node then ancestors; `None` if absent or the stored value is not a `T`. |
| `Elab.envE[T: ClassTag: TypeName](nme: String)` | `Elab[T]` | As `env`, but **fails** with a descriptive message if the key is missing or mistyped. |
| `Elab.attributes` | `Elab[List[(String, Query)]]` | The synthetic attributes added at this node. |
| `Elab.transform` | `Elab[Query => Elab[Query]]` | The accumulated `childTransform` to apply to the child. |

`hasSibling` reads the parent state's query — a different mechanism from `localEnv`, which is local to the node and not visible to siblings.

### Writer combinators

These return `Elab[Unit]` (except where noted) and modify `ElabState`.

| Combinator | Effect |
| --- | --- |
| `Elab.transformChild(f: Query => Elab[Query])` | Captures `f` in `childTransform`; applied to the *already-elaborated* child in `SelectElaborator.transform`. This is how field arguments become algebra. |
| `Elab.transformChild(f: Query => Query)` | Pure overload (resolved via a `DummyImplicit`). |
| `Elab.transformChild(f: Query => Result[Query])` | `Result`-returning overload. |
| `Elab.env(nme: String, value: Any)` | Bind one name/value in the node's local env. |
| `Elab.env(kv, kvs*)` / `Elab.env(kvs: Seq[(String, Any)])` | Bind several name/value pairs. |
| `Elab.env(other: Env)` | Merge an entire `Env` into the local env. |
| `Elab.addAttribute(name: String, query: Query = Empty)` | Record a synthetic field merged into the elaborated `Select`; the client never asked for it but a `CursorField` can read it. |
| `Elab.push` / `Elab.push(context, query)` / `Elab.push(schema, context, query)` | Save the current state and descend into a child (resetting local env/attributes/childTransform). |
| `Elab.pop` | Restore the parent state. |

`transformChild` calls **compose** rather than replace: `ElabState.addChildTransform` chains each via `childTransform.andThen(_.flatMap(f))`, so two cases that both call `transformChild` on the same node both run, in order. `Elab.env` writes only to the *current* node's local env — it is materialised as an `Environment(env, select)` wrapper and is visible to that node's children/cursor, **not** to siblings.

### Lifting & error reporting

| Combinator | Type | Effect |
| --- | --- | --- |
| `Elab.unit` | `Elab[Unit]` | Pure `()` — the no-op an elaborator returns to leave a field's arguments un-eliminated. |
| `Elab.pure[T](t: T)` | `Elab[T]` | Lift a pure value. |
| `Elab.liftR[T](rt: Result[T])` | `Elab[T]` | Lift a `Result` into `Elab`, threading state unchanged. |
| `Elab.warning(msg: String)` / `Elab.warning(err: Problem)` | `Elab[Unit]` | Record a GraphQL warning; elaboration continues. Surfaces in the response `errors` array. |
| `Elab.failure[T](msg: String)` / `Elab.failure[T](err: Problem)` | `Elab[T]` | Record a GraphQL error (a [`Problem`](result-problem.md)); the field fails to compile. Surfaces in `errors`. |
| `Elab.internalError[T](msg: String)` / `Elab.internalError[T](err: Throwable)` | `Elab[T]` | Raise an internal error. This is **not** a GraphQL `Problem`; it propagates into the effect `F` and does **not** appear in the response `errors` array. |

Reach for `failure`/`warning` for malformed queries the client should see, and `internalError` only for impossible-state bugs.

## The `Phase` trait

A `Phase` rewrites a `Query`. The compiler folds a list of phases over the parsed query (see [the pipeline](#the-phase-pipeline) below).

```scala
trait Phase {
  def transformFragments: Elab[Unit] = Elab.unit
  def transform(query: Query): Elab[Query]
  def transformSelect(fieldName: String, alias: Option[String], child: Query): Elab[Query]
  def validateSubselection(fieldName: String, child: Query): Elab[Unit]
}
```

| Member | Default behaviour |
| --- | --- |
| `transformFragments` | No-op. Override to rewrite fragment definitions before the body (e.g. `IntrospectionElaborator`). |
| `transform(query)` | The recursive walk. It pattern-matches every `Query` node, `push`es the child context, recurses, and `pop`s — for `Unique`/`Filter`/`Count`/`Narrow`/fragments/`Group` etc. it descends with the correctly-narrowed `Context`. Override the cases you care about and call `super.transform(query)` for the rest. |
| `transformSelect(fieldName, alias, child)` | Validates the subselection, computes the child `Context` via `c.forField(fieldName, alias)`, then recurses into `child`. Used by the default `transform` for `Select`/`UntypedSelect`. |
| `validateSubselection(fieldName, child)` | Enforces leaf rules (see below). |

`validateSubselection` rejects mismatched selection sets: a leaf field with a non-empty `{ ... }` fails with `"Leaf field 'x' of T must have an empty subselection set"`, and a non-leaf field with an empty selection fails with `"Non-leaf field 'x' of T must have a non-empty subselection set"`.

When you subclass `Phase` directly, override only the node cases you handle. The standard idiom matches `UntypedSelect` (or `Select`), does your work, and delegates everything else to `super.transform`, so the base traversal keeps the `Context` correct as it descends.

## `SelectElaborator`

`SelectElaborator` is the phase you supply on a `Mapping` to give field arguments meaning. It extends `Phase`, overriding `transform` to fire on `UntypedSelect` nodes; everything else falls through to `super.transform`.

```scala
trait SelectElaborator extends Phase {
  def select(ref: TypeRef, name: String, args: List[Binding], directives: List[Directive]): Elab[Unit]
  def elaborateFieldArgs(tpe: NamedType, field: Field, args: List[Binding]): Result[List[Binding]]
}

object SelectElaborator {
  def apply(sel: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]): SelectElaborator
  def identity: SelectElaborator
}
```

For each `UntypedSelect`, `SelectElaborator.transform`:

1. Resolves the field on the schema and runs `elaborateFieldArgs`, which **types and normalises** the arguments: validates/types enums, coerces `String`/`Int` to `ID` where the schema says so, fills defaults for missing arguments, and **permutes arguments into schema-declared order**. Unknown argument names fail with `"Unknown argument(s) 'quux' in field character of type Query"`.
2. Calls `select(ref, name, eArgs, dirs)` (or, for introspection fields, `elaborateIntrospection`) to consume the normalised arguments.
3. Recurses into the child, then applies the accumulated `childTransform` to it, wraps the result in `Select(name, alias, child')`, merges any `addAttribute` selects, and — if `localEnv` is non-empty — wraps the whole thing in an `Environment` node.

Because args are normalised first, `select` cases match the **canonical** order, not source order: `List(Binding("offset", IntValue(o)), Binding("limit", IntValue(l)))` assumes schema order. A case that does not match leaves the field's arguments un-eliminated. Optional arguments arrive as `Value.AbsentValue` (distinct from `Value.NullValue`); match `Binding("namePattern", AbsentValue)` separately or the elaborator silently no-ops.

| Member | Meaning |
| --- | --- |
| `SelectElaborator(sel)` | Factory: builds a `SelectElaborator` whose `select` runs `sel` when `sel.isDefinedAt((ref, name, args))`, else `Elab.unit`. The common path. |
| `SelectElaborator.identity` | A `SelectElaborator` that discards all arguments (`select` always returns `Elab.unit`). |
| `select(ref, name, args, directives)` | The low-level hook. Override directly (instead of the factory) when you need the field's `directives`, or to wrap `transform`. |
| `elaborateFieldArgs(tpe, field, args)` | The argument typing/normalisation step. Returns `Result[List[Binding]]`; rarely overridden. |

### The factory form

The canonical elaborator is a `PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]` matched on the field's type ref, name, and normalised bindings, returning `Elab.transformChild(...)` to rewrite the child into interpretable algebra. The following matches `character(id: ...)` and rewrites its selection set into a `Unique` over a `Filter`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/CompilerSuite.scala", "#atomic_elaborator"))
```

`SelectElaborator { case (QueryType, "character", List(Binding("id", StringValue(id)))) => ... }` matches that one field. `Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(id)), child)))` takes the elaborated selection set (`child`) and wraps it: `Filter` keeps only `Character`s whose `id` equals the argument, and `Unique` asserts the result is a single element. After compilation the `UntypedSelect` with an `id` argument has become a directly-interpretable `Select(Unique(Filter(...)))` tree. The `Eql`/`Const`/`/` building blocks are [predicates and terms](predicates.md); for the full repertoire of `Filter`/`Limit`/`Offset`/`OrderBy`/`Count` recipes see [filtering, ordering & paging](../how-to/filtering-ordering-paging.md).

### The low-level form

To inspect a field's directives, or to run extra work around the standard `transform`, extend `SelectElaborator` directly: implement `select(ref, name, args, directives)` and override `transform`. The test helper below stashes each field's raw `args`/`directives` in the env via `select`, then in `transform` re-reads them with `Elab.envE` and substitutes them back, preserving the original arguments on the elaborated tree:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/PreserveArgsElaborator.scala", "#preserve_args"))
```

Note `select` returns `Elab.env("preserved", Preserved(args, directives))` — storing the arguments rather than eliminating them — and `transform` calls `super.transform(query)` to run the normal elaboration before `Elab.envE[Preserved]("preserved")` retrieves them (failing if absent). This is the pattern for a phase that needs both the field arguments *and* the elaborated subtree. For directive-driven phases written as a plain `Phase` (not a `SelectElaborator`), see [query directives](../how-to/query-directives.md).

### `IntrospectionLevel`

`SelectElaborator` also elaborates introspection fields, gated by the `introspectionLevel` passed to `QueryCompiler.compile`.

```scala
sealed trait IntrospectionLevel
object IntrospectionLevel {
  case object Full extends IntrospectionLevel        // __schema, __type, __typename allowed
  case object TypenameOnly extends IntrospectionLevel // only __typename allowed
  case object Disabled extends IntrospectionLevel      // no introspection
}
```

`IntrospectionElaborator(level: IntrospectionLevel)` returns `Option[IntrospectionElaborator]` — `None` for `Disabled`. With `TypenameOnly` or `Disabled`, a `__schema`/`__type` query fails with `"Introspection is disabled"`.

## Built-in phases

The compiler always runs three built-in phases ahead of the mapping's phases; a `Mapping` adds three more by default.

### The phase pipeline

`compileOperation` assembles the phase list as:

```text
IntrospectionElaborator(level)?  ::  VariablesSkipAndFragmentElaborator  ::  MergeFields  ::  <mapping.compilerPhases>
```

where `IntrospectionElaborator` is omitted when introspection is `Disabled`, and `mapping.compilerPhases` defaults to:

```scala
List(selectElaborator, componentElaborator, effectElaborator)
```

Each phase runs `phase.transformFragments *> phase.transform(acc)`, folded left over the query. Order matters: variables are substituted, `@skip`/`@include` resolved, and fragments expanded *before* your `SelectElaborator` sees a clean, typed `Select` tree — it never meets a raw fragment spread or variable reference. Equally, `SelectElaborator` (which fires on `UntypedSelect`) must run before `ComponentElaborator`/`EffectElaborator` (which fire on already-typed `Select`), which is why it leads the default `compilerPhases`.

### Phase reference

| Phase | Constructor | Fires on | What it does |
| --- | --- | --- | --- |
| `IntrospectionElaborator` | `IntrospectionElaborator(level): Option[IntrospectionElaborator]` | `UntypedSelect("__typename" \| "__schema" \| "__type")` | Wraps introspection fields in an `Introspect(schema, ...)` node, or fails per `IntrospectionLevel`. Also elaborates fragments via `transformFragments`. |
| `VariablesSkipAndFragmentElaborator` | `object` (no args) | every node | Substitutes variable values into `Binding`s, drops `@skip`/`@include`-guarded subtrees, expands fragment spreads and inline fragments (introducing `Narrow` for type conditions), and re-runs `validateSubselection`. |
| `MergeFields` | `object` (no args) | the whole query | Merges duplicate sibling selections via `mergeUntypedQueries`, keying on `(name, alias)` and recursively merging children. |
| `SelectElaborator` | `SelectElaborator(pf)` / `.identity` | `UntypedSelect` | Types/normalises arguments and rewrites `UntypedSelect` into interpretable `Select`/`Filter`/`Unique`/… (above). |
| `ComponentElaborator` | `ComponentElaborator(mappings: Seq[ComponentMapping[F]])` | `Select` | At each `(type, field)` boundary registered in `mappings`, replaces the `Select` with a `Component(mapping, join, …)` node delegating that subtree to another mapping. See [composition](../concepts/composition.md). |
| `EffectElaborator` | `EffectElaborator(effects: (Context, String) => Option[EffectHandler[F]])` | `Select` | Wraps a `Select` whose `(context, field)` has an effect handler in an `Effect(handler, …)` node. See [effects & batching](../concepts/effects-batching.md). |
| `QuerySizeValidator` | `new QuerySizeValidator(maxDepth: Int, maxWidth: Int)` | the whole query | Fails compilation if the query exceeds `maxDepth` levels or `maxWidth` leaves. **Not** on by default — add it to `compilerPhases` yourself (e.g. `super.compilerPhases :+ querySizeValidator`). |

### `ComponentElaborator` companion

```scala
object ComponentElaborator {
  val TrivialJoin = (q: Query, _: Cursor) => q.success
  case class ComponentMapping[F[_]](
    tpe: TypeRef, fieldName: String, mapping: Mapping[F],
    join: (Query, Cursor) => Result[Query] = TrivialJoin)
  def apply[F[_]](mappings: Seq[ComponentMapping[F]]): ComponentElaborator[F]
}
```

`join` computes the continuation query for the delegated mapping from the current `Cursor`; `TrivialJoin` passes the subquery through unchanged.

### `EffectElaborator` companion

```scala
object EffectElaborator {
  case class EffectMapping[F[_]](tpe: TypeRef, fieldName: String, handler: EffectHandler[F])
  def apply[F[_]](effects: (Context, String) => Option[EffectHandler[F]]): EffectElaborator[F]
}
```

The `EffectHandler[F]` an `Effect` node carries has the shape `def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]]`, run by the [interpreter](../concepts/query-interpreter.md) in a later phase; see [effects & batching](effects.md).

## See also

- [The query algebra reference](query-algebra.md) — the `Query` ADT (`Select`, `Filter`, `Unique`, `Limit`, `Count`, …) these phases produce, plus structural helpers like `FilterOrderByOffsetLimit`.
- [Compiler & elaboration (concept)](../concepts/compiler-elaboration.md) — the narrative of how a query string becomes an `Operation`.
- [Filter, sort and page a field (how-to)](../how-to/filtering-ordering-paging.md) — task-oriented recipes for writing a `SelectElaborator` that turns arguments into `Filter`/`OrderBy`/`Limit`/`Offset`/`Count`.
- [Predicates, terms & filtering](predicates.md) — the `Predicate`/`Term` vocabulary used inside `Filter`/`OrderBy`.
- [Context & Env reference](context-env.md) — the `Context` and `Env` types threaded through `ElabState`.
- [Result & Problem reference](result-problem.md) — the `Result` effect and `Problem` values `Elab.failure`/`warning` produce.
