# The compiler and elaboration

This page explains how Grackle turns a GraphQL query string into an executable query — the `QueryCompiler` and the *elaboration* phases it runs. It is aimed at developers who write `SelectElaborator`s or custom `Phase`s and want to understand what the compiler does before and after their code runs, why phase order matters, and how elaboration-time decisions reach the interpreter at runtime. For the exhaustive list of `Query` nodes see the [query algebra reference](../reference/query-algebra.md); for the `Elab` combinator surface see the [Elab monad & phases reference](../reference/elab-phases.md).

## The three stages: parse, validate, elaborate

Grackle is a compiler. A query string travels through three stages before anything is executed:

1. **Parse.** `QueryParser` turns GraphQL text into a list of `UntypedOperation`s plus the document's fragments. The result is *untyped algebra*: a tree of `UntypedSelect` nodes carrying raw arguments (`Binding`s) and directives, not yet checked against the schema.
2. **Validate.** Before any phase runs, the compiler validates variable and fragment usage (undefined, unused, cyclic) and the GraphQL field-mergeability rules. Failures here are accumulated as `Problem`s in the `Result`.
3. **Elaborate.** A sequence of `Phase`s is folded over the tree, rewriting the untyped algebra into the directly-interpretable `Query` algebra and producing a compiled `Operation`.

`QueryCompiler.compile` is the entry point and takes several parameters that tune these stages:

```scala
def compile(
    text: String,
    name: Option[String] = None,
    untypedVars: Option[Json] = None,
    introspectionLevel: IntrospectionLevel = Full,
    reportUnused: Boolean = true,
    env: Env = Env.empty): Result[Operation]
```

- `name` selects one operation when the document defines several named ones.
- `untypedVars` supplies the JSON variables to substitute.
- `introspectionLevel` (`Full` / `TypenameOnly` / `Disabled`) controls which `__schema`/`__type` fields are permitted.
- `reportUnused` (default `true`) decides whether unused variables and fragments are reported as warnings; set it to `false` for persisted or partial queries.
- `env` seeds an external [`Env`](../reference/context-env.md) that every `Cursor.env` lookup can read at runtime — useful for request-scoped context such as auth or feature flags.

Errors and warnings from every stage surface as `Problem`s in the returned `Result`. Remember the invariant: validation `Problem`s come back in the GraphQL `errors` array, but an `InternalError` is raised into the effect `F` instead — see [Result, Problem & ResultT](../reference/result-problem.md).

## From `UntypedSelect` to `Select`

The parser produces a tree of `UntypedSelect` nodes. Consider the simplest possible query:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/CompilerSuite.scala", "#compile_simple"))
```

Parsing alone needs no schema. `character(id: "1000") { name }` becomes an `UntypedSelect("character", …)` whose `args` list holds `Binding("id", StringValue("1000"))`, wrapping an inner `UntypedSelect("name", …)`. Leaf fields have `child == Empty`. Nothing here is executable yet — the `id` argument is just data hanging off the node.

Elaboration eliminates those arguments. Compiling the same shape against a mapping whose `SelectElaborator` knows about `character` produces the interpretable algebra:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/CompilerSuite.scala", "#compile_elaborated"))
```

The `UntypedSelect` with an `id` argument has become a `Select("character", …)` whose child is `Unique(Filter(Eql(CharacterType / "id", Const("1000")), …))`. The argument did not vanish — it was *consumed* and turned into algebra: a `Filter` on a `Predicate` that the interpreter can evaluate, wrapped in `Unique` to assert a single result. The nested `name`/`friends` selections are now plain `Select`s combined with the `~` group operator. This before/after pair captures what elaboration does: arguments are pushed down into nodes like `Filter`, `Unique`, `Limit` and `OrderBy` so the interpreter only ever sees a tree it can run.

The mechanism is `Elab.transformChild`. A `SelectElaborator` case returns `Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(id)), child)))`; the function is captured and applied to the *already-elaborated* child of the matching `Select`. Multiple `transformChild` calls on the same node compose with `andThen` rather than replacing one another, so two cases that both target a node both run, in order. Writing one of these is covered in [Filter, sort and page a field](../how-to/filtering-ordering-paging.md).

## The `Elab` monad

Every phase runs inside the `Elab` monad:

```scala
type Elab[T] = StateT[Result, ElabState, T]
```

`StateT` over `Result` means `Elab` threads an `ElabState` through the traversal *and* accumulates GraphQL errors and warnings. `ElabState` carries everything an elaborator can see — the `schema`, the current `Context` (your position in the schema, as a `TypeRef`), the resolved query `vars`, the `fragments` map, the node's local elaboration `Env`, the accumulated `attributes`, the pending `childTransform`, and a `parent` pointer used to navigate up and down the tree.

You rarely touch `ElabState` directly. Instead you compose the combinators the `Elab` object exposes:

- **Readers:** `Elab.context`, `Elab.schema`, `Elab.vars`, `Elab.fragments`, `Elab.hasField`, `Elab.hasSibling`, `Elab.resultName`.
- **Writers:** `Elab.transformChild` (rewrite the child), `Elab.env` (bind elaboration-time values), `Elab.addAttribute` (inject a synthetic field), `Elab.push`/`Elab.pop` (descend into a child context).
- **Errors:** `Elab.failure`, `Elab.warning`, `Elab.internalError`, `Elab.liftR`.

Elaboration descends into a child selection with `Elab.push(childContext, child)` — which saves the parent `ElabState` — and returns with `Elab.pop`. `Context.forField(fieldName, alias)` computes the child's type context, and `hasSibling` works by inspecting the saved parent state. This push/pop walk is exactly what lets an elaborator know its position in the schema and inspect neighbouring selections. The full combinator surface is documented in the [Elab monad & phases reference](../reference/elab-phases.md).

## Auto-prepended phases, and why your elaborator sees clean Selects

You configure a mapping's phases via `compilerPhases` (by default `selectElaborator :: componentElaborator :: effectElaborator`). But the compiler does not run that list as-is. `compileOperation` builds the real pipeline like this:

```text
allPhases =
  IntrospectionElaborator(level)?        // unless Disabled
  :: VariablesSkipAndFragmentElaborator  // substitute vars, apply @skip/@include, expand fragments
  :: MergeFields                         // apply GraphQL field-merge rules
  :: yourPhases                          // selectElaborator, componentElaborator, effectElaborator, …
```

and folds it left over the query, running `phase.transformFragments *> phase.transform(acc)` for each phase. The order is deliberate and matters:

- `VariablesSkipAndFragmentElaborator` runs **first**, so variable references are already substituted into `Binding` values, `@skip`/`@include`-guarded subtrees are dropped, and fragment spreads are expanded into ordinary selections.
- `MergeFields` then applies the field-merge rules, collapsing duplicate selections.
- Only then does your `SelectElaborator` run. By the time it sees the tree, it is dealing with a clean, variable-substituted, fragment-expanded, field-merged set of typed `Select` nodes — never a raw `VariableRef`, fragment spread, or `@skip` directive.

This is why a `SelectElaborator` case can pattern-match `(QueryType, "character", List(Binding("id", StringValue(id))))` and trust that the argument list is final and normalised. (Argument elaboration also permutes args into schema-declared order and fills defaults, so your patterns should assume canonical order, not source order; optional arguments arrive as `AbsentValue`, distinct from `NullValue`.)

Ordering within `yourPhases` matters too: `SelectElaborator.transform` only fires on `UntypedSelect`, whereas `ComponentElaborator` and `EffectElaborator` fire on the already-typed `Select`. So `selectElaborator` must run before the other two — the default order — or they will see `UntypedSelect` nodes and fall through. `ComponentElaborator` (for [composed mappings](composition.md)) and `EffectElaborator` (for [effects and batching](effects-batching.md)) are themselves phases; this page treats them as the built-in stages that turn cross-mapping and effectful boundaries into `Component` and `Effect` nodes.

## Elaboration-time `Env` becomes runtime `Cursor.env`

Not every argument becomes a `Filter`. Some fields are *computed* from their arguments by a `CursorField`, and elaboration's job is to carry the argument values from compile time to run time. The bridge is the [`Env`](../reference/context-env.md): an immutable, type-tagged string-to-value bag.

During elaboration, `Elab.env("x" -> x, "y" -> y)` writes into the current node's *local* env. If that local env is non-empty after the `Select` is elaborated, it is materialised as an `Environment(env, select)` node. At run time the interpreter installs those bindings, and a `CursorField` reads them back with `cursor.env[Int]("x")`. The following self-contained mapping wires both ends together:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/EnvironmentSuite.scala", "#env_mapping"))
```

Trace the `sum` field. The `SelectElaborator` case `(NestedType, "sum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y)))) => Elab.env("x" -> x, "y" -> y)` consumes the two arguments and stashes their values in the local env — it does *not* transform the child, because `sum` is a leaf with no selection set. On the other side, `def sum(c: Cursor): Result[Int]` reads `c.env[Int]("x")` and `c.env[Int]("y")` and adds them. The `Nested.sum` field is mapped to `CursorField("sum", sum)`, so running `query { nested { sum(x: 13, y: 23) } }` produces:

```json
{
  "data" : {
    "nested" : {
      "sum" : 36
    }
  }
}
```

The same mapping shows the *external* seeding path: `url` reads `c.env[Boolean]("secure")`, and that boolean is never an argument — it is supplied by passing `env = Env("secure" -> true)` to `compile` (and seeded into `ElabState`'s `localEnv` for the whole operation). This is the canonical way to thread request-scoped context such as auth or feature flags into field computations.

Two things to keep in mind. `Elab.env` writes only to the current node's local env, so the value is visible to that node's own children and cursor — not to its siblings; sibling inspection goes through the parent state via `hasSibling`, a different mechanism entirely. And `Env.get[T]` is filtered by runtime type tag, so a value stored under the wrong type returns `None`; use `Cursor.envR` / `Elab.envE` when you want a missing or mistyped lookup to become a descriptive `Result` failure instead of a silent `None`.

## `addAttribute`: synthetic computed fields

Sometimes a `CursorField` needs data the client never asked for — a count to compute a `hasMore` flag for pagination, say. `Elab.addAttribute(name, query)` records a synthetic field that is merged into the elaborated `Select`, even though it appears nowhere in the original query:

```scala
// inside a SelectElaborator case:
Elab.addAttribute("numCountries", Count(Select("items", Select("code"))))
```

Here a `Count` subquery is injected alongside the requested selections. The interpreter evaluates it and exposes the result for a `CursorField` to read, but it is invisible in the response because the client did not select it. This is how computed paging metadata is assembled by hand — Grackle has no built-in Relay `Connection` type, so `Limit`/`Offset`/`OrderBy`/`Count` and synthetic attributes are the raw material you compose. See [Filter, sort and page a field](../how-to/filtering-ordering-paging.md) for the full pattern.

## `UntypedOperation` vs `Operation`; root type resolution

The parser yields `UntypedOperation`s — `UntypedQuery`, `UntypedMutation`, or `UntypedSubscription` — each carrying an untyped `query`, its variable definitions, and directives. Compilation turns one of these into an `Operation(query, rootTpe, directives)`: a fully elaborated, directly-interpretable result.

Before running any phase, `compileOperation` resolves the root type. It calls `op.rootTpe(schema)`, which maps the operation kind to the schema's `Query`, `Mutation`, or `Subscription` root type, and seeds the elaboration state with `Context(rootTpe)`. That initial `Context` is the starting position from which the push/pop walk descends — every `(TypeRef, fieldName, args)` your `SelectElaborator` matches on is computed by stepping the context down from this root. If the schema declares no matching root type, compilation fails before any phase runs.

The output `Operation` is what you hand to the interpreter. From here, the [query interpreter](query-interpreter.md) walks the `Query` algebra against a [`Mapping` and its `Cursor`s](mappings-cursors.md), and for SQL backends the algebra is compiled further into SQL.

## See also

- [Architecture overview](architecture.md) — where the compiler sits in the end-to-end pipeline.
- [How the query interpreter works](query-interpreter.md) — what happens to the `Operation` after compilation.
- [Mappings and cursors](mappings-cursors.md) — how `CursorField`s read the `Env` you bind during elaboration.
- [Filter, sort and page a field](../how-to/filtering-ordering-paging.md) — write a `SelectElaborator` that emits `Filter`/`OrderBy`/`Limit`/`Count`.
- [Query algebra reference](../reference/query-algebra.md) — every `Query` node and structural helper.
- [Elab monad & compiler phases reference](../reference/elab-phases.md) — the full `Elab` combinator surface and built-in phases.
- [Context & Env reference](../reference/context-env.md) — the `Env` and `Context` types in detail.
