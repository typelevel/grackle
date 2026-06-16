# Query algebra reference

This page enumerates the `Query` algebra: the sealed ADT in `query.scala` that Grackle uses to represent a GraphQL operation as a tree. It covers every node — both the directly *interpretable* nodes the [query interpreter](../concepts/query-interpreter.md) evaluates and the *pre-elaboration* precursors the [compiler](../concepts/compiler-elaboration.md) rewrites away — plus the structural helper functions and type aliases that operate on them. It is for developers who read, build, or rewrite query-algebra terms (for example, inside a `SelectElaborator`). Unless noted otherwise, every member below lives in `object Query` in package `grackle`; bring them into scope with `import grackle.Query._`.

## The `Query` trait

`Query` is a sealed trait. Two members are defined on every node:

| Member | Signature | Meaning |
| --- | --- | --- |
| `~` | `def ~(query: Query): Query` | Group this query with another. Adjacent `Group`s are flattened, so `a ~ b ~ c` yields a single `Group(List(a, b, c))` rather than nested groups. |
| `render` | `def render: String` | A human-readable debug rendering of the node (used in error messages and tests), not GraphQL or JSON. |

Because the trait is sealed, the node lists below are exhaustive.

## Interpretable nodes

These are the nodes that survive compilation and are evaluated by the interpreter against a [`Cursor`](cursor.md). After a successful `QueryCompiler.compile`, an `Operation.query` is built only from these.

| Node | Signature | Semantics |
| --- | --- | --- |
| `Select` | `case class Select(name: String, alias: Option[String], child: Query)` | A field selection. `resultName` returns `alias.getOrElse(name)`. Leaf fields have `child == Empty`. |
| `Group` | `case class Group(queries: List[Query])` | A list of sibling selections at the same level. |
| `Unique` | `case class Unique(child: Query)` | Asserts `child` produces a single-element list and yields that element (e.g. the body of `character(id: …)`). |
| `Filter` | `case class Filter(pred: Predicate, child: Query)` | Retains only the elements of `child` that satisfy `pred`. See [predicates](predicates.md). |
| `Limit` | `case class Limit(num: Int, child: Query)` | Takes the first `num` elements of a list-producing `child`. |
| `Offset` | `case class Offset(num: Int, child: Query)` | Drops the first `num` elements of a list-producing `child`. |
| `OrderBy` | `case class OrderBy(selections: OrderSelections, child: Query)` | Orders a list-producing `child` by `selections`. |
| `Count` | `case class Count(child: Query)` | Computes the number of top-level elements of `child`. It *replaces* the node it is built from rather than wrapping a selection. |
| `Narrow` | `case class Narrow(subtpe: TypeRef, child: Query)` | Yields `child` when the focus is of type `subtpe`, `Empty` otherwise. Produced when expanding fragments with a type condition. |
| `Component` | `case class Component[F[_]](mapping: Mapping[F], join: (Query, Cursor) => Result[Query], child: Query)` | Marks a boundary where another mapping takes over; `join` computes the continuation query from the current cursor. Inserted by `ComponentElaborator`. |
| `Effect` | `case class Effect[F[_]](handler: EffectHandler[F], child: Query)` | Embeds (possibly batched) effects to be run by the interpreter in a later phase. See [effects and batching](../how-to/effects-batching.md). |
| `Environment` | `case class Environment(env: Env, child: Query)` | Adds `env` bindings to the runtime environment for `child`. Materialised from elaboration-time `Elab.env`. See [context and env](context-env.md). |
| `TransformCursor` | `case class TransformCursor(f: Cursor => Result[Cursor], child: Query)` | Computes a continuation cursor from the current cursor before evaluating `child`. |
| `Introspect` | `case class Introspect(schema: Schema, child: Query)` | Evaluates an introspection subquery relative to `schema`. Inserted by `IntrospectionElaborator` around `__schema` / `__type` / `__typename`. |
| `Empty` | `case object Empty` | The terminal query / empty selection set. Leaf-field `Select`s have `child == Empty`. |

### `Select` companion overloads

The `Select` companion provides three convenience constructors that default `alias` to `None` and `child` to `Empty`:

| Constructor | Expands to |
| --- | --- |
| `Select(name)` | `Select(name, None, Empty)` |
| `Select(name, alias)` (with `alias: Option[String]`) | `Select(name, alias, Empty)` |
| `Select(name, child)` (with `child: Query`) | `Select(name, None, child)` |

### `Effect` handler

`Effect` carries an `EffectHandler`, the interface the interpreter calls to run the embedded effect:

```scala
trait EffectHandler[F[_]] {
  def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]]
}
```

The `List` argument lets a handler receive several `(Query, Cursor)` pairs at once, which is how batching is expressed.

### Ordering: `OrderSelections` and `OrderSelection`

`OrderBy` holds an `OrderSelections`, a wrapper over a list of per-term `OrderSelection`s applied in order:

| Type | Signature | Notes |
| --- | --- | --- |
| `OrderSelections` | `case class OrderSelections(selections: List[OrderSelection[_]])` | Compares cursors by each `OrderSelection` left-to-right; the first non-equal comparison wins. |
| `OrderSelection` | `case class OrderSelection[T: Order](term: Term[T], ascending: Boolean = true, nullsLast: Boolean = true)` | Orders by a [`Term`](predicates.md) `T`; needs a cats `Order[T]`. `ascending` and `nullsLast` both default to `true`. |

## Pre-elaboration nodes

These nodes exist only between parsing and elaboration. After parsing, the tree is built from them (plus the `Group`/`Empty` nodes above); the compiler's phases progressively rewrite them into the interpretable nodes. They are not directly interpretable.

| Node | Signature | Role |
| --- | --- | --- |
| `UntypedSelect` | `case class UntypedSelect(name: String, alias: Option[String], args: List[Binding], directives: List[Directive], child: Query)` | A field selection carrying raw argument `Binding`s and directives. Rewritten to `Select` by `SelectElaborator`. |
| `UntypedFragmentSpread` | `case class UntypedFragmentSpread(name: String, directives: List[Directive])` | A named fragment spread. Replaced by the fragment's body, guarded by a `Narrow` for its type condition. |
| `UntypedInlineFragment` | `case class UntypedInlineFragment(tpnme: Option[String], directives: List[Directive], child: Query)` | An inline fragment. Replaced by its `child`, guarded by a `Narrow` if a type condition (`tpnme`) is present. |
| `UntypedFragment` | `case class UntypedFragment(name: String, tpnme: String, directives: List[Directive], child: Query)` | A parsed fragment *definition* (the `fragment X on T { … }` declaration), kept in a fragment map and spliced in where spread. |
| `UntypedVarDef` | `case class UntypedVarDef(name: String, tpe: Ast.Type, default: Option[Value], directives: List[Directive])` | A parsed variable *definition*, before its type is resolved against the schema. |
| `Binding` | `case class Binding(name: String, value: Value)` | A single field or directive argument: a name plus a `Value`. The unit a `SelectElaborator` pattern-matches on. `render` yields `"name: value"`. |

### The `Value` ADT

`Binding.value` is a `Value` (defined in `object Value`, package `grackle`; `import grackle.Value._`). These are the argument values a `SelectElaborator` matches on — note that GraphQL ID and String literals are distinct cases, and that an omitted optional argument arrives as `AbsentValue`, not `NullValue`.

| Case | Signature |
| --- | --- |
| `IntValue` | `case class IntValue(value: Int)` |
| `FloatValue` | `case class FloatValue(value: Double)` |
| `StringValue` | `case class StringValue(value: String)` |
| `BooleanValue` | `case class BooleanValue(value: Boolean)` |
| `IDValue` | `case class IDValue(value: String)` |
| `EnumValue` | `case class EnumValue(name: String)` |
| `ListValue` | `case class ListValue(elems: List[Value])` |
| `ObjectValue` | `case class ObjectValue(fields: List[(String, Value)])` |
| `VariableRef` | `case class VariableRef(name: String)` |
| `NullValue` | `case object NullValue` |
| `AbsentValue` | `case object AbsentValue` |

`VariableRef` nodes are resolved to concrete `Value`s during compilation, so a `SelectElaborator` never sees one. `AbsentValue` lets an elaborator distinguish a missing optional argument from an explicit `null` (`NullValue`) — match it as a separate case, e.g. `Binding("namePattern", AbsentValue)`.

## Example: parsing into the algebra

Parsing alone — with no schema — turns GraphQL text into the *untyped* algebra. This test from `CompilerSuite` parses a one-field query and asserts the exact tree it produces:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/CompilerSuite.scala", "#compile_simple"))
```

The result is an `UntypedOperation.UntypedQuery` whose `.query` is an `UntypedSelect("character", …)` carrying the raw `List(Binding("id", StringValue("1000")))` argument, with the nested `UntypedSelect("name", …)` as its `child`. The leaf field `name` has `child == Empty`. No types or predicates appear yet — turning the `Binding` into an `Eql`/`Unique`/`Filter` tree is the [`SelectElaborator`'s job](../concepts/compiler-elaboration.md), which runs only once a schema and elaborator are present.

## Structural helpers

These functions (also in `object Query`) inspect and rewrite query trees. They are used throughout elaboration and are the supported way for a custom elaborator to reach into a subtree rather than pattern-matching the ADT by hand. Most of them "see through" the wrapper nodes `Environment` and `TransformCursor` to reach the underlying `Select`/`UntypedSelect`.

| Function | Signature | Result |
| --- | --- | --- |
| `ungroup` | `def ungroup(query: Query): List[Query]` | Flattens nested `Group`s into a flat list of top-level queries; a non-`Group` returns a singleton list. |
| `children` | `def children(q: Query): List[Query]` | The (ungrouped) child selections of a `Select`/`UntypedSelect`; `Nil` for other nodes. |
| `extractChild` | `def extractChild(query: Query): Option[Query]` | The single child selection of a top-level field, or `None`. |
| `substChild` | `def substChild(query: Query, newChild: Query): Option[Query]` | The query with its top-level field's child replaced by `newChild`, or `None`. |
| `hasField` | `def hasField(query: Query, fieldName: String): Boolean` | Whether `fieldName` is a top-level selection of `query`. |
| `fieldAlias` | `def fieldAlias(query: Query, fieldName: String): Option[String]` | The alias, if any, of the top-level field `fieldName`. |
| `mapFields` | `def mapFields(query: Query)(f: Query => Query): Query` | Applies `f` to each top-level `Select` (descending through `Group`/`Environment`/`TransformCursor`). |
| `mapFieldsR` | `def mapFieldsR(query: Query)(f: Query => Result[Query]): Result[Query]` | As `mapFields`, but `f` returns a [`Result`](result-problem.md); failures short-circuit. |
| `mergeQueries` | `def mergeQueries(qs: List[Query]): Query` | Merges *typed* (`Select`) queries, combining selections keyed by `(name, alias)` and recursively merging children. |
| `mergeUntypedQueries` | `def mergeUntypedQueries(qs: List[Query]): Query` | The same merge for *untyped* (`UntypedSelect`) queries; used by the `MergeFields` phase. |

`mergeQueries` and `mergeUntypedQueries` are not interchangeable: each only merges nodes of its own stage. Use the typed merger when assembling `Select`s in an elaborator (for example, when `Elab.addAttribute` folds a synthetic field into an already-elaborated selection set) and the untyped merger only on pre-elaboration trees.

### `FilterOrderByOffsetLimit`

A constructor/extractor that nests `Filter`, `OrderBy`, `Offset`, and `Limit` in canonical order around a child. It is the convenient way to build (or destructure) a paging selection.

```scala
object FilterOrderByOffsetLimit {
  def apply(
    pred:   Option[Predicate],
    oss:    Option[List[OrderSelection[_]]],
    offset: Option[Int],
    limit:  Option[Int],
    child:  Query): Query

  def unapply(q: Query): Option[
    (Option[Predicate], Option[List[OrderSelection[_]]], Option[Int], Option[Int], Query)]
}
```

`apply` wraps `child` from the inside out as `Limit(Offset(OrderBy(Filter(child))))`, omitting any layer whose argument is `None`. `unapply` peels those same four layers back off (in `Limit`, `Offset`, `OrderBy`, `Filter` order) and returns `None` only when *none* of the four are present. See [filtering, ordering and paging](../how-to/filtering-ordering-paging.md) for how an elaborator uses it.

## Type aliases

Three aliases describe variable definitions across compilation stages:

| Alias | Definition | Stage |
| --- | --- | --- |
| `UntypedVarDefs` | `type UntypedVarDefs = List[UntypedVarDef]` | Parsed variable definitions, before their types are resolved. |
| `VarDefs` | `type VarDefs = List[InputValue]` | Variable definitions after their declared types are resolved against the schema. |
| `Vars` | `type Vars = Map[String, (Type, Value)]` | The fully resolved `(type, value)` map of an operation's variables, available to an elaborator via `Elab.vars`. |

## See also

- [The compiler and elaboration](../concepts/compiler-elaboration.md) — how a query string becomes a tree of these nodes.
- [Compiler phases and the Elab monad](elab-phases.md) — the phase pipeline and the `Elab` combinators (`transformChild`, `env`, `addAttribute`) that rewrite this algebra.
- [Predicates and terms](predicates.md) — the `Predicate`/`Term` types carried by `Filter` and `OrderSelection`.
- [Cursor](cursor.md) and the [query interpreter](../concepts/query-interpreter.md) — how the interpretable nodes are evaluated.
- [Filtering, ordering and paging](../how-to/filtering-ordering-paging.md) — building `Filter`/`OrderBy`/`Limit`/`Offset` from field arguments.
