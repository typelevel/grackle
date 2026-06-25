# Cursor reference

A `Cursor` is Grackle's read-only navigator over an abstract backing data model during query interpretation. It pairs an untyped runtime `focus` value with the GraphQL `Type` it is expected to represent, and exposes type-directed navigation — descend into a field, render a leaf as JSON, iterate a list, narrow an interface — with every method returning a [`Result`](result-problem.md) so failures accumulate rather than throw. This page lists the `Cursor` trait and its methods, the provided base and proxy cursors, and what you must implement to write a custom one. For the `Context`/`Env` types a cursor carries, see the [Context & Env reference](context-env.md); for how the interpreter walks a `Cursor`, see [How the query interpreter works](../concepts/query-interpreter.md).

## The `Cursor` trait

A `Cursor` (`modules/core/src/main/scala/cursor.scala`) holds four pieces of state and derives a position from its `Context`:

| Member | Signature | Meaning |
| --- | --- | --- |
| `parent` | `def parent: Option[Cursor]` | The enclosing cursor, or `None` at the root. |
| `focus` | `def focus: Any` | The backing value at this position, untyped. |
| `context` | `def context: Context` | Schema path, alias-applied result path, and GraphQL type stack. |
| `path` | `def path: List[String] = context.path` | Schema field names from the root (innermost-first). |
| `resultPath` | `def resultPath: List[String] = context.resultPath` | `path` with query aliases applied. |
| `tpe` | `def tpe: Type = context.tpe` | The GraphQL `Type` the `focus` is expected to have. |
| `env` | `private[grackle] def env: Env` | The local environment bindings at this cursor. |
| `withEnv` | `def withEnv(env: Env): Cursor` | A copy with additional environment values. |

`focus` is typed `Any`, and the value and `tpe` must agree: navigation methods pattern-match on the two together. A `ListType` `tpe` carrying a non-list `focus`, for example, reports `isList == false` and errors from `asList`. Keep the backing value in sync with the `Context` type.

## Type predicates and accessors

Each "is" predicate tests the shape of the `focus`; each "as" accessor produces the corresponding value, returning an error `Result` if the focus is not of that shape. Match the accessor to the cursor's actual type — the predicate tells you which one is valid.

| Method | Signature | Yields |
| --- | --- | --- |
| `isLeaf` / `asLeaf` | `def isLeaf: Boolean` · `def asLeaf: Result[Json]` | Whether the focus is scalar/enum, and its rendering as circe `Json`. `asLeaf` is the terminal step for scalar fields. |
| `isList` / `asList` | `def isList: Boolean` · `final def asList: Result[List[Cursor]] = asList(List)` · `def asList[C](factory: Factory[Cursor, C]): Result[C]` · `def listSize: Result[Int]` | Whether the focus is a list, one child `Cursor` per element (collected into any `Factory`), and its length. |
| `isNullable` / `asNullable` | `def isNullable: Boolean` · `def asNullable: Result[Option[Cursor]]` · `def isDefined: Result[Boolean]` · `def isNull: Boolean` | Whether the focus is nullable; `asNullable` yields `Some(child)` when present, `None` when absent. |

`asNullable` has two layers: a present value is `Result.Success(Some(c))`, an absent value is `Result.Success(None)`, and a type mismatch is an internal error. The interpreter maps `None` to `Json.Null`. `isNull` is derived as `isNullable && asNullable == Success(None)`.

## Field navigation

| Method | Signature | Behaviour |
| --- | --- | --- |
| `field` | `def field(fieldName: String, resultName: Option[String]): Result[Cursor]` | Descend into a named field, optionally under a query alias (`resultName`). Concrete cursors delegate to `mkCursorForField`. |
| `fieldAs` | `def fieldAs[T: ClassTag: TypeName](fieldName: String): Result[T]` | `field(fieldName, None)` followed by `as[T]`. |
| `nullableField` | `def nullableField(fieldName: String): Result[Cursor]` | Transparently unwraps a nullable focus before descending into `fieldName`. |
| `path` | `def path(fns: List[String]): Result[Cursor]` | Follow a multi-field path, unwrapping nullables along the way, to a single cursor. |
| `listPath` | `def listPath(fns: List[String]): Result[List[Cursor]]` | Follow a path, fanning out across any lists or nullables encountered. |
| `flatListPath` | `def flatListPath(fns: List[String]): Result[List[Cursor]]` | Like `listPath`, but also flattens a list at the end of the path. |

`path`, `listPath`, and `flatListPath` are how predicates and joins reach nested attribute values; see the [Predicates & terms reference](predicates.md).

## Narrowing

Interface and union members are reached by narrowing to a concrete subtype before its subtype-only fields become accessible.

| Method | Signature | Behaviour |
| --- | --- | --- |
| `narrowsTo` | `def narrowsTo(subtpe: TypeRef): Result[Boolean]` | Tests run-time membership of `subtpe`. |
| `narrow` | `def narrow(subtpe: TypeRef): Result[Cursor]` | Re-types the cursor to `subtpe`, erroring if the focus is not narrowable. |

`narrow` backs `Narrow` query nodes and `__typename` resolution.

## Unique support

| Method | Signature | Behaviour |
| --- | --- | --- |
| `preunique` | `def preunique: Result[Cursor]` | Re-types the focus as a list (`tpe.nonNull.list`) so the caller can run it as a single-element list. |

`preunique` does **not** return the unique element. It is the antecedent of a `Unique` operation: it wraps the focus so the interpreter can then run the list with `unique = true`, which errors on "No match" (size 0, non-nullable) or "Multiple matches" (size > 1) and otherwise yields the one element. See [Filtering & paging query nodes](filtering-paging-nodes.md) for `Unique`.

## Casting and environment

| Method | Signature | Behaviour |
| --- | --- | --- |
| `as` | `def as[T: ClassTag: TypeName]: Result[T]` | Cast the focus to a concrete Scala type, with a descriptive error if the runtime class does not match. |
| `env` | `def env[T: ClassTag](nme: String): Option[T]` | Look up an environment key locally, then walk parents. |
| `envR` | `def envR[T: ClassTag: TypeName](nme: String): Result[T]` | Like `env`, but a missing key is a `Result` error rather than `None`. |
| `fullEnv` | `def fullEnv: Env` | The cumulative environment from the root down to this cursor. |
| `withEnv` | `def withEnv(env: Env): Cursor` | A copy with extra environment bindings. |

`env` lookups are type-checked at run time via `ClassTag`, so a key present but stored as the wrong type returns `None`; use `envR` for a descriptive error. The `Env` type itself is documented in the [Context & Env reference](context-env.md).

### Companion helpers

```scala
object Cursor {
  def flatten(c: Cursor): Result[List[Cursor]]
  def flatten(cs: List[Cursor]): Result[List[Cursor]]
}
```

`Cursor.flatten` recursively expands lists and nullables into a flat `List[Cursor]` of leaf positions.

## Worked navigation

The following examples build a real `Cursor` by hand with `CursorBuilder[T].build(context, value)` — the most direct way to obtain one outside the interpreter — and exercise the accessors above against Grackle's generic Star Wars model. They are taken verbatim from `DerivationSuite`. For `CursorBuilder` itself, see [generic derivation](../how-to/generic-derivation.md).

A leaf cursor is rooted at a scalar `Context`; `isLeaf` is true and `asLeaf` renders the focus as `Json`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_primitive"))
```

`field` descends by GraphQL field name (`None` means no alias). Because `name` is nullable in the schema, the child cursor is nullable, so `asNullable` is unwrapped first: its `Result[Option[Cursor]]` is flattened with `toResultOrError("missing")`, which turns the absent (`None`) case into an error, before `asLeaf` renders the present value:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_product"))
```

For a list field, `asNullable` unwraps the nullable wrapper and `asList(List)` yields one child `Cursor` per element, each traversed with `asLeaf`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_list"))
```

`appearsIn` is declared `[Episode!]` on the `Character` interface — a nullable list — so the snip unwraps with `asNullable` before `asList`, and each element is itself a leaf enum cursor. `narrow` re-types an interface cursor to a concrete subtype; `homePlanet` exists only on `Human`, so the cursor must be narrowed from `Character` first:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_narrow"))
```

## Provided base and proxy cursors

`Cursor` is a wide trait, so two base classes in `object Cursor` supply defaults, and several concrete cursors cover common positions.

| Type | Signature | Purpose |
| --- | --- | --- |
| `AbstractCursor` | `abstract class AbstractCursor extends Cursor` | Gives every navigation method an error default (`isLeaf = false`, `asLeaf`/`asList`/`asNullable`/`preunique`/`narrow`/`field` all `Result.internalError`, `narrowsTo = false`). A concrete cursor overrides only the cases its model supports. |
| `ProxyCursor` | `class ProxyCursor(underlying: Cursor) extends Cursor` | Delegates every method to `underlying`; the base for cursors that wrap another and override a few methods. |
| `EmptyCursor` | `case class EmptyCursor(context: Context, parent: Option[Cursor], env: Env) extends AbstractCursor` | A contentless position carrying only `context`/`env`; accessing `focus` errors. |
| `ListTransformCursor` | `case class ListTransformCursor(underlying: Cursor, newSize: Int, newElems: Seq[Cursor]) extends ProxyCursor(underlying)` | A list proxy that substitutes an alternative element set/size — the result of a `TransformCursor` over a list (filter/order/limit re-projection). |
| `NullCursor` | `case class NullCursor(underlying: Cursor) extends ProxyCursor(underlying)` | Always reports `isDefined = false` and `asNullable = None`, forcing a null. |
| `NullFieldCursor` | `case class NullFieldCursor(underlying: Cursor) extends ProxyCursor(underlying)` | Wraps every child field result in a `NullCursor`. |
| `DeferredCursor` | `case class DeferredCursor(context: Context, parent: Option[Cursor], env: Env, deferredPath: List[String], mkCursor: (Context, Cursor) => Result[Cursor]) extends AbstractCursor` | Defers construction of the real cursor until a fixed `deferredPath` is traversed, invoking `mkCursor` at the final step. |

`DeferredCursor` also has a `Path`-based constructor, `DeferredCursor(path: Path, mkCursor: (Context, Cursor) => Result[Cursor]): Cursor`. Its `field` errors unless `fieldName` equals the next element of `deferredPath` — it can only walk the pre-declared path, not arbitrary fields.

## Implementing a custom Cursor

To navigate your own backing model, extend `AbstractCursor` and override the navigation methods your model supports — the unsupported ones keep their error defaults. The canonical reference is `ValueCursor` (`modules/core/src/main/scala/valuemapping.scala`), a complete `Cursor` over plain Scala values:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/main/scala/valuemapping.scala", "#value_cursor"))
```

Note the recurring shapes:

- **`mkChild` + `context.asType`.** Every child cursor re-types the `Context` to the child's GraphQL type (`context.asType(tpe)`) while passing the child `focus`. Keeping this re-typing correct is what makes the interpreter's `cursorCompatible` guard pass; a `tpe` that doesn't match the query type surfaces as a "Mismatched query and cursor type" internal error rather than a useful message.
- **Match on `(tpe, focus)` together.** `isList`, `asList`, `asNullable`, `isDefined`, and `preunique` all require both the GraphQL type *and* the runtime value to agree (`(_: ListType, _: List[_])`, `(NullableType(tpe), o: Option[_])`, and so on).
- **`narrowsTo`/`narrow` consult the mapping.** `ValueCursor.narrowsTo` checks both that `subtpe <:< tpe` and that the mapped runtime class matches the focus; `narrow` then re-types via `mkChild(context.asType(subtpe))`.
- **`field` bridges into the mapping.** `ValueCursor.field` simply calls `mkCursorForField(this, fieldName, resultName)`, handing field resolution back to the [`Mapping`](mapping-types.md).

For a cursor over a structured document rather than native values, `CirceCursor` (`modules/circe/src/main/scala/circemapping.scala`) is the other reference implementation: its `asLeaf` validates each scalar/enum against the JSON shape and its `narrowsTo` checks that required fields are present. See [serving GraphQL from circe JSON](../how-to/circe-backend.md).

The interpreter obtains its starting cursor from the mapping's `RootCursor` (focus is `Unit`), whose `field` delegates to `mkCursorForField` to enter the real model; see [How the query interpreter works](../concepts/query-interpreter.md) for the full root-to-JSON walk.

## See also

- [Context & Env reference](context-env.md) — the `Context` and `Env` types a `Cursor` carries.
- [Mapping types reference](mapping-types.md) — `mkCursorForField` and the mappings that build cursors.
- [How the query interpreter works](../concepts/query-interpreter.md) — how `runValue`/`runFields`/`runList` walk a `Cursor`.
- [Mappings and cursors](../concepts/mappings-cursors.md) — the concept behind tying a schema to data.
- [Result, Problem & ResultT reference](result-problem.md) — the four-armed `Result` every cursor method returns.
