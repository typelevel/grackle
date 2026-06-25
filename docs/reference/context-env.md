# Context & Env reference

Every [`Cursor`](cursor.md) carries two immutable companions threaded through both elaboration and interpretation: a `Context`, which records *where* in the output tree the cursor sits, and an `Env`, a small type-checked key/value store for values computed during query processing. This page enumerates both types — the `Context` fields and their derivations, its `equals`/`hashCode` caveats, and the `Env` algebra with its type-checked lookups. Both are defined in `modules/core/src/main/scala/cursor.scala`, in package `grackle`; bring them into scope with `import grackle._`.

## `Context`

`Context` represents a position in the output tree in terms of three parallel paths plus the schema root. All four fields are public and immutable.

| Field | Type | Meaning |
| --- | --- | --- |
| `rootTpe` | `Type` | The GraphQL `Type` at the root of the operation (for example the `Query` type). |
| `path` | `List[String]` | The schema field names from the root to this position. |
| `resultPath` | `List[String]` | `path` with query aliases applied — the names that appear in the response JSON. |
| `typePath` | `List[Type]` | The GraphQL `Type` at each level of `path`. |

```scala
case class Context(
  rootTpe: Type,
  path: List[String],
  resultPath: List[String],
  typePath: List[Type]
)
```

### Reversed path ordering

`path`, `resultPath`, and `typePath` are stored **innermost-first**: the head is the deepest (current) element and the last entry is the field directly under the root. Each derivation prepends to the head, so descending into a field is a cons, not an append. The current type is therefore the head of `typePath`:

| Member | Signature | Yields |
| --- | --- | --- |
| `tpe` | `lazy val tpe: Type` | `typePath.headOption.getOrElse(rootTpe)` — the type at this position, falling back to `rootTpe` at the root. |
| `isRoot` | `def isRoot: Boolean` | `path.isEmpty` — whether this context is still at the operation root. |

Because the lists run innermost-first, the textual schema path reads in reverse: a context two fields deep has `path == List("inner", "outer")`.

### Derivations

Each derivation returns a child (or parent) context with the three paths extended or trimmed consistently. The `forField` variants are partial — they consult `tpe.underlyingField` and fail with a `Result` error if no such field exists on the current type.

| Method | Signature | Behaviour |
| --- | --- | --- |
| `forField` | `def forField(fieldName: String, resultName: String): Result[Context]` | Descend into `fieldName`, recording `resultName` in `resultPath`. Errors with `"No field '…' for type …"` if `fieldName` is not a field of `tpe`. |
| `forField` | `def forField(fieldName: String, resultName: Option[String]): Result[Context]` | As above; `None` reuses `fieldName` as the result name. |
| `forFieldOrAttribute` | `def forFieldOrAttribute(fieldName: String, resultName: Option[String]): Context` | Total variant: if `fieldName` is not a schema field it falls back to `ScalarType.AttributeType`, so it never errors. Used for synthetic attribute paths (for example join keys) that have no GraphQL field. |
| `forPath` | `def forPath(path1: List[String]): Result[Context]` | Fold `forField(hd, hd)` over a list of field names to reach a deeper position. |
| `asType` | `def asType(tpe: Type): Context` | Replace the type at the current position (`typePath.head`, or `rootTpe` at the root) without changing the paths. This is how a cursor re-types itself on narrowing or when entering a child of a known type. |
| `forUnderlyingNamed` | `def forUnderlyingNamed: Context` | Replace the current type with its `underlyingNamed.dealias` — strip list/nullable wrappers and aliases down to the named type. |
| `parent` | `def parent: Option[Context]` | Drop the head of all three paths, yielding the enclosing context, or `None` at the root. |

`forField` and `forFieldOrAttribute` are the workhorses: the interpreter calls them as it descends from a parent cursor into a selected field, and a [`SelectElaborator`](elab-phases.md) walks them to validate paths inside predicates.

### Companion constructors

`object Context` provides three ways to build a context. The single-argument form is the usual entry point for rooting a context at an operation's root type.

| Constructor | Signature | Result |
| --- | --- | --- |
| `Context(rootTpe)` | `def apply(rootTpe: Type): Context` | A root context: `Context(rootTpe, Nil, Nil, Nil)`. |
| `Context(rootTpe, fieldName, resultName)` | `def apply(rootTpe: Type, fieldName: String, resultName: Option[String]): Option[Context]` | A one-field context, or `None` if `fieldName` is not a field of `rootTpe`. |
| `Context(path)` | `def apply(path: Path): Result[Context]` | Fold `forField` over a [`Path`](predicates.md), returning a `Result` because any step can fail. |

### `equals` / `hashCode` caveats

`Context` overrides both `equals` and `hashCode`, and the implementations are deliberately narrower than the generated case-class versions. Read them before using a `Context` as a map key or comparing two by `==`:

```scala
override def equals(other: Any): Boolean =
  other match {
    case Context(oRootTpe, oPath, oResultPath, _) =>
      rootTpe =:= oRootTpe && resultPath == oResultPath && path == oPath
    case _ => false
  }

override def hashCode(): Int = resultPath.hashCode
```

Two consequences follow:

- **`typePath` is ignored by `equals`.** Only `rootTpe`, `path`, and `resultPath` are compared. Two contexts that disagree only in the *types* along the path (for example one narrowed to a subtype) are considered equal. This is intentional — the interpreter keys deferred work on output position, not on the type stack.
- **`rootTpe` is compared with `=:=`, not `==`.** Roots compare by GraphQL type *equivalence* (dealiased structural equality), so a `TypeRef` and its resolved definition match. `hashCode`, however, is *only* `resultPath.hashCode`, so it ignores `rootTpe` and `path` entirely. The pair is internally consistent (equal contexts share a `resultPath` and so share a hash), but the hash is intentionally coarse — expect collisions between contexts that differ only in `rootTpe` or `path`.

## `Env`

`Env` is a sealed key/value environment threaded through cursors and query elaboration. It has exactly two cases — `EmptyEnv` and `NonEmptyEnv` — and stores values as `Any`, recovering their type at lookup via a `ClassTag`.

```scala
sealed trait Env {
  def add[T](items: (String, T)*): Env
  def add(env: Env): Env
  def contains(name: String): Boolean
  def get[T: ClassTag](name: String): Option[T]
  def isEmpty: Boolean

  def getR[A: ClassTag: TypeName](name: String): Result[A]
  def addFromQuery(query: Query): Env
}
```

| Member | Signature | Behaviour |
| --- | --- | --- |
| `add` | `def add[T](items: (String, T)*): Env` | Add one or more bindings, returning a `NonEmptyEnv`. Later keys overwrite earlier ones. |
| `add` | `def add(env: Env): Env` | Merge another `Env` in; its bindings win on key clashes. |
| `contains` | `def contains(name: String): Boolean` | Whether `name` is bound (regardless of value type). |
| `get` | `def get[T: ClassTag](name: String): Option[T]` | Type-checked lookup: `Some(v)` only if `name` is bound **and** its value is a `T`; otherwise `None`. |
| `getR` | `def getR[A: ClassTag: TypeName](name: String): Result[A]` | Like `get`, but a missing or wrongly-typed key is a `Result` failure carrying `"Key '…' of type … was not found in …"` rather than `None`. |
| `isEmpty` | `def isEmpty: Boolean` | Whether the environment holds no bindings. |
| `addFromQuery` | `def addFromQuery(query: Query): Env` | Walk leading `Query.Environment` nodes off the front of `query`, folding each node's `Env` into this one (and stopping at the first non-`Environment` node). |

### Type-checked lookups

`get` (and therefore `getR`) is checked at run time by the value's `ClassTag`: `NonEmptyEnv.get` does `elems.get(name).flatMap(classTag[T].unapply)`, so a key that *is* present but stored as a different type returns `None` — not the raw value cast blindly. Use `getR` when you want a descriptive error instead of a silent `None`; its message includes the requested type name (via `TypeName`) and the current environment contents.

### Constructors

`object Env` exposes the empty value and a varargs factory.

| Constructor | Signature | Result |
| --- | --- | --- |
| `Env.empty` | `def empty: Env` | The `EmptyEnv` singleton. |
| `Env(items*)` | `def apply[T](items: (String, T)*): Env` | A `NonEmptyEnv(Map(items: _*))`, or an empty map's `NonEmptyEnv` if no items are given. |

The two concrete cases are `case object EmptyEnv` (every lookup is `None`/`false`; `add` promotes it to a `NonEmptyEnv`) and `case class NonEmptyEnv(elems: Map[String, Any])`.

### Where `Env` comes from

During elaboration a `SelectElaborator` stages values into the environment with `Elab.env(...)`, which the compiler emits as `Query.Environment(env, child)` nodes in the [query algebra](query-algebra.md). At interpretation time `addFromQuery` lifts those nodes' bindings into the live environment. A [`Cursor`](cursor.md) reads them back through `env`/`envR`/`fullEnv`, walking from the local environment up through its parents. A field resolver therefore sees a value a `SelectElaborator` case staged earlier, without that value appearing anywhere in the cursor's backing data.

## Example: rooting a context and a round-trip through `Env`

The following compiles against the `QuickStartMapping` doc example, whose schema has a `Query` type with a `book` field of type `Book`. Rooting a `Context` at the `Query` type and deriving the child `book` context returns a `Result` because `forField` is partial:

```scala mdoc:silent
import grackle._
import grackle.docs.QuickStartMapping

val rootCtx: Context = Context(QuickStartMapping.QueryType)
```

```scala mdoc
rootCtx.isRoot
rootCtx.tpe

// Descend into the `book` field; `forField` returns a Result because the
// field might not exist on the current type.
val bookCtx: Result[Context] = rootCtx.forField("book", "book")
bookCtx.toOption.map(_.path)        // innermost-first: List("book")
bookCtx.toOption.map(_.resultPath)  // alias-applied path

// An unknown field is a Result failure, not an exception.
rootCtx.forField("nope", "nope")
```

`Env` lookups are type-checked at run time, so asking for the wrong type yields `None` (from `get`) or a descriptive failure (from `getR`):

```scala mdoc
val env: Env = Env("answer" -> 42)

env.get[Int]("answer")      // Some(42)
env.get[String]("answer")   // None — present, but not a String
env.getR[Int]("answer")     // Result.Success(42)
env.getR[String]("answer")  // Result.Failure with a descriptive message
env.contains("answer")
```

## See also

- [Cursor reference](cursor.md) — the navigator that carries a `Context` and an `Env`.
- [Query algebra reference](query-algebra.md) — the `Query.Environment` node that injects an `Env`.
- [Elaboration phases reference](elab-phases.md) — where a `SelectElaborator` stages values with `Elab.env`.
- [Result, Problem & ResultT reference](result-problem.md) — the `Result` returned by `forField` and `getR`.
- [How the query interpreter works](../concepts/query-interpreter.md) — how contexts and environments thread through a full query walk.
