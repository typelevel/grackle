# Mapping types reference

This page is the full catalog of Grackle's mapping constructs: the `Mapping[F]` base, its query entry points, the two kinds of `TypeMapping` (`ObjectMapping` and `LeafMapping[T]`), every `FieldMapping` (`CursorField`, `ValueField`, `Delegate`, `EffectField`, `RootEffect`, `RootStream`, plus circe's `CirceField`/`CursorFieldJson`), the `MappingPredicate`s that select a type mapping, and the `TypeMappings` builders. It is a lookup reference for developers writing mapping constructors; for the *why* behind the design read [Mappings and cursors](../concepts/mappings-cursors.md), and for an end-to-end walkthrough see the [in-memory model tutorial](../tutorial/in-memory-model.md). All members below live in `modules/core/src/main/scala/mapping.scala` unless noted otherwise.

## `Mapping[F]`

`Mapping[F[_]]` is the abstract base that ties a GraphQL `Schema` to an underlying data source. A concrete mapping supplies three members; from them Grackle derives the compiler and interpreter.

| Member | Type | Provided by you? |
| --- | --- | --- |
| `M` | `implicit val M: MonadThrow[F]` | yes (usually via the backend base class) |
| `schema` | `val schema: Schema` | yes |
| `typeMappings` | `val typeMappings: TypeMappings` | yes |
| `selectElaborator` | `val selectElaborator: SelectElaborator` | optional override (defaults to `SelectElaborator.identity`) |
| `compiler` | `lazy val` `QueryCompiler` | derived |
| `interpreter` | `QueryInterpreter[F]` | derived |
| `compilerPhases` | `def compilerPhases: List[QueryCompiler.Phase]` | derived (`List(selectElaborator, componentElaborator, effectElaborator)`) |

Everything in this catalog — `ObjectMapping`, `LeafMapping`, `CursorField`, `RootEffect`, the `ValidationFailure` case classes — is a **path-dependent type nested inside `Mapping[F]`**. You reference these members through a concrete mapping instance or subclass; for example a validation match reads `case List(M.MissingTypeMapping(_))` where `M` is your mapping. The concrete backends — `ValueMapping` (in-memory Scala values), `ComposedMapping` (federation), `CirceMapping`, `GenericMapping`, `SqlMapping` — each extend `Mapping[F]` and override `mkCursorForMappedField` to build backend-specific `Cursor`s.

A minimal `ValueMapping` shows the required members in place — schema literal, type refs, a `List` of object mappings (implicitly a `TypeMappings`, see [below](#typemappings-builders)), and a `selectElaborator`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/composed/ComposedData.scala", "#composed_currency"))
```

The `CurrencyMapping` above declares `schema`, derives the type refs `QueryType`/`CurrencyType` with `schema.ref(...)`, lists two `ValueObjectMapping`s, and overrides `selectElaborator` to turn the `code` argument into a `Unique`/`Filter`/`Eql` query. There is no `compiler` or `interpreter` to write — Grackle derives them.

## Entry points

These methods on `Mapping[F]` run operations or are called by the interpreter. The implicit `Compiler[F, F]` parameter is supplied by `import grackle.syntax._`.

| Method | Signature | Purpose |
| --- | --- | --- |
| `compileAndRun` | `def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty)(implicit sc: Compiler[F, F]): F[Json]` | Compile and run one query/mutation, yielding the JSON response. |
| `compileAndRunSubscription` | `def compileAndRunSubscription(text: String, ...): Stream[F, Json]` | Compile and run a subscription, yielding a stream of JSON responses. |
| `combineAndRun` | `def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]]` | Evaluate a batch of deferred `(query, cursor)` pairs in one interpreter stage. |
| `defaultRootCursor` | `def defaultRootCursor(query: Query, tpe: Type, parentCursor: Option[Cursor]): F[Result[(Query, Cursor)]]` | Yield the default root cursor focused on the top-level operation type. |
| `mkCursorForMappedField` | `protected def mkCursorForMappedField(parent: Cursor, fieldContext: Context, fm: FieldMapping): Result[Cursor]` | The override point each backend extends to build a child `Cursor` for a resolved field mapping. |

`compileAndRun` is implemented on top of `compileAndRunSubscription` and expects **exactly one** result element; running a query whose root produces zero or more than one element (e.g. a `Subscription` root) raises `IllegalStateException`. Use `compileAndRunSubscription` for subscriptions. `combineAndRun`'s default runs each deferred query one-shot and combines the `ProtoJson` results; backends that can batch (such as SQL) override it to issue a single combined query — this is the mechanism behind `Delegate`/`ComposedMapping` cross-backend joins. See [Running operations](running-operations.md) for the full driving API.

## `TypeMapping`

A `TypeMapping` maps a whole GraphQL type and carries a `MappingPredicate` that decides when it applies; its GraphQL type comes from that predicate (`tpe = predicate.tpe`). It is `sealed` with exactly two concrete kinds.

```scala
sealed trait TypeMapping extends TypeMappingCompat with Product with Serializable {
  def predicate: MappingPredicate
  def pos: SourcePos
  def tpe: NamedType = predicate.tpe
}
```

### `ObjectMapping`

Maps an object, interface or union type to its field mappings.

```scala
abstract class ObjectMapping extends TypeMapping {
  def fieldMappings: Seq[FieldMapping]
  def fieldMapping(fieldName: String): Option[FieldMapping]
}
```

Constructors (companion `object ObjectMapping`):

| Constructor | Use |
| --- | --- |
| `ObjectMapping(predicate)(fieldMappings: FieldMapping*)` | Explicit `MappingPredicate`. |
| `ObjectMapping(tpe: NamedType)(fieldMappings: FieldMapping*)` | Curried form keyed on a type ref — the most common. |
| `ObjectMapping(path: Path)(fieldMappings: FieldMapping*)` | Path-sensitive (builds a `PathMatch`). |
| `ObjectMapping(tpe: NamedType, fieldMappings: List[FieldMapping])` | Named-argument form; common in test/demo code. |

The `ValueMapping` backend adds `ValueObjectMapping`, whose narrowing for interfaces/unions is driven by a runtime `ClassTag`: `ValueObjectMapping[T](tpe = ...)(...)` or the `ValueObjectMapping(tpe).on[T](...)` builder. You must supply the correct concrete `[T]` because narrowing tests `classTag.runtimeClass.isInstance(focus)`; an erased or wrong type parameter breaks `narrowsTo`/`narrow`.

### `LeafMapping[T]`

Maps a scalar or enum GraphQL type to a circe `Encoder`. **There is no `PrimitiveMapping` in Grackle — `LeafMapping[T]` is the leaf/primitive construct.**

```scala
trait LeafMapping[T] extends TypeMapping {
  def encoder: Encoder[T]
  def scalaTypeName: String
  def pos: SourcePos
}
```

Constructors require an implicit circe `Encoder[T]` and a `TypeName[T]`:

| Constructor | Notes |
| --- | --- |
| `LeafMapping[T](predicate)` | Explicit predicate. |
| `LeafMapping[T](tpe: NamedType)` | Keyed on a type ref — the common form. |
| `LeafMapping[T](path: Path)` | Path-sensitive. |

You only declare `LeafMapping`s for **custom** scalars and enums; the built-in leaf mappings for `String`, `Int`, `Float`, `Boolean` and `ID` are appended automatically by `TypeMappings` (see [below](#typemappings-builders)). A list of `LeafMapping`s for custom scalar/enum types looks like this:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ScalarsSuite.scala", "#scalars_leafmappings"))
```

Each `LeafMapping[T](tpe)` above needs an implicit circe `Encoder[T]` in scope (for example a `Genre` enum encoder) and resolves the schema's `UUIDType`, `GenreType`, `DateType` and so on to those Scala types. There is no separate primitive construct — these leaf mappings *are* how the custom scalars are encoded.

## `FieldMapping`

A `FieldMapping` maps a single field **inside an `ObjectMapping`**. The base trait exposes the field name plus flags that govern lookup and traversal.

```scala
trait FieldMapping extends Product with Serializable {
  def fieldName: String
  def hidden: Boolean
  def subtree: Boolean
  def pos: SourcePos
}
```

`hidden = true` marks a synthetic/attribute field used only as a `required` dependency of another field — a *declared* schema field whose mapping is hidden triggers a `DeclaredFieldMappingIsHidden` validation failure. `subtree = true` marks fields whose value is a sub-tree the interpreter descends into (effects and delegates), versus leaf-producing fields. The catalog:

| Field mapping | Signature (constructor) | Semantics |
| --- | --- | --- |
| `CursorField[T]` | `CursorField[T](fieldName, f: Cursor => Result[T], required: List[String] = Nil, hidden = false)(implicit Encoder[T])` | Computes a leaf value from the parent `Cursor`. `required` lists sibling fields/attributes `f` reads, so the interpreter materializes them. `encoder` is implicit. |
| `ValueField[T]` | `ValueField[T](fieldName, f: T => Any, hidden = false)` | Maps a field to a function from the parent value `T` to a child value (`ValueMapping` only). Operates on the parent focus, not sibling cursors. `ValueField.fromValue(name, t)` yields a constant. |
| `Delegate` | `Delegate(fieldName, mapping: Mapping[F], join: (Query, Cursor) => Result[Query] = ComponentElaborator.TrivialJoin)` | Delegates the field to another `Mapping` (cross-backend composition). `join` rewrites the sub-query against the parent cursor. `subtree = true`. |
| `EffectField` | `EffectField(fieldName, handler: EffectHandler[F], required: List[String] = Nil, hidden = false)` | A field whose value is produced by a deferred, batched `EffectHandler[F]`. Extends `EffectMapping` (`subtree = true`). |
| `RootEffect` | constructed only via the companion (see below) | Runs an initial `F`-effect for a root query/mutation field, producing a `(Query, Cursor)`. |
| `RootStream` | constructed only via the companion (see below) | Streaming analogue of `RootEffect` for subscription roots. |

### `RootEffect` / `RootStream`

`RootEffect` and `RootStream` are **private case classes** — never use `new`. Construct them through these companion factories, choosing the shape that matches what your effect needs to compute:

| Factory | Signature | When |
| --- | --- | --- |
| `RootEffect.apply` | `RootEffect(fieldName)(effect: (Query, Path, Env) => F[Result[(Query, Cursor)]])` | Full control over both the rewritten query and the cursor. |
| `RootEffect.computeCursor` | `RootEffect.computeCursor(fieldName)(effect: (Path, Env) => F[Result[Cursor]])` | Query unchanged; build the root cursor (e.g. with `valueCursor(path, env, value)`). |
| `RootEffect.computeChild` | `RootEffect.computeChild(fieldName)(effect: (Query, Path, Env) => F[Result[Query]])` | Cursor unchanged; rewrite the child query. Raises an internal error if the root query has no extractable child. |
| `RootEffect.computeUnit` | `RootEffect.computeUnit(fieldName)(effect: Env => F[Result[Unit]])` | Run a side effect only; query and cursor unchanged (typical for a mutation that returns the recorded value). |
| `RootStream.apply` | `RootStream(fieldName)(effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]])` | Full streaming control. |
| `RootStream.computeCursor` | `RootStream.computeCursor(fieldName)(effect: (Path, Env) => Stream[F, Result[Cursor]])` | Subscription root emitting a cursor per element. |
| `RootStream.computeChild` | `RootStream.computeChild(fieldName)(effect: (Query, Path, Env) => Stream[F, Result[Query]])` | Subscription root rewriting the child query per element. |

`mapping.rootEffect` / `mapping.rootStream` look these up by field. The `EffectField`/`RootEffect`/`RootStream` mappings drive the `effectElaborator` compiler phase. See [Effects reference](effects.md) for the effect machinery and [Running operations](running-operations.md) for driving subscriptions.

### Circe field mappings

`CirceField` and `CursorFieldJson` are **not in core** — they live in `modules/circe/src/main/scala/circemapping.scala` and are usable only from a `CirceMapping`/`CirceMappingLike` subtype.

| Field mapping | Signature | Semantics |
| --- | --- | --- |
| `CirceField` | `CirceField(fieldName, value: Json, hidden = false)` | Maps a field to a constant `Json` value. |
| `CursorFieldJson` | `CursorFieldJson(fieldName, f: Cursor => Result[Json], required: List[String], hidden = false)` | Computes a `Json` subtree from the parent `Cursor` (e.g. decoding a JSON blob stored in a column). |

The circe module also adds `RootEffect.computeJson` / `computeEncodable` syntax. See [CirceMapping reference](circe-mapping.md).

## `MappingPredicate`

A `MappingPredicate` decides when a `TypeMapping` applies, returning an `Option[Int]` **priority** (highest wins; equal highest priorities are an `AmbiguousTypeMappings` error).

```scala
trait MappingPredicate {
  def tpe: NamedType
  def apply(ctx: Context): Option[Int]
  def continuationContext(ctx: Context): Option[Context]
}
```

| Predicate | Signature | Priority | Use |
| --- | --- | --- | --- |
| `TypeMatch` | `case class TypeMatch(tpe: NamedType)` | `0` | Matches the type in any context. |
| `PathMatch` | `case class PathMatch(path: Path)` | `path.length + 1` for a non-empty path (`0` for an empty path) | Matches a type reached via a specific field path; preferred for context-sensitive mappings. |
| `PrefixedTypeMatch` | `case class PrefixedTypeMatch(prefix: List[String], tpe: NamedType)` | `prefix.length` | Legacy `PrefixedMapping` semantics. |

Because a non-empty `PathMatch`/`PrefixedTypeMatch` carries higher priority than a bare `TypeMatch` (which is always `0`), a path-specific mapping wins over a plain type mapping for the same type in that path. The `ObjectMapping(tpe)(...)`, `ObjectMapping(path)(...)`, `LeafMapping[T](tpe)` convenience constructors build the appropriate predicate for you, so you rarely instantiate these directly. Prefer `PathMatch` over `PrefixedTypeMatch` in new code; `PrefixedMapping(tpe, mappings)` remains as a backwards-compat shim that throws `IllegalArgumentException` unless all entries share the same `tpe`.

## `TypeMappings` builders

`TypeMappings` is the indexed catalog the mapping holds: a `Seq[TypeMapping]` plus lookup indices, with `typeMapping(context)`, `objectMapping(context)`, `fieldMapping(context, fieldName)` and `encoderForLeaf(context)` lookups, and a `validate(severity)` method. Construct it via the companion.

| Builder | Signature | Behaviour |
| --- | --- | --- |
| `TypeMappings.apply` | `apply(mappings: Seq[TypeMapping])` / `apply(mappings: TypeMapping*)` | **Checked** — the catalog is validated on first use. |
| `TypeMappings.unchecked` | `unchecked(mappings: Seq[TypeMapping])` / `unchecked(mappings: TypeMapping*)` | Skips validation. |
| `TypeMappings.fromList` | `implicit def fromList(mappings: List[TypeMappingCompat]): TypeMappings` | Lets a plain `List` be used where `TypeMappings` is expected. |
| `TypeMappings.empty` | `val empty: TypeMappings` | The empty catalog. |

Writing `val typeMappings = List(...)` compiles only because of the implicit `fromList` — and `fromList` always builds a **checked** catalog. To skip validation you must call `TypeMappings.unchecked(...)` explicitly. Built-in leaf mappings for `String`, `Int`, `Float`, `Boolean` and `ID` are always appended automatically, so you never declare them yourself.

Validation does **not** run at construction time. A checked catalog is validated lazily the first time the `compiler` val is forced (`unsafeValidateIfChecked`), which raises a `ValidationException` — so a bad mapping can look fine until the first query is compiled. `validate(severity)` unfolds the schema from the root operation types and reports `ValidationFailure`s including `MissingTypeMapping`, `AmbiguousTypeMappings`, `MissingFieldMapping`, `DeclaredFieldMappingIsHidden`, `ObjectTypeExpected`, `LeafTypeExpected`, `ReferencedTypeDoesNotExist`, `UnusedTypeMapping` and `UnusedFieldMapping`. See [Validate a mapping](../how-to/validate-mappings.md) for triggering and matching each failure.

## See also

- [Mappings and cursors](../concepts/mappings-cursors.md) — the two-layer model and how `mkCursorForMappedField` wires fields to data.
- [Build an in-memory model](../tutorial/in-memory-model.md) — a `GenericMapping` over Star Wars ADTs; for a `ValueMapping` from scratch see the [quick start](../getting-started/quick-start.md).
- [Validate a mapping and read the failures](../how-to/validate-mappings.md) — every `ValidationFailure` with worked examples.
- [Effects reference](effects.md) — `RootEffect`, `RootStream` and `EffectHandler` in depth.
- [Running operations reference](running-operations.md) — `compileAndRun` and the driving API.
- [CirceMapping reference](circe-mapping.md) — `CirceField`, `CursorFieldJson` and circe roots.
