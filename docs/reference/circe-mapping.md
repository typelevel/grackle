# CirceMapping reference

The circe backend builds a Grackle [`Mapping`](mapping-types.md) whose data source is in-memory circe `Json` rather than a database. The whole subsystem lives in one file, `modules/circe/src/main/scala/circemapping.scala`, and requires no database — `F` only needs a `cats.MonadThrow`. This page lists its entry points (`CirceMapping`/`CirceMappingLike`), the field mappings (`CirceField`, `CursorFieldJson`), the [`Cursor`](cursor.md) that walks JSON against the schema (`CirceCursor`), the `circeCursor` helper, the `RootEffect`/`RootStream` syntax extensions, and the leaf-validation rules. It is for developers building or reading a circe-backed mapping; for a task-oriented walkthrough see [Serving a GraphQL API from circe JSON](../how-to/circe-backend.md).

## Entry points

| Type | Signature | Purpose |
| --- | --- | --- |
| `CirceMapping[F]` | `abstract class CirceMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with CirceMappingLike[F]` | Convenience base class for a standalone JSON-backed mapping. Extend it and provide `schema` and `typeMappings`. |
| `CirceMappingLike[F]` | `trait CirceMappingLike[F[_]] extends Mapping[F]` | The mixin carrying all circe behaviour. Mix into another mapping to add JSON-valued fields. |

`CirceMappingLike` is the trait carrying all behaviour; `CirceMapping` adds the `MonadThrow[F]` instance and the `Mapping[F]` base. Use the trait directly when you want to splice JSON fields into a non-circe backend rather than start from scratch — `SqlMappingLike[F] extends CirceMappingLike[F]` (`modules/sql-core/src/main/scala/SqlMapping.scala`), which is the only reason `CirceField`, `CursorFieldJson`, and `circeCursor` are usable inside [SQL mappings](sql-mapping.md).

The base class imposes only `MonadThrow[F]`. The effectful-root examples below use `Sync[F]`/`IO`, but that is a requirement of their effect bodies, not of `CirceMapping`.

## A standalone mapping

A `CirceMapping` serves an entire subtree from one constant `Json`. The canonical example (`modules/circe/src/test/scala/CirceData.scala`) defines a schema, a `json"""…"""` fixture, and maps the `root` field to it with `CirceField`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CirceData.scala", "#circe_mapping"))
```

The `CirceField("root", data)` mapping serves the whole `Root` subtree from `data`. Note two further points the snippet illustrates:

- **Custom scalars need a `LeafMapping`.** The schema declares `scalar BigDecimal`; serving it from JSON requires `LeafMapping[BigDecimal](BigDecimalType)`. Built-in scalars (`Int`/`Float`/`String`/`Boolean`/`ID`) and enums need no mapping.
- **`CursorField` can read JSON not in the schema.** `data` has a top-level `"hidden": 13` that the schema does not expose. `CursorField("computed", computeField, List("hidden"))` declares `"hidden"` as `required`, and `computeField` reads it with `c.fieldAs[Json]("hidden")`. Querying `hidden` directly fails with `No field 'hidden' for type Root`; it is reachable only internally.

## Field mappings

Both circe field mappings extend the sealed parent `CirceFieldMapping`, whose `subtree` flag is `true`:

| Type | Signature | Serves |
| --- | --- | --- |
| `CirceFieldMapping` | `sealed trait CirceFieldMapping extends FieldMapping { def subtree: Boolean = true }` | Sealed parent; marks both mappings as opaque subtrees. |
| `CirceField` | `case class CirceField(fieldName: String, value: Json, hidden: Boolean = false)(implicit val pos: SourcePos)` | A constant `Json` subtree for `fieldName`. |
| `CursorFieldJson` | `case class CursorFieldJson(fieldName: String, f: Cursor => Result[Json], required: List[String], hidden: Boolean = false)(implicit val pos: SourcePos)` | A `Json` subtree computed from the parent `Cursor` by `f`. `required` lists sibling fields `f` may read. |

`CursorFieldJson` is the bridge for splicing JSON into a non-JSON backend: a SQL mapping uses it to decode a column into a `Json` value (`modules/sql-core/src/test/scala/SqlCursorJsonMapping.scala`).

### Opaque-subtree priority

Because `subtree == true`, the `Json` a field mapping supplies is self-contained: `CirceCursor.field` looks for the field **inside the current JSON object first**, and only falls back to the mapping's other `typeMappings` when the JSON object lacks that field. So an explicit field mapping on a nested type is shadowed whenever the JSON already provides the field. `CircePrioritySuite` (`modules/circe/src/test/scala/CircePrioritySuite.scala`) demonstrates this:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CircePrioritySuite.scala", "#circe_priority"))
```

`present`'s JSON contains `monkey.name`, so the literal `"Bob"` wins. `fallback`'s JSON omits `name`, so Grackle falls back to the explicit `CirceField("name", "Steve")` on `Monkey`. Querying `present.monkey.name` returns `"Bob"`; `fallback.monkey.name` returns `"Steve"`.

## `CirceCursor`

`CirceCursor` is the [`Cursor`](cursor.md) implementation that traverses a `Json` `focus` against the GraphQL type in its `Context`. It is constructed by `mkCursorForMappedField` (for `CirceField`/`CursorFieldJson`) and by `circeCursor` (for effectful roots).

```scala
case class CirceCursor(context: Context, focus: Json, parent: Option[Cursor], env: Env) extends Cursor
```

| Method | Signature | Behaviour |
| --- | --- | --- |
| `field` | `def field(fieldName: String, resultName: Option[String]): Result[Cursor]` | Looks up `fieldName` in the JSON object; if present, descends. If absent, falls back to `mkCursorForField` (the mapping's other `typeMappings`). This fallback is the opaque-subtree priority rule above. |
| `isLeaf` / `asLeaf` | `def isLeaf: Boolean` · `def asLeaf: Result[Json]` | Whether the focus is a scalar/enum of the expected shape, and its rendering as `Json`. See [leaf validation](#leaf-validation) below. |
| `isList` / `asList` | `def isList: Boolean` · `def asList[C](factory: Factory[Cursor, C]): Result[C]` · `def listSize: Result[Int]` | True when `tpe.isList` and the focus is a JSON array; `asList` yields one child cursor per element. A non-array focus is an internal error. |
| `isNullable` / `asNullable` | `def isNullable: Boolean` · `def asNullable: Result[Option[Cursor]]` · `def isDefined: Result[Boolean]` | A JSON `null` focus yields `None`; any other value yields `Some(child)`. |
| `narrowsTo` / `narrow` | `def narrowsTo(subtpe: TypeRef): Result[Boolean]` · `def narrow(subtpe: TypeRef): Result[Cursor]` | Interface/union narrowing by JSON key inspection. See [narrowing](#narrowing) below. |
| `preunique` | `def preunique: Result[Cursor]` | Re-types an array focus as `tpe.nonNull.list` for a `Unique` operation; a non-array focus is an internal error. |
| `withEnv` | `def withEnv(env0: Env): Cursor` | Returns a copy with `env0` added to the existing `env`. |
| `mkChild` | `def mkChild(context: Context = context, focus: Json = focus): CirceCursor` | Builds a child cursor. It resets the child's `env` to `Env.empty` — environment bindings do **not** propagate downward through navigation automatically. |

### Leaf validation

`asLeaf` dispatches on `tpe.dealias` and validates the JSON shape against the expected scalar/enum type. The rules (from `CirceCursor.asLeaf`) are exact:

| Expected type | JSON requirement | Result |
| --- | --- | --- |
| `BooleanType` | `focus.isBoolean` | passes the JSON through unchanged |
| `StringType`, `IDType` | `focus.isString` | passes the JSON through unchanged |
| `IntType` | `focus.isNumber` | re-parsed via `_.toLong` and re-emitted as `Json.fromLong`; a non-integral number or one overflowing `Long` fails with `Expected Int found …` |
| `FloatType` | `focus.isNumber` | passes the number through unchanged |
| `EnumType` | `focus.isString` and the string is a declared value | passes through; a string that is not a declared enum value is an internal error (`Expected Enum …`) |
| any other `ScalarType` | `!focus.isObject` | catch-all for custom scalars: any non-object JSON passes through |
| anything else | — | internal error (`Expected Scalar type, found …`) |

The custom-scalar catch-all is why a custom scalar still wants its own `LeafMapping[T]`: without it the raw JSON passes through, but the value is not registered/encoded as your scalar type. `isLeaf` mirrors these shape checks (`Boolean→isBoolean`, `String`/`ID→isString`, `Int`/`Float→isNumber`, `Enum→isString`) but does not validate enum membership.

### Narrowing

`narrowsTo` decides interface/union fragment membership and `__typename` resolution by inspecting the JSON object's keys. A subtype `subtpe` matches only when **both** hold:

- every non-nullable field of `subtpe` is present as a key in the JSON object, and
- every key of the JSON object corresponds to a field of `subtpe`.

A missing required key, or an extra unknown key, causes the narrow to fail. `narrow` re-types the cursor to `subtpe` when `narrowsTo` succeeds, and is an internal error otherwise.

## `circeCursor`

`circeCursor` builds the right cursor for a position. It backs the effect syntax and is callable directly inside `computeCursor` handlers.

```scala
def circeCursor(path: Path, env: Env, value: Json): Cursor
```

| `path` | Result |
| --- | --- |
| `path.isRoot` | a `CirceCursor` at `Context(path.rootTpe)`, focused directly on `value` |
| otherwise | a `DeferredCursor` that builds the `CirceCursor` once the parent `Context` is known |

The path choice determines what `value` must contain. With `circeCursor(p, e, json)` (and therefore `RootEffect.computeJson`, which uses `p`), `json` is the **value of the field**. With `circeCursor(Path.from(p.rootTpe), e, json)`, `json` is an **object containing the field name**. Mixing these up produces a shape mismatch.

`mkCursorForMappedField` is the override that turns circe field mappings into cursors:

| Field mapping | Cursor produced |
| --- | --- |
| `CirceField(_, json, _)` | `CirceCursor(fieldContext, json, Some(parent), parent.env)` |
| `CursorFieldJson(_, f, _, _)` | runs `f(parent)`, then `CirceCursor(fieldContext, result, Some(parent), parent.env)` |
| anything else | delegates to `super.mkCursorForMappedField` |

## Effectful and streaming roots

`CirceMappingLike` enriches the `RootEffect` and `RootStream` companions with circe-specific constructors. See [the effects reference](effects.md) for `RootEffect`/`RootStream` in general.

| Syntax | Signature |
| --- | --- |
| `RootEffect.computeJson` | `def computeJson(fieldName: String)(effect: (Path, Env) => F[Result[Json]])(implicit pos: SourcePos): RootEffect` |
| `RootEffect.computeEncodable` | `def computeEncodable[A](fieldName: String)(effect: (Path, Env) => F[Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootEffect` |
| `RootStream.computeJson` | `def computeJson(fieldName: String)(effect: (Path, Env) => Stream[F, Result[Json]])(implicit pos: SourcePos): RootStream` |
| `RootStream.computeEncodable` | `def computeEncodable[A](fieldName: String)(effect: (Path, Env) => Stream[F, Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootStream` |

`computeJson` delegates to the core `computeCursor`, mapping the yielded `Json` through `circeCursor(p, e, _)`. `computeEncodable[A]` is `computeJson` after applying the implicit circe `Encoder[A]`. The `RootStream` variants are the subscription/stream forms: each emitted `Json` (or encoded value) becomes a `CirceCursor` result. Grackle ships no websocket transport — wire the resulting `fs2.Stream` to one yourself.

The three effect styles side by side (`modules/circe/src/test/scala/CirceEffectData.scala`):

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CirceEffectData.scala", "#circe_effects"))
```

- `foo` uses `RootEffect.computeCursor` with an explicit `circeCursor(p, e, json)` — full control, `json` is the field value.
- `bar` uses `computeJson`: yield the `Json`, the syntax wraps it.
- `baz` uses `computeEncodable`: yield a `Struct`, the implicit `Encoder[Struct]` and the syntax do the rest.
- `qux` uses `computeCursor` rooted via `Path.from(p.rootTpe)`, so its `json` is an object `{ "qux": { … } }` that includes the field name.

The effect bodies here perform a real effect (a `SignallingRef` update) to prove each root runs exactly once, which is why the mapping is `CirceMapping[F : Sync]`.

## See also

- [Serving a GraphQL API from circe JSON](../how-to/circe-backend.md) — task-oriented recipe using these types.
- [Mapping types reference](mapping-types.md) — the full mapping/field-mapping catalogue this backend plugs into.
- [Cursor reference](cursor.md) — the `Cursor` contract `CirceCursor` implements.
- [Effects reference](effects.md) — `RootEffect`/`RootStream` and how roots run.
- [SQL mapping reference](sql-mapping.md) — `SqlMappingLike` inherits `CirceMappingLike`, enabling `CursorFieldJson` over a database.
