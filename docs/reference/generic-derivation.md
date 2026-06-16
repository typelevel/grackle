# Generic derivation reference

The generic backend (module `grackle-generic`, package `grackle.generic`) serves a GraphQL schema directly from ordinary Scala case classes and sealed traits. This page is the reference for its public surface: the `CursorBuilder[T]` typeclass and the implicit builders, the `derive*` entry points, the `ObjectCursorBuilder` combinators, and the `GenericMapping` wiring (`GenericField`, `genericCursor`). It is for developers already using generic derivation; for a task-oriented walkthrough see the [how-to](../how-to/generic-derivation.md).

```text
libraryDependencies += "org.typelevel" %% "grackle-generic" % "@VERSION@"
```

Derivation is purely structural: there is no `@GraphQLField` or any other annotation. A case-class member name must match its GraphQL field name; to diverge, use `renameField` / `transformFieldNames` / `transformField`. The GraphQL `Type` is always passed explicitly to every `derive*` call — nothing is inferred from the Scala type name.

## `CursorBuilder[T]`

The core typeclass (`modules/generic/src/main/scala/CursorBuilder.scala`). It carries the GraphQL `tpe` it produces and a `build` method that constructs a [`Cursor`](cursor.md) over a value of `T`.

| Member | Signature |
| --- | --- |
| `tpe` | `def tpe: Type` |
| `build` | `def build(context: Context, focus: T, parent: Option[Cursor] = None, env: Env = Env.empty): Result[Cursor]` |
| `contramap` | `final def contramap[A](f: A => T): CursorBuilder[A]` |

Summon an in-scope instance with the companion's `apply`:

| Member | Signature |
| --- | --- |
| `CursorBuilder.apply` | `def apply[T](implicit cb: CursorBuilder[T]): CursorBuilder[T]` |

`contramap` adapts an existing builder to a wrapper type while keeping the same `tpe` — e.g. `CursorBuilder[String].contramap[Planet](_.toString)` for a single-field value class. `build` returns a `Result[Cursor]`; failures are carried as `Problem`s (see [`Result` and `Problem`](result-problem.md)).

## Provided implicit builders

These instances are resolved automatically when deriving products and coproducts, so each field type only needs a `CursorBuilder` in scope. Note the GraphQL `Type` each reports — several Scala types collapse onto a shared GraphQL type.

| Scala type | Implicit | GraphQL `tpe` |
| --- | --- | --- |
| `String` | `stringCursorBuilder` | `StringType` |
| `Int` | `intCursorBuilder` | `IntType` |
| `Long` | `longCursorBuilder` | `IntType` |
| `Float` | `floatCursorBuilder` | `FloatType` |
| `Double` | `doubleCursorBuilder` | `FloatType` |
| `Boolean` | `booleanCursorBuilder` | `BooleanType` |
| `T <: Enumeration#Value` | `enumerationCursorBuilder` | `StringType` |
| `Option[T]` | `optionCursorBuiler` | `NullableType(elem.tpe)` |
| `List[T]` | `listCursorBuiler` | `ListType(elem.tpe)` |
| `T : Encoder` (fallback) | `leafCursorBuilder` | `StringType` |

```scala
implicit def enumerationCursorBuilder[T <: Enumeration#Value]: CursorBuilder[T]
implicit def optionCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[Option[T]]
implicit def listCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[List[T]]
implicit def leafCursorBuilder[T](implicit encoder: Encoder[T]): CursorBuilder[T]
```

Reference notes:

- `Long` reports `IntType` and both `Float`/`Double` report `FloatType`. GraphQL has no separate `Long`/`Double`, so they share `Int`/`Float`.
- An `Enumeration#Value` serialises via `.toString`.
- `optionCursorBuiler` and `listCursorBuiler` are spelled with the typo (missing `d`). If you ever name them directly rather than relying on implicit resolution, use the typo'd spelling.
- `leafCursorBuilder` is an implicit fallback for **any** `T` with a Circe `Encoder[T]`, and it reports `StringType`. This means a domain type with an `Encoder` in scope is silently treated as a `String` leaf even where you intended an object, and can shadow object derivation. Keep `Encoder` instances off types you derive as objects.

## Leaf and enumeration constructors

For custom scalars you build a leaf `CursorBuilder` explicitly and bind it to a schema scalar `Type`.

| Constructor | Signature | Maps to |
| --- | --- | --- |
| `deriveLeafCursorBuilder` | `def deriveLeafCursorBuilder[T](tpe0: Type)(implicit encoder: Encoder[T]): CursorBuilder[T]` | leaf, `tpe0`, JSON via `encoder` |
| `deriveEnumerationCursorBuilder` | `def deriveEnumerationCursorBuilder[T <: Enumeration#Value](tpe0: Type): CursorBuilder[T]` | leaf, `tpe0`, `.toString` |

The implicit `leafCursorBuilder` and `enumerationCursorBuilder` (above) are these same constructors fixed to `StringType`. Use the `derive*` forms when the leaf must report a named custom scalar instead of `StringType`, e.g. `CursorBuilder.deriveLeafCursorBuilder[Genre](GenreType)`. The leaf renders its JSON through the supplied `Encoder`.

## `GenericMapping[F]` and `GenericMappingLike[F]`

Your mapping object extends `GenericMapping[F]`; the derivation entry points and `GenericField` live in `GenericMappingLike[F]` (`modules/generic/src/main/scala/genericmapping.scala`).

| Member | Signature |
| --- | --- |
| `GenericMapping[F]` | `abstract class GenericMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with GenericMappingLike[F]` |
| `genericCursor` | `def genericCursor[T](path: Path, env: Env, t: T)(implicit cb: => CursorBuilder[T]): Result[Cursor]` |

`GenericMapping` requires an implicit `MonadThrow[F]` (the in-memory examples use `IO`). It is a full [`Mapping`](mapping-types.md), so you also supply a `schema` and `typeMappings`. `genericCursor` builds a `Cursor` for a value `t` at an arbitrary `path`: at the root (`path.isRoot`) it builds eagerly; otherwise it returns a `DeferredCursor` that defers construction until the surrounding context and parent are known. It is the value you return from a `RootEffect.computeCursor` to plug a generic value into an effectful root.

## `semiauto` derivation

The two entry points for product (object) and coproduct (interface/union) derivation, in the `semiauto` object inside `GenericMappingLike`.

| Member | Signature |
| --- | --- |
| `deriveObjectCursorBuilder` | `final def deriveObjectCursorBuilder[T](tpe: Type)(implicit mkBuilder: => MkObjectCursorBuilder[T]): ObjectCursorBuilder[T]` |
| `deriveInterfaceCursorBuilder` | `final def deriveInterfaceCursorBuilder[T](tpe: Type)(implicit mkBuilder: => MkInterfaceCursorBuilder[T]): CursorBuilder[T]` |

- `deriveObjectCursorBuilder[T](tpe)` derives an `ObjectCursorBuilder[T]` for a case class, mapping each member name to the `CursorBuilder` resolved for that member's type. `tpe` is the GraphQL object `Type` (a `schema.ref`).
- `deriveInterfaceCursorBuilder[T](tpe)` derives a `CursorBuilder[T]` for a sealed trait whose subtypes each already have a `CursorBuilder`. At runtime it selects the concrete branch and wraps it so the cursor reports the interface `tpe` but can `narrow` to the subtype. It backs GraphQL `interface` and union types.
- The `mkBuilder` evidence (`MkObjectCursorBuilder` / `MkInterfaceCursorBuilder`) is supplied by the scala-version-specific backend; you do not write it by hand.

A mismatch between case-class fields and the schema type's fields is **not** caught at derivation — it surfaces at query time (e.g. `No field ...`).

## `ObjectCursorBuilder[T]` combinators

`deriveObjectCursorBuilder` returns an `ObjectCursorBuilder[T]`, a `CursorBuilder[T]` with three field-customisation combinators. Each returns a new `ObjectCursorBuilder[T]`, so they chain.

| Combinator | Signature | Effect |
| --- | --- | --- |
| `renameField` | `def renameField(from: String, to: String): ObjectCursorBuilder[T]` | derive field `from` under GraphQL name `to` |
| `transformFieldNames` | `def transformFieldNames(f: String => String): ObjectCursorBuilder[T]` | rewrite every derived field name with `f` (e.g. snake_case) |
| `transformField` | `def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T]` | replace the derived field with a computed `Result[U]` |

`transformField` is the escape hatch when a field's Scala shape differs from its GraphQL shape (for example resolving an `Option[List[String]]` of ids to an `Option[List[Character]]` of nested objects). The replacement type `U` must itself have a `CursorBuilder[U]` in scope — that `implicit cb: => CursorBuilder[U]` parameter is summoned to build the replacement cursor. Because it lets a field be computed lazily, `transformField` is also how recursion between object types is broken (model the link as an id and resolve it here).

## `GenericField`

A `FieldMapping` that exposes a Scala value, plus its by-name `CursorBuilder`, at a schema field — typically on the root `Query` `ObjectMapping`.

| Member | Signature |
| --- | --- |
| factory | `def GenericField[T](fieldName: String, t: T, hidden: Boolean = false)(implicit cb: => CursorBuilder[T], pos: SourcePos): GenericField[T]` |
| case class | `case class GenericField[T](fieldName: String, t: T, cb: () => CursorBuilder[T], hidden: Boolean)(implicit val pos: SourcePos) extends FieldMapping` |
| `subtree` | `def subtree: Boolean = true` |

The factory is the normal entry point; the `CursorBuilder` is captured as a `() => CursorBuilder[T]` thunk so mutually-recursive builders can be defined as implicit `val`s without an initialization-order deadlock. `subtree` is `true`, and the field can be `hidden`. `GenericMapping` overrides `mkCursorForMappedField` to dispatch a `GenericField` to `cb().build(...)`.

## Canonical example

The Star Wars demo derives a sealed-trait interface and its two subtype objects, customises a field, and is the end-to-end reference for the API above (`demo/src/main/scala/demo/starwars/StarWarsMapping.scala`).

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#model_types"))
```

Reading it against the reference:

- `Character` is a `sealed trait`; its `implicit val cursorBuilder` is a `deriveInterfaceCursorBuilder[Character](CharacterType)` — a coproduct mapped to the GraphQL interface type.
- `Human` and `Droid` are case classes; each `cursorBuilder` is a `deriveObjectCursorBuilder[…](…Type)` chained with `.transformField("friends")(resolveFriends)`.
- `resolveFriends` returns `Result[Option[List[Character]]]`: the model stores `friends` as `Option[List[String]]` (ids), but the schema declares `friends: [Character!]`. `transformField` bridges the two, and `CursorBuilder[Option[List[Character]]]` is summoned (via the `Option`/`List`/interface builders) to build the replacement.
- Every member name (`id`, `name`, `appearsIn`, `homePlanet`, `primaryFunction`) matches its GraphQL field name exactly — no annotations.

The data is wired at the root with `GenericField`, e.g. `GenericField("hero", characters)`; the implicit `CursorBuilder[List[Character]]` is picked up automatically.

## Traversing a built cursor

The `Cursor` produced by a derived builder is navigated with the usual [`Cursor`](cursor.md) operations. These unit-level examples (`modules/generic/src/test/scala/DerivationSuite.scala`) show `build`, `field`, `asNullable`, `asList`, `asLeaf` and `narrow` on derived builders.

Leaf builders produce leaf cursors whose `asLeaf` yields the encoded JSON:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_primitive"))
```

An object builder's cursor descends into fields; `Option`-typed fields are unwrapped with `asNullable`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_product"))
```

A `List`-typed field is unwrapped with `asList` after `asNullable`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_list"))
```

An interface cursor reports the interface type but `narrow`s to a concrete subtype, after which subtype-only fields (`homePlanet`) become reachable:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/DerivationSuite.scala", "#cursor_narrow"))
```

`narrow(subTpe)` only succeeds when `subTpe <:< interfaceTpe` and the concrete branch type `<:< subTpe`; narrowing to an unrelated type, or selecting a subtype-only field without a matching fragment, raises an `InternalError` (`cannot be narrowed`) into the effect `F` rather than appearing in the GraphQL `errors` array.

## Scala 2 vs Scala 3 backends

The two compiler versions use different derivation engines in separate source roots, but expose the **identical** public surface — `MkObjectCursorBuilder` / `MkInterfaceCursorBuilder` / `ObjectCursorBuilder` and every signature above are the same on both.

| | Scala 2 | Scala 3 |
| --- | --- | --- |
| Source root | `modules/generic/src/main/scala-2/genericmapping2.scala` | `modules/generic/src/main/scala-3/genericmapping3.scala` |
| Engine | shapeless | shapeless3 |
| Object derivation | `Generic` / `LabelledGeneric` / `Keys` | `K0.ProductInstances` / `Labelling` |
| Interface derivation | `Coproduct` / `CLiftAll` | `K0.CoproductInstances` |

These engine-specific implicits (`Generic.Aux`, `K0.ProductInstances`, …) are derivation internals — do not depend on them as public API. Write your code against `derive*`, `ObjectCursorBuilder`, `GenericField` and `genericCursor`, and it compiles unchanged on both versions.

## See also

- [Serve Scala ADTs with generic derivation](../how-to/generic-derivation.md) — task-oriented recipes for the API on this page.
- [Mappings and cursors](../concepts/mappings-cursors.md) — how a `CursorBuilder`'s `Cursor` fits the wider model.
- [`Cursor`](cursor.md) — the navigation operations (`field`, `asNullable`, `asList`, `narrow`) used above.
- [`Result` and `Problem`](result-problem.md) — the `Result[Cursor]` that `build` returns, and where errors surface.
- [Mapping types](mapping-types.md) — `GenericField` alongside the other `FieldMapping`s and `RootEffect`.
