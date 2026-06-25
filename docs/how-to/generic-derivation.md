# Serve Scala ADTs with generic derivation

This how-to shows you how to expose ordinary Scala case classes and sealed traits over GraphQL with Grackle's generic backend (`grackle-generic`, package `grackle.generic`), without writing a field-by-field mapping. It is for developers whose in-memory model already lines up with their schema: you derive a `CursorBuilder` for each domain type, wire the data in with `GenericField`, and reach for a handful of combinators only when a field's Scala shape differs from its GraphQL shape. Every recipe here is effect-free except the last, and none of it needs a database.

## How the generic backend builds a cursor

The whole backend turns on one typeclass: `CursorBuilder[T]`. A `CursorBuilder[T]` knows the GraphQL `tpe: Type` it produces and how to `build` a [`Cursor`](../concepts/mappings-cursors.md) over a value of `T`. Instances are resolved implicitly, so to serve a type you only need a `CursorBuilder` for it in scope.

The backend ships implicit builders for the common leaves and collections, and you rarely name them directly:

- `String`, `Int`, `Boolean` map to `StringType`, `IntType`, `BooleanType`.
- `Long` maps to `IntType`, and both `Float` and `Double` map to `FloatType` — GraphQL has no separate `Long`/`Double`, so they share `Int`/`Float`.
- `Option[T]` becomes a nullable cursor and `List[T]` a list cursor, wrapping the element builder.
- A Scala `Enumeration#Value` serialises to its `toString` and reports `StringType`.
- As a fallback, any type with a Circe `Encoder[T]` in scope becomes a leaf reporting `StringType` (this is what drives `java.time` and `UUID` fields — see [custom scalars](#custom-scalars-and-time-and-uuid-leaves) below).

Because each field type must have a `CursorBuilder` in scope, that last fallback is sharp-edged: a domain type that happens to have a Circe `Encoder` in scope will be treated as a `String` leaf even where you meant it to be a derived object. Keep `Encoder` instances off types you intend to derive as objects.

## Derive object and interface builders

You derive builders semi-automatically with the two entry points on a `GenericMapping`'s `semiauto` object: `deriveObjectCursorBuilder[T](tpe)` for a case class (a GraphQL `object`) and `deriveInterfaceCursorBuilder[T](tpe)` for a sealed trait (a GraphQL `interface` or `union`). Note that the GraphQL `Type` is always passed **explicitly** — nothing is inferred from the Scala type name, so `tpe` is a `schema.ref(...)` you supply.

Here is the model from the Star Wars demo. `Character` is a sealed trait derived as an interface; `Human` and `Droid` are case classes derived as objects:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#model_types"))
```

The key lines:

- `deriveInterfaceCursorBuilder[Character](CharacterType)` derives the interface builder. At query time it dispatches to the concrete branch's builder (`Human` or `Droid`), then wraps the result so the cursor reports `Character` but can `narrow` to the subtype for an `... on Human` fragment.
- `deriveObjectCursorBuilder[Human](HumanType)` and the `Droid` equivalent derive the object builders. Each case-class field name (`id`, `name`, `appearsIn`, …) is matched **structurally** to a GraphQL field of the same name — there is no `@GraphQLField` or any other annotation in this backend.
- The `.transformField("friends")(resolveFriends)` calls are the one customisation here; the next section covers them.

There is no separate registration step: because each builder is an `implicit val` in its companion object, deriving the interface automatically finds the subtype builders, and the subtype builders find the leaf builders for `String`, `Option`, `List`, and `Episode.Value`.

This matches the schema's `interface Character` with `type Human implements Character` and `type Droid implements Character`. Field names and the explicit `tpe` are the only contract; a mismatch between a case-class field and the schema type's fields is not caught until query time.

## Rename a field whose Scala name differs

Because matching is structural, a case-class member must be named exactly like its GraphQL field. When the two differ, fix it on the derived builder rather than renaming your domain type:

```scala
// Inside a `GenericMapping`, with `semiauto._` imported and a `WidgetType` schema.ref:
deriveObjectCursorBuilder[Widget](WidgetType)
  .renameField("internalName", "name")            // one field
  .transformFieldNames {                          // or rewrite all of them
    case "internalName" => "name"
    case other          => other
  }
```

`renameField(from, to)` maps a single Scala field name to a GraphQL field name; `transformFieldNames(f: String => String)` rewrites them in bulk (useful for a `snake_case` ↔ `camelCase` convention).

## Resolve id lists to nested objects with `transformField`

The escape hatch you will reach for most is `transformField[U](name)(f: T => Result[U])`. It replaces a derived field with a computed value — typically resolving a list of foreign-key ids stored in the model into the nested objects the schema actually declares.

In the Star Wars model, `friends` is an `Option[List[String]]` of character ids, but the schema declares `friends: [Character!]`. The `resolveFriends` function (visible in the `#model_types` snip above) bridges the gap, and is attached during derivation:

```scala
deriveObjectCursorBuilder[Human](HumanType).transformField("friends")(resolveFriends)
```

`resolveFriends` has type `Character => Result[Option[List[Character]]]`, looking each id up in `characters`. An unknown id is reported with `toResultOrError`, which yields an [`InternalError`](../how-to/errors.md): unlike a `Failure`, an internal error is raised into the effect `F` rather than surfacing in the response `errors` array, since a dangling id is a bug in your data, not a client-facing problem. The replacement value's type — here `Option[List[Character]]` — must itself have a `CursorBuilder` in scope; it is summoned implicitly from the `Option`, `List`, and `Character` interface builders. `transformField` is also how you break recursion, which is the next recipe.

## Wire the data in with `GenericField`

A derived `CursorBuilder` becomes reachable through a `GenericMapping[F]` (your mapping extends `GenericMapping[F]`, which needs a `MonadThrow[F]`). On the root `Query` `ObjectMapping`, `GenericField(fieldName, value)` exposes an in-memory value at a schema field, picking up the implicit `CursorBuilder` for the value's type:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#root"))
```

Each `GenericField` attaches a list of characters (or just the `Human`/`Droid` subset, via `collect`) to the matching `Query` field. The implicit `CursorBuilder[List[Character]]` (and `List[Human]`, `List[Droid]`) is resolved automatically. A [`SelectElaborator`](../concepts/compiler-elaboration.md) then narrows each list down to the requested element — for example by `id` with `Unique(Filter(Eql(...), child))` — so that `character(id: "1000")` yields a single value rather than the whole list.

With the schema, the model, and these four `GenericField`s in place, the mapping is complete: this query

```graphql
{
  hero(episode: EMPIRE) {
    name
    friends {
      name
    }
  }
}
```

resolves `hero` to Luke Skywalker, then walks his `friends` ids through `resolveFriends` into nested `Character` objects, narrowing each to `Human` or `Droid` as needed.

## Model mutually recursive or graph-shaped data

Two types that reference each other — or a type that references itself — would deadlock if each builder tried to construct the other eagerly. The generic backend avoids this by taking `CursorBuilder` implicits **by name** (`implicit cb: => CursorBuilder[T]`), so a pair of mutually-recursive `implicit val`s can refer to one another without initialization-order errors. The practical pattern is: store the relationship as an **id** in your model and resolve it with `transformField`.

Here `Programme` holds a list of production ids and `Production` holds a programme id:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/RecursionSuite.scala", "#recursion_data"))
```

`Programme.cursorBuilder` derives the object then `transformField("productions")` resolves the id list into `List[Production]`; `Production.cursorBuilder` does the mirror image with `transformField("programme")` resolving a single `Programme`. The two builders refer to each other's types, but because the implicits are by-name, defining both as `implicit val`s in their companions is safe. Storing ids (rather than the related object directly) is what keeps the cycle finite — the resolver runs only when the field is actually selected.

The mapping wires both root fields with `GenericField` and elaborates the `id` argument into a `Unique(Filter(...))`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/RecursionSuite.scala", "#recursion_mapping"))
```

A query may now recurse to any depth — `programmeById` → `productions` → `programme` → `productions` → … — and each step runs its resolver on demand.

## Custom scalars and time and UUID leaves

To serve a custom scalar, give the type a Circe `Encoder` and bind it to the schema's scalar type with `CursorBuilder.deriveLeafCursorBuilder[T](scalarType)`. The generic backend's `ScalarsSuite` does exactly this for a `Genre` ADT and serves `java.time` and `UUID` fields through the implicit `Encoder`-based leaf builder:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/ScalarsSuite.scala", "#generic_scalars"))
```

What to notice:

- `Genre` is a sealed trait, but it is **not** derived as an interface. Its `Encoder[Genre]` maps each case to a `String`, and `deriveLeafCursorBuilder[Genre](GenreType)` makes it a **leaf** reporting the schema's `Genre` scalar. This is the difference between a coproduct served as an `interface` (use `deriveInterfaceCursorBuilder`) and an enum-like ADT served as a `scalar` (use `deriveLeafCursorBuilder`).
- The `Movie` fields `id: UUID`, `releaseDate: LocalDate`, `showTime: LocalTime`, `nextShowing: OffsetDateTime`, and `duration: Duration` need no explicit builder. They each have a Circe `Encoder` (from `io.circe`), so the implicit fallback `leafCursorBuilder` treats them as `String` leaves automatically.
- `deriveObjectCursorBuilder[Movie](MovieType)` then derives the surrounding object as usual, resolving each field's leaf builder from scope.

For the more general story on declaring scalar and enum types in your schema, see [Define custom scalars and enums](../how-to/custom-scalars-enums.md).

## Serve a derived value from an effectful root

`GenericField` is for data already in hand. When the root needs to run an effect in `F` first — read a `Ref`, hit a cache — combine `RootEffect.computeCursor` with the mapping's `genericCursor` helper. `RootEffect.computeCursor(field)((path, env) => F[Result[Cursor]])` runs your effect and yields a cursor; `genericCursor(path, env, value)` builds that cursor for a derived value, deferring construction until the field's context is known:

```scala
// Sketch of an effectful root inside a `GenericMapping[F]` with `semiauto._`,
// a `StructType` schema.ref, and a derived `CursorBuilder[Struct]`:
case class Struct(n: Int, s: String)
object Struct {
  implicit val cb: CursorBuilder[Struct] =
    deriveObjectCursorBuilder[Struct](StructType)
}

def fooField[F[_]: Concurrent](ref: Ref[F, Int]) =
  RootEffect.computeCursor("foo") { (path, env) =>
    ref.update(_ + 1).as(genericCursor(path, env, Struct(42, "hi")))
  }
```

The effect (here bumping a counter) runs once per request; its result, a `Result[Cursor]` from `genericCursor`, plugs the derived `Struct` into the response exactly as a `GenericField` would, but only after the effect has completed. For effect mechanics in general — root effects, nested effects, and batching — see [Run effects and batch nested fields](../how-to/effects-batching.md).

## A note on Scala 2 vs Scala 3

The derivation engine differs by Scala version — shapeless on Scala 2, shapeless3 on Scala 3 — but they live in separate source roots and expose the **identical** public surface: `CursorBuilder`, `GenericMapping`, `GenericField`, `semiauto.deriveObjectCursorBuilder`, `deriveInterfaceCursorBuilder`, and the `ObjectCursorBuilder` combinators. Everything in this page works the same on both. The shapeless-specific implicits behind derivation are internals you never reference directly.

## See also

- [Generic derivation reference](../reference/generic-derivation.md) — exact signatures for `CursorBuilder`, the provided implicit builders, `GenericField`, and the `semiauto` entry points.
- [In-memory Model](../tutorial/in-memory-model.md) — a guided, end-to-end build of the Star Wars mapping shown here.
- [Mappings and cursors](../concepts/mappings-cursors.md) — what a `Cursor` is and how a `Mapping` turns a query into JSON.
- [Define custom scalars and enums](../how-to/custom-scalars-enums.md) — declaring scalar and enum types in the schema that pair with the leaf builders above.
- [Compose multiple mappings (federation)](../how-to/compose-mappings.md) — combine a generic mapping with SQL or circe backends behind one schema.
