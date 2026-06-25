# Define custom scalars and enums

GraphQL gives you five built-in scalars (`Int`, `Float`, `String`, `Boolean`, `ID`), but real domains need more: a `UUID`, a `Date`, a duration `Interval`, an enumerated `Genre`. This page shows you how to declare such types in your schema and wire them to Scala types. It is for developers who already have domain scalars (UUIDs, dates, times, durations) and want them to appear in their GraphQL API. You will declare the types in SDL, encode their results with [`LeafMapping`](../reference/mapping-types.md), decode them when they arrive as arguments with `Value` extractors, and (optionally) point at their specification with `@specifiedBy`.

## Declare the scalar and enum in SDL

Custom scalars are declared with `scalar Name`; enums with `enum Name { ... }`. The example mapping below defines five custom scalars and one enum, then uses them throughout a `Movie` type and its query arguments.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ScalarsSuite.scala", "#scalars_schema"))
```

The SDL here is ordinary GraphQL. Each `scalar UUID` line introduces a name; each enum lists its allowed value names (`DRAMA`, `ACTION`, `COMEDY`). Note that scalar names are case-sensitive and validated at parse time: referencing `Date` in a field without a matching `scalar Date` declaration is a `Reference to undefined type` error. The five built-in scalar names are always available without declaration.

Because this schema is written with the `schema"""..."""` interpolator (from `grackle.syntax._`), it is parsed and fully validated at **compile time** and yields a bare `Schema`. The runtime factory `Schema(text)` returns a `Result[Schema]` instead — use that when the SDL comes from a file or config. See [the schema model](../concepts/schema-model.md) for the distinction.

## Why the schema only records the name

A `scalar UUID` declaration produces a `ScalarType` whose `isBuiltIn` is `false`. That is *all* the schema layer knows about it: the name. The schema does not know that `UUID` should become a `java.util.UUID`, nor how to serialise or parse one. Defining `scalar UUID` on its own does nothing at runtime — values pass through unchanged until you supply the encoding and decoding in the mapping layer.

This separation is deliberate. The schema describes the *shape* of your API; the [mapping](../concepts/mappings-cursors.md) describes how that shape is backed by data. The next two sections supply the two halves: encoding results, and decoding arguments.

## Encode results with `LeafMapping`

A `LeafMapping[T]` ties a scalar or enum type in the schema to a Scala type `T`, using an implicit circe `Encoder[T]` to turn `T` into JSON in the response. There is no separate `PrimitiveMapping`; `LeafMapping` is the single construct for custom scalars and enums. (The built-in `String`/`Int`/`Float`/`Boolean`/`ID` leaf mappings are added for you automatically, so you only declare your own.)

You declare one `LeafMapping` per custom type, alongside your object mappings, in the `typeMappings` catalog:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ScalarsSuite.scala", "#scalars_leafmappings"))
```

Each `LeafMapping[T](tpe)` takes the schema type reference (`UUIDType`, `GenreType`, …, obtained with `schema.ref("UUID")`) and requires an implicit `Encoder[T]` in scope. For `UUID`, `LocalDate`, `LocalTime`, `ZonedDateTime` and `Duration`, circe's standard library encoders apply. For the domain `Genre` enum there is no library encoder, so the mapping supplies one explicitly:

```scala mdoc:silent
import io.circe.Encoder

sealed trait Genre
object Genre {
  case object Drama  extends Genre
  case object Action extends Genre
  case object Comedy extends Genre
}

implicit val genreEncoder: Encoder[Genre] =
  Encoder[String].contramap {
    case Genre.Drama  => "DRAMA"
    case Genre.Action => "ACTION"
    case Genre.Comedy => "COMEDY"
  }
```

The encoder maps each ADT case to the exact value name declared in the enum. With these `LeafMapping`s in place, a `Movie`'s `genre` and `releaseDate` fields render as the enum value name and an ISO date string in the response JSON.

## Decode custom-scalar arguments with `Value` extractors

Encoding handles *output*. For *input* — a custom scalar or enum used as a field argument — you decode the incoming `Value` during elaboration. Grackle delivers argument values as the `Value` ADT (`StringValue`, `IntValue`, `EnumValue`, …); a custom scalar arrives as whatever literal the client wrote (typically a `StringValue`), and an enum arrives as an `EnumValue`. You convert these to your domain type with small extractor objects:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ScalarsSuite.scala", "#scalars_values"))
```

Each object defines an `unapply` that pattern-matches the raw `Value` and returns an `Option` of the domain type — `UUIDValue` parses a `StringValue` into a `UUID`, `GenreValue` turns an `EnumValue`'s `name` back into a `Genre`. You then use them as patterns inside your [`SelectElaborator`](../concepts/mappings-cursors.md), binding the parsed value into the query algebra:

```scala
override val selectElaborator = SelectElaborator {
  case (QueryType, "movieById", List(Binding("id", UUIDValue(id)))) =>
    Elab.transformChild(child => Unique(Filter(Eql(MovieType / "id", Const(id)), child)))
  case (QueryType, "moviesByGenre", List(Binding("genre", GenreValue(genre)))) =>
    Elab.transformChild(child => Filter(Eql(MovieType / "genre", Const(genre)), child))
}
```

A non-built-in scalar passes argument type-checking unchanged, so the raw literal reaches the elaborator as-is. If an extractor then returns `None` (an unparseable UUID, an unknown enum name) the `case` does not match; `SelectElaborator` falls through to `Elab.unit` and leaves the query untransformed, so the argument silently has no effect. To reject a malformed value, add a fallback `case` for that field that fails explicitly — for example with `Elab.failure(...)` — rather than relying on the extractor. For more on turning arguments into predicates, see [filtering, ordering and paging](filtering-ordering-paging.md).

## Document a scalar with `@specifiedBy`

The GraphQL built-in directive `@specifiedBy(url: "...")` records, in the schema itself, where a custom scalar's format is specified. Grackle parses it and exposes the URL through `ScalarType.specifiedByURL`. The following block parses a one-scalar schema at runtime with `Schema(text)` (hence the `Result[Schema]`), looks the scalar up by name, and reads the directive back:

```scala mdoc
import grackle._

val rfcSchema =
  Schema("""
    type Query { now: DateTime }
    scalar DateTime @specifiedBy(url: "https://scalars.graphql.org/andimarek/date-time")
  """)

val dateTimeUrl =
  rfcSchema.toOption.flatMap(_.definition("DateTime")).flatMap {
    case st: ScalarType => st.specifiedByURL
    case _              => None
  }
```

`specifiedByURL` returns an `Option[String]` — the URL when the directive is present, `None` otherwise. The five built-in scalars carry no `@specifiedBy` directive. The directive is metadata only: it has no effect on encoding or decoding, but tools and clients can surface it through introspection.

## Generic backend: derive a leaf cursor builder

When you use the [generic backend](generic-derivation.md) (`GenericMapping`), you map a Scala value tree to the schema with `CursorBuilder` instances instead of an explicit `typeMappings` catalog of leaf mappings. A custom scalar or enum then needs a *leaf* cursor builder, which you obtain with `CursorBuilder.deriveLeafCursorBuilder[T](tpe)`. As with `LeafMapping`, it requires an implicit circe `Encoder[T]`.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/generic/src/test/scala/ScalarsSuite.scala", "#generic_scalars"))
```

The key line is the `CursorBuilder[Genre]`, bound to the schema's `GenreType` via `deriveLeafCursorBuilder[Genre](GenreType)`. It uses the `genreEncoder` defined just above it, so the enum renders as its value name. The java-time and `UUID` fields of `Movie` (`releaseDate: LocalDate`, `nextShowing: OffsetDateTime`, `duration: Duration`, `id: UUID`) need no per-field wiring: the generic backend's implicit `leafCursorBuilder` derives a leaf builder for any type with a circe `Encoder` in scope. Note that `deriveObjectCursorBuilder[Movie](MovieType)` builds the surrounding object cursor that ties those leaves together.

## Mapping enum value names to a domain ADT

Across both backends the enum recipe is the same, and is worth stating on its own. A `enum Genre { DRAMA ACTION COMEDY }` declaration produces an `EnumType` that knows only the value names. To connect those names to a sealed ADT you provide two total, mutually-inverse mappings:

- **Decode** (name to ADT), used for arguments — the `fromString` helper behind `GenreValue.unapply`:

```scala
def fromString(s: String): Option[Genre] =
  s.trim.toUpperCase match {
    case "DRAMA"  => Some(Genre.Drama)
    case "ACTION" => Some(Genre.Action)
    case "COMEDY" => Some(Genre.Comedy)
    case _        => None
  }
```

- **Encode** (ADT to name), used for results — the `Encoder[Genre]` shown earlier.

Keep the string literals in both directions identical to the value names in the SDL. Decoding returns an `Option` so an unknown name fails cleanly rather than throwing; encoding is total because every ADT case has a declared name.

## See also

- [Reference: mapping types](../reference/mapping-types.md) — the full `LeafMapping` and field-mapping catalog.
- [Reference: schema and SDL](../reference/schema-sdl.md) — `ScalarType`, `EnumType`, and how SDL is parsed.
- [Concept: the schema model](../concepts/schema-model.md) — why the schema records names, not codecs.
- [Concept: mappings and cursors](../concepts/mappings-cursors.md) — how `LeafMapping`, elaborators and cursors fit together.
- [How-to: filtering, ordering and paging](filtering-ordering-paging.md) — more on turning arguments into predicates.
- [How-to: generic derivation](generic-derivation.md) — the `CursorBuilder` backend in full.
