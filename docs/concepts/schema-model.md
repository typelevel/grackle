# The schema model

A `Schema` is Grackle's in-memory representation of a GraphQL schema: an immutable collection of named type declarations, directive definitions and extensions. It is the ground truth that every other layer consults — mappings attach data to its types, the compiler type-checks queries against it, and the interpreter walks it to assemble results. This page explains how that model is shaped, how its types refer to and compare with one another, and how SDL text becomes a `Schema`. It is for developers working with the `Schema` API directly; for the exhaustive list of methods and constructors see the [Schema & SDL reference](../reference/schema-sdl.md).

## A schema is a collection of named types

At its core a `Schema` holds a list of `NamedType` declarations plus the directive definitions and extensions that apply to them. Every GraphQL type kind is a distinct case class: `ScalarType`, `EnumType`, `InterfaceType`, `ObjectType`, `UnionType` and `InputObjectType`. You rarely build these by hand — you write SDL and let Grackle parse it. Here is a real schema (from the World demo) defined with the compile-time-validated `schema"..."` interpolator:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#schema"))
```

`schema"..."` comes from `import grackle.syntax._`. The text is parsed and fully validated *at compile time*, and the interpolator yields a bare `Schema`. The result is four `ObjectType`s — `Query`, `Country`, `City`, `Language` — each carrying a list of `Field`s, and each field carrying its argument list (`InputValue`s), result `Type` and any directives. The `Query` type is special only by name: it is picked up as the query root (see [Root operation types](#root-operation-types-and-the-schema-type) below).

You read the model back through the schema's lookup helpers. `schema.types` is the full list of named types (with any extensions merged in), `schema.definition(name)` looks one up by name, and `schema.directives` lists the directive definitions. Two things are worth knowing up front. First, the five built-in directive definitions (`@skip`, `@include`, `@deprecated`, `@specifiedBy`, `@oneOf`) are silently appended to every parsed schema, so `schema.directives` always contains them even when your SDL declares none. Second, the five built-in scalar names — `Int`, `Float`, `String`, `Boolean`, `ID` — are always available without being declared.

## Type vs NamedType, and TypeRef

`Type` is the root sealed trait of the type hierarchy and carries all the navigation operators (nullability, lists, field lookup, subtyping). `NamedType` is the subset of types that have a schema-defined `name`: the six declared kinds above, plus one more — `TypeRef`.

`ListType` and `NullableType` are *not* named types. They are unnamed *modifiers* that wrap another `Type`, and they are how Grackle models lists and optionality. Because Grackle's model is non-null by default, a bare `ScalarType` or `ObjectType` is already non-null, and optionality is the explicit `NullableType` wrapper — the inverse of SDL surface syntax, where `String` is nullable and `String!` is not. That inversion has its own page; see [Nullability and lists](nullability-lists.md).

A `TypeRef` is a lazy, by-name reference to a type defined in a schema:

```scala
case class TypeRef private[grackle] (schema: Schema, name: String) extends NamedType {
  override lazy val dealias: NamedType = schema.definition(name).getOrElse(this)
  override lazy val exists: Boolean    = schema.definition(name).isDefined
}
```

`TypeRef` solves two problems. The first is **mutual recursion**: in the World schema, `Country.cities` refers to `City` and `City.country` refers back to `Country`. A `TypeRef` lets a field name its result type without the target having to exist yet — the reference is resolved lazily through `dealias` when you actually navigate it.

The second is **value-equality**. `TypeRef` is the only type representation that compares meaningfully with `==`, because two `TypeRef`s for the same name in the same schema are equal by case-class structure. This is exactly what elaborators and mappings rely on when they pattern-match on a type. You obtain one with `schema.ref(name)`:

```scala mdoc:silent
import grackle.Schema
import grackle.syntax._

val schema: Schema =
  schema"""
    type Query {
      hero(episode: Episode!): Character!
      character(id: ID!): Character
    }
    enum Episode {
      NEWHOPE
      EMPIRE
      JEDI
    }
    interface Character {
      id: String!
      name: String
      friends: [Character!]
    }
    type Human implements Character {
      id: String!
      name: String
      friends: [Character!]
      homePlanet: String
    }
  """

val QueryType     = schema.ref("Query")
val CharacterType = schema.ref("Character")
```

`schema.ref(name)` is *checked*: it throws `IllegalArgumentException` if the name is not defined in the schema, so a `TypeRef` you get this way always points somewhere. There is also `schema.uncheckedRef(name)`, which skips the check and can produce a dangling reference whose `.exists` is `false` and whose navigation returns `None`. Note that `TypeRef`'s constructor is `private[grackle]` — you cannot `new TypeRef(...)`; always go through `schema.ref` / `schema.uncheckedRef`.

## Comparing types: `=:=`, `<:<`, `==` and `dealias`

Because a type can appear either directly or behind a `TypeRef` alias, plain `==` is usually the wrong comparison: it distinguishes a type from an alias to that same type. Grackle provides alias-aware operators instead.

`a =:= b` is **type equivalence** — equal after both sides are dealiased:

```scala
def =:=(other: Type): Boolean = (this eq other) || (dealias == other.dealias)
```

`a <:< b` is the **subtype relation**. An object type is a subtype of every interface it implements, a member type is a subtype of any union it belongs to, and the modifiers are covariant: `NullableType` and `ListType` propagate `<:<` to their element types, and a non-null type is a subtype of its nullable form.

`dealias` resolves a `TypeRef` to its target named type, looking it up by name in the schema (and leaving the reference untouched if the target is undefined); on any other type it is the identity. There is also `nominal_=:=`, which compares two types purely by their underlying name.

The practical rule: compare with `=:=`, not `==`. The one deliberate exception is `TypeRef`s obtained from the *same* schema via `schema.ref` — those are designed to be `==`-comparable, which is the whole point of using them in elaborator pattern matches. The [compiler and elaboration](compiler-elaboration.md) page shows that comparison in action.

```scala mdoc
// Same name, alias-aware equivalence:
CharacterType =:= schema.ref("Character")

// Human is a subtype of the Character interface it implements:
schema.ref("Human") <:< CharacterType
```

## Root operation types and the schema type

GraphQL groups its three root operation types under a `schema { ... }` block. Grackle models that block as a synthetic named type — the *schema type* — exposed as `schema.schemaType`. If your SDL contains no explicit `schema { ... }` block (as the schemas above do not), Grackle synthesises a default one referencing the types named `Query`, `Mutation` and `Subscription`, including only those that actually exist:

```text
type Schema {
  query: Query!
  mutation: Mutation        # only if a type named Mutation exists
  subscription: Subscription # only if a type named Subscription exists
}
```

From that, `schema.queryType` returns the query root, while `schema.mutationType` and `schema.subscriptionType` return `Option`s. There is an important asymmetry: `queryType` is non-optional and calls `.get` internally, so a schema with *no* query root throws `NoSuchElementException`. The mutation and subscription accessors are safe `Option`s. `schema.isRootType(tpe)` tells you whether a given type is one of the three roots.

You can also declare the schema type explicitly when you want non-default names, by writing a `schema { query: ... }` block in your SDL; Grackle then uses that instead of the synthesised default.

## Type extensions

GraphQL's `extend` keyword lets you add to a type declared elsewhere — `extend type Query { ... }`, `extend interface ...`, `extend enum ...`, and so on, plus `extend schema`. Grackle parses these into `TypeExtension` and `SchemaExtension` values stored *separately* from the base types, on `schema.typeExtensions` and `schema.schemaExtensions`.

The merge is lazy and happens at read time. `schema.types` returns `baseTypes` unchanged when there are no extensions, and otherwise maps each base type through its applicable extensions; `schema.schemaType` merges schema extensions onto the base schema type the same way. So extensions never mutate the base declarations — they are folded in when you ask for the resolved view. Extensions are validated too: you cannot apply an object extension to a non-object, or extend a type that does not exist.

## Compile-time `schema"..."` vs runtime `Schema(text)`

There are two ways to turn SDL into a `Schema`, and choosing correctly matters.

The `schema"..."` interpolator validates **at compile time** and expands to roughly `Schema(s, ...).toOption.get`, yielding a *bare* `Schema`. An invalid schema is a compile error (the message is prefixed with `Invalid schema:`), so by the time your program runs the schema is known good. Use it whenever the SDL is a literal in your source.

`Schema(text)` is the **runtime** factory. It returns a `Result[Schema]` — Grackle's success/failure type — so you use it when the SDL is not known until runtime, or when you want to inspect validation problems yourself rather than fail the build. Validation errors accumulate as `Problem`s inside a `Result.Failure`:

```scala mdoc
import grackle.Result

val parsed: Result[Schema] =
  Schema(
    """
      type Query {
        episodeById(id: String!): Episod
      }
      type Episode {
        id: String!
      }
    """
  )

parsed match {
  case Result.Failure(ps) => ps.map(_.message).toChain.toList
  case Result.Success(s)  => s.types.map(_.name)
  case other              => List(other.toString)
}
```

The deliberate typo `Episod` (the type is named `Episode`) is caught by the schema validator and surfaces as `Reference to undefined type 'Episod'`. Note that these are GraphQL `Problem`s, not exceptions: `Result.Failure` carries a `NonEmptyChain[Problem]`, and the same `Result` type threads through the rest of Grackle. (`Result` also has `Warning` and `InternalError` cases; only `Failure` and `Warning` carry problems — see [The compiler and elaboration](compiler-elaboration.md) and the errors reference.)

Validation is thorough. On construction Grackle checks references to undefined types, duplicate definitions, union members that are not object types, interface conformance (a type must declare every field of every interface it implements, with a subtype-compatible result type and matching arguments) and transitive-interface obligations, interface cycles, extension target mismatches, and directive validity. All of these accumulate into the `Problem` chain rather than stopping at the first error.

Finally, the model round-trips: `schema.toString` renders a `Schema` back to SDL via `SchemaRenderer`. The built-in directive definitions are deliberately omitted from that rendering, so a round-tripped schema reads exactly like what you wrote, even though `schema.directives` still contains them.

## See also

- [Nullability and lists](nullability-lists.md) — how `[T]`, `[T!]` and `[T!]!` map to `ListType`/`NullableType`, and why the model inverts SDL.
- [The compiler and elaboration](compiler-elaboration.md) — how queries are type-checked and rewritten against this model.
- [Mappings and cursors](mappings-cursors.md) — how a `Mapping` attaches data to the schema's types.
- [Schema & SDL reference](../reference/schema-sdl.md) — the full `Schema`/`Type` API, constructors and operators.
- [Define custom scalars and enums](../how-to/custom-scalars-enums.md) and [Define and use schema directives](../how-to/schema-directives.md) — task recipes built on this model.
