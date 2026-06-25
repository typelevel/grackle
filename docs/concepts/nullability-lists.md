# Nullability and lists

The one feature of Grackle's type model most likely to trip you up is *inverted nullability*: in the internal model a bare type is already **non-null**, and optionality is an explicit `NullableType` wrapper — the exact opposite of GraphQL SDL, where `String` is nullable and `String!` is non-null. This page explains why the model is built that way, how the `[T]` / `[T!]` / `[T!]!` list forms translate into nested `ListType` and `NullableType` wrappers, how the parser produces those nestings, and which navigation helpers see *through* the modifiers when you write mappings and elaborators. It assumes you have met the [schema model](schema-model.md) and the `Type` ADT; it is about understanding the mechanism, not a reference of every method.

## Why Grackle inverts SDL nullability

In GraphQL's surface syntax, types are nullable by default and you opt *in* to non-null with a trailing `!`. Grackle's in-memory model flips the default: every named type (`ScalarType`, `EnumType`, `ObjectType`, an interface, a union, or a `TypeRef` to one) is non-null on its own, and you opt *in* to nullability by wrapping it in `NullableType`. So the SDL field `name: String` parses to `NullableType(StringType)`, while `name: String!` parses to a bare `StringType`.

The reason is that the model is consumed far more often than it is written. The interpreter, mappings and elaborators constantly ask "what is the underlying named type here, ignoring optionality and lists?" — and a non-null-by-default representation makes a bare `NamedType` the common, unwrapped case. Wrapping is reserved for the genuinely optional positions, which keeps the type you usually care about at the surface rather than buried under a wrapper. It also means optionality composes by structure: a nullable list of non-null elements and a non-null list of nullable elements are simply different arrangements of the same two wrappers, with no special flags.

This matters because the wrappers are unnamed *modifiers*, not types you look up by name:

```scala
case class ListType(ofType: Type) extends Type
case class NullableType(ofType: Type) extends Type
```

When you write SDL in a schema you always use normal GraphQL — `String`, `String!`, `[Post]`. The inverted representation only surfaces when you inspect the parsed `Type` values, which is exactly what this page is about. (`NullableType` even renders itself with a trailing `?` in `toString`, so `[Post]` prints back as `[Post?]?` — a useful tell that you are looking at the model and not SDL.)

## How `[T]`, `[T!]`, `[T!]!` map to `ListType`/`NullableType`

A GraphQL list type has two independent nullability switches: the list itself, and its elements. Both default to nullable in SDL, and each `!` removes one wrapper. Reading from the outside in, here is how the four combinations of `[Post]` translate, given that `Post` is an object type:

```text
  SDL              internal Type (outermost first)              renders as
  ───────────────  ──────────────────────────────────────────  ──────────
  [Post]           NullableType(ListType(NullableType(Post)))   [Post?]?
  [Post!]          NullableType(ListType(Post))                 [Post]?
  [Post]!          ListType(NullableType(Post))                 [Post?]
  [Post!]!         ListType(Post)                               [Post]
```

The pattern is mechanical: a missing `!` on the list adds an outer `NullableType`; a missing `!` on the element adds an inner `NullableType` around the element type; `ListType` always sits between them. The fully-non-null `[Post!]!` is the only form with no `NullableType` at all — it is just `ListType(Post)`.

Nothing stops these from nesting. `[[Int!]!]` is a non-null list of non-null `Int`s nested inside a nullable outer list, which becomes `NullableType(ListType(ListType(IntType)))` (and renders back as `[[Int]]?`) — the wrappers stack to whatever depth the parser allows.

## The parser's wrapping logic and `maxListTypeDepth`

The translation above is produced by `SchemaParser.mkType`. Its inner recursive helper, `loop`, walks the untyped `Ast.Type` and threads a single `nullable` flag. The flag starts `true` (SDL's default), each `Ast.Type.NonNull` clears it for the type it guards, and a small `wrap` helper applies a `NullableType` only when the flag is still set:

```scala
def loop(tpe: Ast.Type, nullable: Boolean): Result[Type] = {
  def wrap(tpe: Type): Type = if (nullable) NullableType(tpe) else tpe

  tpe match {
    case Ast.Type.List(tpe)         => loop(tpe, true).map(tpe => wrap(ListType(tpe)))
    case Ast.Type.NonNull(Left(tpe))  => loop(tpe, false)
    case Ast.Type.NonNull(Right(tpe)) => loop(tpe, false)
    case Ast.Type.Named(Name(nme))  =>
      wrap(ScalarType.builtIn(nme).getOrElse(lazyRef(schema, nme))).success
  }
}

loop(tpe, true)   // SDL default: nullable
```

Two details make the inversion fall out. First, a list re-enters `loop` with `nullable = true`, so the *element* type defaults to nullable again, independently of the list's own nullability. Second, a named type resolves to a built-in `ScalarType` if its name is one of the five built-ins, otherwise to a by-name `TypeRef` (`lazyRef`) so mutually recursive schemas work; the `wrap` is applied to whichever it is.

List nesting is bounded. The parser only accepts list modifiers up to `maxListTypeDepth` deep, which defaults to `5` in `GraphQLParser.defaultConfig`; exceeding it is a parse error ("exceeded maximum list type depth"). If you genuinely need deeper nesting, build a `GraphQLParser` with a custom `Config` and feed it to `SchemaParser` (see [parsing SDL at runtime](../how-to/validate-mappings.md)). In practice the limit is generous — real schemas rarely nest lists beyond one or two levels.

## Navigation helpers that see through the modifiers

Because optionality and lists are wrappers rather than fields, the `Type` ADT carries helpers that test or strip them. The important thing to internalise is that **the predicates inspect only the outermost wrapper**, while the *underlying* helpers strip every wrapper (and every `TypeRef` alias) down to a named type:

- `isNullable` / `isList` — `true` only if the *outermost* constructor is `NullableType` / `ListType`. They do not look inside.
- `nullable` / `nonNull` — add or remove the outer `NullableType` (`nonNull` strips all leading nullables).
- `item` — for a list, the element type (unwrapping a leading `NullableType` first); otherwise `None`.
- `list` — wrap as a list, preserving a leading nullable.
- `underlying`, `underlyingNamed`, `underlyingObject` — strip all aliases, nullability and list wrappers; `underlyingNamed` always reaches a `NamedType`, while `underlyingObject` yields the object/interface/union or `None` for a leaf.

The consequence of "outermost only" is the most common surprise: a nullable list answers `isList` with `false`, because its outer wrapper is the `NullableType`. The following block inspects a small schema and shows exactly what each helper returns. It compiles and runs at site build time, so the printed values are the real ones.

```scala mdoc:silent
import grackle.syntax._

val schema =
  schema"""
    type Query {
      posts: [Post]
    }
    type Post {
      id: Int!
      title: String
      tags: [String!]!
    }
  """

val QueryType = schema.ref("Query")
val PostType  = schema.ref("Post")

// `field` dealiases the TypeRef for you, so no explicit `.dealias` is needed.
val postsTpe = QueryType.field("posts").get  // [Post]     -> [Post?]?
val tagsTpe  = PostType.field("tags").get    // [String!]! -> [String]
```

Now read the wrappers off those field types. `id: Int!` is non-null, `title: String` is nullable, and the two list fields differ only in their `!`s:

```scala mdoc
PostType.field("id").get.isNullable     // Int!   -> false
PostType.field("title").get.isNullable  // String -> true
postsTpe.isNullable                     // [Post] is a NULLABLE list -> true
postsTpe.isList                         // outermost is NullableType   -> false
postsTpe.nonNull.isList                 // strip the nullable, now a list -> true
postsTpe.item                           // the element type, Post? -> Some(Post?)
tagsTpe.isList                          // [String!]! has no outer nullable -> true
tagsTpe.item                            // element is non-null String -> Some(String)
```

There is also a *path* family for navigating a chain of field names at once. `path(fieldNames)` returns the `Type` reached by following the names, transparently passing through list and nullable wrappers along the way; `pathIsList` and `pathIsNullable` answer whether the path *passes through* a list or a nullable field at any hop. That "any hop" semantics is deliberate and differs from the single-wrapper predicates above:

```scala mdoc
QueryType.path(List("posts", "title"))            // sees through [Post?]? -> Some(String?)
QueryType.pathIsList(List("posts", "title"))      // passes through the posts list -> true
QueryType.pathIsNullable(List("posts", "id"))     // passes through nullable posts -> true (even though id is Int!)
```

`pathIsNullable(List("posts", "id"))` is `true` even though `id` is `Int!`, because the route to it goes through the *nullable* `posts` field — if `posts` resolves to `null` there is no `id` to speak of. This is the right question to ask when you are deciding whether a whole projected path can be absent.

## Implications for writing mappings and elaborators

The inverted model is something you read constantly and rarely write, so the practical advice is about reading it correctly:

- **Reach for the `underlying*` helpers, not manual unwrapping.** When you need the object behind a field — to look up its mapping, or to build a `Context` — use `underlyingObject` / `underlyingNamed` rather than pattern-matching on `NullableType`/`ListType` yourself. They handle nullable-of-list-of-nullable and `TypeRef` aliasing in one call, so your code does not break when someone adds or removes a `!`.
- **Don't read `isList` on a nullable field.** A `[T]` field is `isList == false` because its outer wrapper is the nullable. Call `.nonNull.isList`, or use `pathIsList`, when the field might be optional.
- **The interpreter dispatches on the wrappers in order.** During execution the [query interpreter](query-interpreter.md) peels the type one layer at a time: a `NullableType` drives `Cursor.asNullable` (a missing value becomes JSON `null`), a `ListType` drives `asList`, and the underlying named type drives leaf or object handling. The order of your wrappers is therefore exactly the order the result JSON is shaped, which is why the structural representation — rather than a pair of boolean flags — is what the model uses.
- **Match on the *underlying* type in elaborators.** A `SelectElaborator` keys on `(TypeRef, fieldName, args)`; the `TypeRef` it sees is the underlying named type, already stripped of list and nullable wrappers. You build path predicates with `/` (for example `PostType / "id"`), and that operator likewise navigates through modifiers. You almost never need to mention `NullableType` or `ListType` by name in mapping code — that is the payoff of the non-null-by-default design.

## See also

- [The schema model](schema-model.md) — the full `Type` ADT, `NamedType` hierarchy, `TypeRef`, and `=:=`/`<:<`.
- [The GraphQL schema and SDL reference](../reference/schema-sdl.md) — every type, modifier and operator in table form, including the navigation helpers above.
- [How the query interpreter works](query-interpreter.md) — how `asNullable` and `asList` peel these wrappers during execution.
- [Cursor reference](../reference/cursor.md) — the `asNullable` / `asList` navigation methods the interpreter calls.
- [Interfaces and unions](../how-to/interfaces-unions.md) — defining the object/abstract types these modifiers wrap.
