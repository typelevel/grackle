# Quick start: your first query

This tutorial walks you from an empty file to a running GraphQL query, entirely in memory and with no
database. You will define a tiny two-type schema, back it with an ordinary Scala `List`, wire the two
together with a [`ValueMapping`](../reference/mapping-types.md), teach Grackle how to interpret one field
argument, and run a query to see the JSON response. It is aimed at first-time users who want something
working in a few minutes; every step is real, compiled code that you can copy and run. For the larger,
multi-type Star Wars example, see the [in-memory tutorial](../tutorial/in-memory-model.md).

## Add the dependency

Add `grackle-core` to your build. That single module gives you the schema parser, the query
compiler/interpreter, and the in-memory `ValueMapping` used here.

```scala
libraryDependencies += "org.typelevel" %% "grackle-core" % "@VERSION@"
```

See [Installation](install.md) for the other backend modules (circe, generic, doobie, skunk) and for
cross-build details.

## The imports

A core mapping pulls names from a handful of packages. The query algebra (`Filter`, `Unique`),
predicates (`Eql`, `Const`), GraphQL argument values (`IntValue`), and the compiler's elaboration
helpers each live in their own object, and `grackle.syntax._` brings in the `schema"..."` interpolator.

```scala
import cats.effect.IO

import grackle._
import grackle.Predicate._
import grackle.Query._
import grackle.QueryCompiler._
import grackle.Value._
import grackle.syntax._
```

## The whole mapping

Here is the complete, compiled mapping. Read it top to bottom — the sections below unpack each part —
then we will run a query against it.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/docs/src/main/scala/grackle/QuickStartMapping.scala", "#quickstart"))
```

### The data

The `Book` case class and the `books` list are plain Scala with no Grackle dependency. This is the point
of a [`ValueMapping`](../reference/mapping-types.md): your domain values stay ordinary, and the mapping
is the only place that knows about GraphQL. Anything you can hold in a Scala value — a `List`, a `Map`,
the result of a prior computation — can back a `ValueMapping`.

### The schema

The schema is written with the `schema"""..."""` interpolator from `grackle.syntax._`, which **validates
the SDL at compile time** and yields a bare `Schema`. (If you instead need to build a schema from text at
runtime — say, loaded from a file — use the `Schema(text)` factory, which returns a `Result[Schema]` you
can handle for parse errors.) Note the GraphQL nullability markers: `[Book!]!` is a non-null list of
non-null `Book`s, while `book(id: Int!): Book` returns a nullable `Book` (there may be no book with that
id) taking a non-null `Int!` argument.

`schema.ref("Query")` and `schema.ref("Book")` produce `TypeRef`s — lightweight, named references into
the schema. You use them both when declaring mappings and, later, when matching on a type in the
elaborator.

### The type mappings

`typeMappings` is the catalog that tells Grackle how to serve each field. There is one
`ValueObjectMapping` per object type:

- `ValueObjectMapping[Unit](tpe = QueryType, ...)` maps the root `Query` type. Its focus value is `Unit`
  because the root has no parent value, so each `ValueField` ignores its argument (`_ => books`) and just
  returns the list. Both `books` and `book` start from the same `books` list — the difference is that
  `book` will be narrowed to a single element by the elaborator below.
- `ValueObjectMapping[Book](tpe = BookType, ...)` maps the `Book` type. Here the focus value is a `Book`,
  so each `ValueField` projects one field out of it: `ValueField("title", _.title)` reads `book.title`.

The type parameter on `ValueObjectMapping[Book]` matters: Grackle uses it at runtime to decide whether a
focus value belongs to this mapping, so supply the concrete element type rather than leaving it inferred.
You declare a field mapping for **every** field in the schema; a declared field with no mapping is a
validation error. You do not, however, declare mappings for the built-in scalars (`Int`, `String`, …) —
those leaf mappings are supplied automatically.

> Note: writing `val typeMappings = List(...)` works because a `List` of mappings is implicitly converted
> to the `TypeMappings` catalog. That catalog is validated lazily the first time a query is compiled, so a
> missing or mismatched mapping surfaces on the first run rather than at construction.

### The elaborator

By default a field's arguments are simply ignored by the interpreter. To make `book(id: ...)` actually
select a book, you override `selectElaborator` — the per-mapping phase that rewrites the parsed query
before it is interpreted. See [the compiler and elaboration](../concepts/compiler-elaboration.md) for the
full pipeline; here one case is enough:

```scala
case (QueryType, "book", List(Binding("id", IntValue(id)))) =>
  Elab.transformChild(child => Unique(Filter(Eql(BookType / "id", Const(id)), child)))
```

This matches the `book` field on `QueryType` whose `id` argument is an `IntValue`, binds the integer as
`id`, and rewrites the child query. `Filter(Eql(BookType / "id", Const(id)), child)` keeps only the books
whose `id` field equals the supplied value (`BookType / "id"` is a typed path to that field), and `Unique`
asserts the result is a single element and unwraps the list into one object — exactly the shape the
nullable `book: Book` field promises. The same `Filter`/`Eql` machinery scales up to sorting and paging,
covered in [Filter, sort and page a field](../how-to/filtering-ordering-paging.md).

## Run a query

`ValueMapping` extends `Mapping[IO]`, so the mapping object exposes `compileAndRun`, which parses,
type-checks, and elaborates the query text, then interprets it and renders the JSON response. It returns
an `IO[Json]`; run it with cats-effect's runtime to get the value.

Ask for one book by id:

```scala mdoc:silent
import grackle.docs.QuickStartMapping
import cats.effect.unsafe.implicits.global

val query = """
  query {
    book(id: 2) {
      title
      author
    }
  }
"""

val response = QuickStartMapping.compileAndRun(query).unsafeRunSync()
```

The elaborator turned `book(id: 2)` into a filter for `id == 2` followed by `Unique`, so the response is
a single object wrapped in the standard `data` envelope (this JSON is the real value `response` holds,
rendered at build time):

```scala mdoc:passthrough
println("```json\n" + response.spaces2 + "\n```")
```

The `books` field needs no argument and no elaboration — it returns the whole list:

```scala mdoc:silent
val all = QuickStartMapping.compileAndRun("query { books { id title } }").unsafeRunSync()
```

```scala mdoc:passthrough
println("```json\n" + all.spaces2 + "\n```")
```

That is a complete Grackle server: a schema, a mapping over in-memory data, one elaboration rule, and a
call to `compileAndRun`. If a query is invalid — an unknown field, a wrong argument type — the failure is
reported as a `Problem` in the response's `errors` array rather than thrown; see
[Reporting errors](../how-to/errors.md) for how `Result` and `Problem` work.

## Next steps

- [In-memory model](../tutorial/in-memory-model.md) — the full Star Wars tutorial: interfaces, enums,
  nested relationships, and a richer elaborator.
- [Filter, sort and page a field](../how-to/filtering-ordering-paging.md) — go beyond a single `Eql`
  predicate to filtering, ordering, and paging.
- [The compiler and elaboration](../concepts/compiler-elaboration.md) — what `selectElaborator` does
  inside the query pipeline.
- [Mappings and cursors](../concepts/mappings-cursors.md) — how a mapping produces the `Cursor`s the
  interpreter walks to build the response.
- [Mapping types reference](../reference/mapping-types.md) — the catalog of mappings (`ValueMapping`,
  `CirceMapping`, `GenericMapping`, `SqlMapping`) and which to reach for.
