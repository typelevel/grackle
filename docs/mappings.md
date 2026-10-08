# Mapping Concepts

A `Mapping` is how Grackle connects a GraphQL schema to whatever actually holds our data: a
Scala collection, a JSON document, a SQL database, a remote API, or some combination of these.
Grackle ships with several base classes to choose from, and a family of smaller building blocks
that plug into any of them. This page is a guided tour of both, with an eye toward answering the
question every new user has: *given my data source, which pieces do I actually need?*

It is worth reading through the [tutorial](tutorial/intro.md) first: it walks through two
complete, running examples (an in-memory model and a database-backed model). This page fills in
the conceptual gaps between them and covers the pieces the tutorial does not reach.

## How a query flows through Grackle

Before getting into the different kinds of `Mapping`, it helps to see the pipeline they all plug
into: Grackle is a compiler/interpreter, and every `Mapping`, however it is built, is just a set
of extension points feeding the same parse/compile/interpret/complete pipeline. See
[How a query flows through Grackle](pipeline.md) for a diagram of the stages, including exactly
when and where `Env` gets created and merged in, and
[Compiler Phases](howto/compiler-phases.md) for how to add a phase of our own.

## The shape of every Mapping

Whichever base class we pick, every `Mapping` has the same two moving parts:

```scala
abstract class Mapping[F[_]] {
  val schema: Schema
  val typeMappings: TypeMappings
}
```

`typeMappings` is a list of `TypeMapping`s, one per GraphQL named type. Most of them are
`ObjectMapping`s, and each `ObjectMapping` carries a list of `FieldMapping`s, one per field of
that type. Scalars and enums get a `LeafMapping` instead, which just needs a `circe.Encoder`
(builtins like `Int`, `String`, `Boolean` are supplied for us; we only add a `LeafMapping` for
our *own* custom scalars).

```scala
TypeMappings(
  ObjectMapping(QueryType)(
    /* one FieldMapping per top level query field */
  ),
  ObjectMapping(SomeType)(
    /* one FieldMapping per field of SomeType */
  )
)
```

So choosing how to map our API really comes down to two independent questions:

1. **Which base `Mapping` class matches the shape of my data source?** This determines how
   *object types* turn into cursors.
2. **Which kind of `FieldMapping` does each individual field need?** This is largely independent
   of (1): `CursorField`, `EffectField`, `RootEffect`, `RootStream` and `Delegate` are available
   no matter which base class we are using.

## Picking a base Mapping

| The data looks like...                                    | Use              | Module            |
|-------------------------------------------------------------|-------------------|--------------------|
| A handful of values, wired up by hand                      | `ValueMapping`     | `grackle-core`     |
| Plain Scala case classes/ADTs that mirror the schema        | `GenericMapping`   | `grackle-generic`  |
| Data already held as `io.circe.Json`                       | `CirceMapping`     | `grackle-circe`    |
| Rows in a relational database                               | `SqlMapping`       | `grackle-doobie-*` / `grackle-skunk` |
| Several of the above, combined under one schema             | `ComposedMapping`  | `grackle-core`     |

### `ValueMapping` - plain Scala values, wired up explicitly

`ValueMapping` is the simplest possible backend: the "cursor" is just whatever Scala value is
currently in focus, and we tell Grackle exactly how to get from a parent value to each child
value with `ValueField`.

```scala
class Countries[F[_]: MonadThrow] extends ValueMapping[F] {
  val schema = schema"""
    type Query { countries: [Country!]! }
    type Country { name: String! population: Int! }
  """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  case class Country(name: String, population: Int)
  val allCountries = List(Country("Wales", 3_100_000), Country("Chad", 17_000_000))

  val typeMappings = TypeMappings(
    ObjectMapping(QueryType)(
      ValueField("countries", _ => allCountries)
    ),
    ObjectMapping(CountryType)(
      ValueField[Country]("name", _.name),
      ValueField[Country]("population", _.population)
    )
  )
}
```

There is no derivation magic here: `ValueField[Country]("name", _.name)` is just a function from
parent to child value. This makes `ValueMapping` a good fit for small, one-off models, tests, and
places where we want full control field-by-field. It does not scale gracefully to large models
because every field needs its own line.

### `GenericMapping` - automatic cursors for Scala case classes/ADTs

`GenericMapping` (in `grackle-generic`) solves the "every field needs a line" problem for the
common case where our Scala model already mirrors our GraphQL schema reasonably closely. Instead
of a `ValueField` per field, we derive a `CursorBuilder[T]` once per type, and Grackle walks the
case class/sealed trait structure for us (via reflection over the type's fields).

```scala
trait StarWarsMapping[F[_]] extends GenericMapping[F] {
  import semiauto._

  case class Human(id: String, name: Option[String], friends: Option[List[String]])
  object Human {
    implicit val cursorBuilder: CursorBuilder[Human] =
      deriveObjectCursorBuilder[Human](HumanType)
        .transformField("friends")(resolveFriends) // override just this one field
  }

  val typeMappings = TypeMappings(
    ObjectMapping(QueryType)(
      GenericField("human", allHumans.find(_.id == "1000").get)
    )
  )
}
```

`transformField` lets us patch individual fields (e.g. to resolve friend ids into actual `Human`
values) without abandoning derivation for the rest of the type. This is the mapping style used by
the [Star Wars in-memory tutorial](tutorial/in-memory-model.md). Reach for it whenever our model
is "plain Scala data that happens to look like the schema."

### `CirceMapping` - the data is already JSON

If our source of truth is `io.circe.Json` (a JSON API we are proxying, a JSON column, a cached
document), `CirceMapping` treats JSON values as the cursor focus directly, so no case classes are
needed at all. It is most often combined with `RootEffect`/`RootStream` (see below), which get
`computeJson`/`computeEncodable` variants for exactly this case:

```scala
class WeatherMapping[F[_]: Sync](client: WeatherClient[F]) extends CirceMapping[F] {
  val typeMappings = TypeMappings(
    ObjectMapping(QueryType)(
      RootEffect.computeJson("forecast")((path, env) =>
        env.getR[String]("city").flatTraverse(client.forecastJson))
    )
  )
}
```

### `SqlMapping` - relational databases

For data that lives in a relational database, `SqlMapping` (via `DoobiePgMapping`,
`DoobieOracleMapping`, `DoobieMSSqlMapping`, `DoobieH2Mapping`, `DoobieSqliteMapping` or Skunk's
`SkunkMapping`) is by far the most
sophisticated of the base classes: it compiles an entire nested GraphQL query into a *single* SQL
query with joins, rather than issuing one query per field (avoiding the classic GraphQL N+1
problem). Instead of `ValueField`, we get `SqlField` (a column), `SqlObject` (a related object,
optionally with a `Join`), and `SqlJson` (a JSON column):

```scala
trait WorldMapping[F[_]] extends DoobiePgMapping[F] {
  object country extends TableDef("country") {
    val code = col("code", Meta[String])
    val name = col("name", Meta[String])
  }
  object city extends TableDef("city") {
    val countrycode = col("countrycode", Meta[String])
    val name = col("name", Meta[String])
  }

  val typeMappings = TypeMappings(
    ObjectMapping(QueryType)(
      SqlObject("country")
    ),
    ObjectMapping(CountryType)(
      SqlField("code", country.code, key = true),
      SqlField("name", country.name),
      SqlObject("cities", Join(country.code, city.countrycode))
    ),
    ObjectMapping(CityType)(
      SqlField("name", city.name),
      SqlObject("country", Join(city.countrycode, country.code))
    )
  )
}
```

`key = true` marks the column(s) that uniquely identify a row (needed so Grackle can deduplicate
and join correctly); `Join(parentColumn, childColumn)` tells Grackle how two tables relate so it
can generate the right SQL join for a nested selection. See the
[DB-backed tutorial](tutorial/db-backed-model.md) for the full walkthrough, including filtering,
sorting and pagination via the query elaborator.

### `ComposedMapping` - stitching multiple mappings together

Sometimes no single backend covers our whole schema: say, most types come from a database but
one type's data lives in memory or behind an API. `ComposedMapping` does not hold any data of its
own; every field is handed off wholesale to some other `Mapping` via `Delegate`:

```scala
class Api[F[_]: Sync](world: Mapping[F], currency: Mapping[F]) extends ComposedMapping[F] {
  val typeMappings = TypeMappings(
    ObjectMapping(QueryType)(
      Delegate("country", world),   // -> SqlMapping
      Delegate("currencies", currency) // -> ValueMapping
    )
  )
}
```

The GraphQL schema for `Api` describes both `country` and `currencies`, but each field's subtree
is compiled and executed entirely by the delegate `Mapping` named. This is also how we would
combine, say, a `SqlMapping` for our core domain with a `CirceMapping` wrapping a third-party
JSON API, all under one unified schema.

## FieldMapping building blocks

These work the same way regardless of which base class we picked above (with the obvious
exception of backend-specific ones like `SqlField`/`ValueField`).

### `CursorField` - a computed leaf value

`CursorField` computes a scalar/leaf field's value from the `Cursor` currently in focus, rather
than from a column or a case class field. It is useful for derived values, and especially for
reading arguments or values stashed in `Env` (more on that below):

```scala
CursorField[Int]("sum", c =>
  (for {
    x <- c.env[Int]("x")
    y <- c.env[Int]("y")
  } yield x + y).toResult("Missing argument")
)
```

### `EffectField` - an effectful, batchable field anywhere in the tree

`EffectField` runs an `F[_]` effect to resolve a field that is not a root field, e.g. a per-row
call out to another service. Crucially, Grackle collects *all* the sibling cursors that need the
same effect and hands them to our `EffectHandler` together, so we can batch the underlying call
instead of doing it once per row (solving our own N+1 problem, the same way `SqlMapping` solves
GraphQL's):

```scala
ObjectMapping(CountryType)(
  // ... SqlField("code2", ...),
  EffectField("currencies", CurrencyQueryHandler, required = List("code2"))
)

object CurrencyQueryHandler extends EffectHandler[F] {
  def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
    // queries is every sibling row that requested `currencies` in this batch;
    // fetch them all in one call and return one Cursor per input query.
    ???
}
```

`required = List("code2")` tells Grackle that the `code2` column/field must already be present on
the parent cursor before the effect runs (e.g. because our handler needs it to make the call).

### `RootEffect` - an effect at the top of a query or mutation

`RootEffect` runs once, before the rest of the query is even compiled against a cursor. This is
the tool for mutations (insert/update/delete), auth checks, or any "do a thing, then answer with
the result" root field. There are four constructors depending on how much we need to override:

| Constructor         | Use when...                                                                 |
|----------------------|------------------------------------------------------------------------------|
| `computeUnit`         | We just need to perform an effect (e.g. a DB write) and return the default shape - the classic mutation that returns `Boolean`/no meaningful payload. |
| `computeChild`        | The effect produces something we need to fold back into the *query* (e.g. "insert a row, then filter the following selection down to just that new id"), letting the rest of the tree be answered normally. |
| `computeCursor`       | The effect produces the *entire* result cursor directly, bypassing the mapping's normal field resolution. |
| `apply`               | We need full control over both the query and the cursor.                   |

```scala
ObjectMapping(MutationType)(
  RootEffect.computeUnit("updatePopulation")(env =>
    env.getR[UpdatePopulation]("updatePopulation").traverse {
      case UpdatePopulation(id, pop) => updatePopulation(id, pop)
    }),
  RootEffect.computeChild("createCity")((child, _, env) =>
    env.getR[CreateCity]("createCity").flatTraverse {
      case CreateCity(name, cc, pop) =>
        createCity(name, cc, pop).map { id =>
          Unique(Filter(Eql(CityType / "id", Const(id)), child)).success
        }
    })
)
```

Here `env` holds the parsed mutation input; see the next section for how it gets there.

### `RootStream` - the subscription version of `RootEffect`

`RootStream` is `RootEffect`'s counterpart for GraphQL subscriptions: instead of returning a single
`F[Result[...]]`, it returns an `fs2.Stream[F, Result[...]]`, one element per event we want
pushed to the client. It has the same `computeCursor`/`computeChild` convenience constructors as
`RootEffect`.

### `LeafMapping` - custom scalars

If our schema declares a custom scalar (e.g. `scalar UUID`), give it a `LeafMapping` with a
circe `Encoder` so Grackle knows how to serialize values of that type. Built-in scalars
(`Int`, `Float`, `String`, `Boolean`, `ID`) already have one.

## A pitfall: using `ValueField` to call out to I/O

It is tempting (and it does compile, and does appear to work) to reach for `ValueField` when a
field's value comes from a REST call, a gRPC service, or an external process, skipping
`EffectField`/`RootEffect` altogether:

```scala
// Don't do this.
ValueField[Country]("exchangeRate", country =>
  Await.result(httpClient.get(s"/rates/${country.code}"), 5.seconds)
)
```

This "works" for a toy case: a handful of rows, no concurrent load, run it once and look at the
JSON. Past that, it stops being merely inelegant and becomes actually incorrect, for reasons
specific to how Grackle and cats-effect fit together, not just a matter of taste:

1. **`f: T => Any` is a plain, synchronous Scala function; there is no `F[_]` in sight.**
   `ValueMapping` requires a `MonadThrow[F]` on the *surrounding* mapping, but `ValueField.f`
   itself never touches `F`. There is no way to hand our effectful call to `F` and let it be
   sequenced, retried, run concurrently, or cancelled the way the rest of a Grackle service (and
   the rest of a typical cats-effect application) is built to work - the only option left is to
   force or block on it right there in the function body (`Await.result`, `.unsafeRunSync()`, a
   blocking HTTP client, ...).

2. **Blocking there defeats cats-effect's scheduling and breaks cancellation.** A thread blocked
   inside `ValueField.f` is invisible to the fiber runtime: it is not a suspended `F[_]` that
   `Fiber.cancel` or `IO.timeout` can interrupt, it is a JVM thread parked doing nothing useful.
   Do this on a bounded compute pool (the default for CPU-bound work in most cats-effect setups)
   and, under enough concurrent load, we can exhaust the pool and deadlock the server, a well
   known cats-effect trap, not something specific to Grackle but very easy to hit here.

3. **There is no batching, so it reintroduces the exact N+1 problem `EffectField` exists to solve.**
   `ValueField.f` is invoked once per row, in isolation - nothing collects the sibling rows of a
   list field into one request the way `EffectField`'s `EffectHandler.runEffects` does (see
   `EffectField` above). Put a REST call in a `ValueField` under a list field and we get one HTTP
   round trip per element, serially, on the request thread: the very thing `SqlMapping` and
   `EffectField` both exist to avoid.

4. **Failures bypass Grackle's error model entirely.** `CursorField`, `EffectField` and
   `RootEffect` all return a `Result[T]` (or `F[Result[T]]`), which is how Grackle attaches a
   well-formed, per-field GraphQL error (a `Problem` with a path) instead of failing the whole
   response. `ValueField.f` returns a bare `Any`, so the only way to signal failure is to *throw*.
   Cursor construction runs eagerly and is only lifted into `F` afterwards
   (`ResultT(runValue(...).pure[F])` in `QueryInterpreter.runRootValue`), so an exception thrown from
   `f` is a raw synchronous throw that happens *before* `F`'s own error channel
   (`MonadThrow[F]`/`attempt`/`handleErrorWith`) gets a chance to turn it into a `Result`. What
   should have been "this one field is null with an error attached" can instead take down far more
   than the field that failed, depending on what (if anything) wraps our top-level
   `compileAndRun` call.

**So when can we get away with it?** When the value handed to `ValueField` is already resident
and pure by the time `ValueField` runs, i.e. the actual I/O happened earlier, as an effect, and
`ValueField` is just projecting a plain field out of the value that effect produced. That is not
"`ValueField` doing I/O," it is the idiomatic combination of the two: fetch once via `RootEffect`,
then map purely over the result using `ValueMapping`'s own `valueCursor` helper:

```scala
ObjectMapping(QueryType)(
  RootEffect.computeCursor("countries")((path, env) =>
    restClient.fetchAllCountries.map(cs => valueCursor(path, env, cs).success))
),
ObjectMapping(CountryType)(
  ValueField[Country]("name", _.name),
  ValueField[Country]("exchangeRate", _.exchangeRate) // pure projection - already fetched above
)
```

One HTTP call, up front, for the whole list; everything under it is genuinely pure field
projection, which is exactly what `ValueField` is for.

Outside of that pattern, treat "I need to call out to get this field's value" as a hard signal for
`EffectField` (nested fields) or `RootEffect`/`RootStream` (root fields), not `ValueField` - even
in a prototype, since prototypes have a way of becoming production code. The one place blocking
inside `ValueField` is close to harmless is a genuinely single-threaded, throwaway script or test
fixture with no concurrency or cancellation contract to violate in the first place, and even
there, `EffectField`/`RootEffect` cost nothing extra to use correctly, so there is rarely a good
reason not to.

## `Env` - passing context down through a query

`Env` is an immutable, string-keyed heterogeneous map attached to every `Cursor`. It is how
information gets from "outside" a field (arguments, an effect's result, request-scoped context)
down to wherever it is actually needed, without threading extra parameters through every mapping
signature.

There are two ways values end up in `Env`:

**From the query elaborator**, using `Elab.env` to stash parsed arguments where a `CursorField`
further down the tree can read them:

```scala
override val selectElaborator = SelectElaborator {
  case (NestedType, "sum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y)))) =>
    Elab.env("x" -> x, "y" -> y)
}
```

Combined with the `CursorField("sum", ...)` example above, this is the standard pattern for
"the value of this field depends on its own GraphQL arguments."

**From a `RootEffect`/`RootStream`**, whose `effect` function receives an `Env` (typically holding
parsed input parsed earlier by the elaborator) and whose resulting `Cursor` can call `withEnv` to
pass values *forward* to its descendants, e.g. so a nested `CursorField` can see the outcome of
the root effect.

Cursors read values back out with `env[T](name): Option[T]` or `envR[T](name): Result[T]`
(the latter failing with a useful error if the key is missing or of the wrong type):

```scala
def sum(c: Cursor): Result[Int] =
  (for {
    x <- c.env[Int]("x")
    y <- c.env[Int]("y")
  } yield x + y).toResult("Missing argument")
```

`Env` is deliberately untyped (`Map[String, Any]` under the hood, guarded by a `ClassTag` check on
read). Treat it as a way to pass a handful of well-known values down a subtree, not as a general
dependency-injection mechanism for our whole application.

## Cheat sheet

- **"I just want to stand something up quickly with a few Scala values."** → `ValueMapping`.
- **"My Scala model already looks like my schema."** → `GenericMapping` with derived
  `CursorBuilder`s, `transformField` for the exceptions.
- **"My data is already JSON."** → `CirceMapping`.
- **"My data is in Postgres/Oracle/SQL Server/H2/SQLite."** → `SqlMapping` via Doobie or Skunk,
  `SqlField` + `SqlObject` + `Join`.
- **"A field's value depends on its own arguments or something computed higher up."** →
  `CursorField` reading from `Env`.
- **"A field needs an effect (I/O), and other sibling rows will need the same kind of effect."** →
  `EffectField` with a batching `EffectHandler`.
- **"A whole query or mutation needs to run an effect before anything else can happen."** →
  `RootEffect` (or `RootStream` for subscriptions).
- **"This field's data actually belongs to a completely different mapping/backend."** →
  `Delegate`, inside a `ComposedMapping`.
- **"I'm tempted to make a blocking/effectful call inside a `ValueField`."** → Don't - see the
  pitfall above. Fetch once via `RootEffect`/`EffectField`, then project purely with `ValueField`
  over the result.

## Beyond the built-in backends

`ValueMapping`, `GenericMapping`, `CirceMapping` and `SqlMapping` cover the common cases, but
they are not the only data sources people put behind Grackle. Here is how the same building blocks
apply to three sources that do not have a dedicated `Mapping` subclass.

### Client-side storage (e.g. IndexedDB)

There is no `IndexedDbMapping`, and there should not be one - IndexedDB is not relational, so
none of `SqlMapping`'s table/column/`Join` machinery applies. What it *is*, an asynchronous,
key/object store with per-store lookups by key or index, makes it exactly the shape of data source
`RootEffect`/`EffectField` were built for:

- Every read is asynchronous (`IDBRequest.onsuccess`/`onerror`), so it must be wrapped as an
  effect in `F` - typically `cats.effect.IO` on Scala.js, bridged from IndexedDB's callback API via
  `IO.async_` (or a small `scala-js-dom` interop helper), never called synchronously from a
  `ValueField`/`GenericField` (see the pitfall above - it applies just as much to a browser event
  loop as it does to a JVM thread pool: blocking there freezes the tab).
- Pick `ValueMapping`/`GenericMapping` if we will decode each record into a Scala case class, or
  `CirceMapping` if we are happy keeping records as `Json` (IndexedDB stores structured-clone JS
  values, which map onto JSON very naturally, and `CirceCursor.field` will pick GraphQL fields
  straight off matching object keys with no per-field mapping at all).
- IndexedDB has no server-side join. Resolving a relationship (e.g. "orders for this customer")
  means an `EffectField` (or nested `RootEffect`) that does a batched `getAll`/key-range lookup
  across every sibling row requesting that relation in one go - the same batching shape as the
  `CurrencyQueryHandler` example under `EffectField` above, just keyed by an IndexedDB index
  instead of a SQL `WHERE ... IN (...)`.

### Graph databases

Same reasoning as IndexedDB: no built-in `GraphMapping`, and `SqlMapping`'s DSL does not apply
unless our graph database happens to expose a genuine JDBC/SQL bridge (a few do - check before
building anything custom, since if it is there we can reuse `SqlMapping` as-is). Otherwise, treat
the driver the same way we would treat any other effectful client: `RootEffect`/`EffectField` issue
the traversal query (Cypher, Gremlin, ...) in `F`, and the result (nodes, edges, or rows) lands in
either a case class (`GenericMapping`/`ValueMapping`) or `Json` (`CirceMapping`), depending on what
the driver hands back.

The one real design decision is *how much of the nested GraphQL selection we push down into a
single graph query*. Graph databases are good at multi-hop traversal, so unlike IndexedDB there is
a genuine payoff to doing so - but Grackle does not give us this for free the way it does for SQL:
`SqlMapping` is thousands of lines of purpose-built compiler translating the elaborated `Query`
algebra (the `Select`/`Filter`/`Group`/... tree described in the
[in-memory tutorial](tutorial/in-memory-model.md#the-query-compiler-and-elaborator)) into one SQL
statement with joins. Writing an equivalent for Cypher/Gremlin is a serious undertaking, not
something to reach for by default. Start with the simple thing - one `EffectField` per hop,
batched across siblings exactly like the SQL/IndexedDB examples above - and only invest in
compiling whole nested selections into a single traversal if profiling says the extra round trips
actually matter.

### An escape hatch: arbitrary/opaque JSON

Given something like `exportGdpr(user: ID!): JSON` where the result is a JSON document whose shape
is not (and should not need to be) described by the GraphQL schema, `CirceMapping` is the right
tool, via `RootEffect.computeJson`/`computeEncodable`, but there are two different situations to
tell apart, because they lead to different mapping code:

**The result has a roughly known top-level shape we are happy exposing as real types.** Then do not
model it as a scalar at all - give it proper GraphQL object types (`type GdprExport { profile:
Profile! orders: [Order!]! ... }`) and return the fetched `Json` via `computeJson`. We need *no*
per-field mapping for these types: `CirceCursor.field` already looks up each requested GraphQL
field as a same-named key on the JSON object automatically. This is the better default whenever
we can manage it: it is introspectable, and clients get real typed errors instead of "some key was
missing inside an opaque blob."

**The result is genuinely free-form** (shape varies per user, per plan, whatever - the literal
"just give me a JSON blob" case). Now we want a real custom scalar:

```scala
val schema = schema"""
  scalar JSON
  type Query { exportGdpr(user: ID!): JSON! }
"""

val typeMappings = TypeMappings(
  LeafMapping[Json](schema.ref("JSON")), // circe already has an identity Encoder[Json]
  ObjectMapping(QueryType)(
    RootEffect.computeJson("exportGdpr")((_, env) =>
      env.getR[String]("user").flatTraverse(gdprService.export))
  )
)
```

Watch out for one sharp edge here: `CirceMapping`'s own leaf encoding
(`CirceCursor.asLeaf`, in `circemapping.scala`) accepts a custom scalar's focus value only when
it is *not* a JSON object: objects are assumed to be GraphQL object types, not scalar payloads, so
an object-shaped export routed through `computeJson`/`computeEncodable` into a `scalar JSON` field
fails with "Expected Scalar type, found ...". If our free-form document is (as GDPR exports
usually are) a JSON *object*, do not route it through `computeJson`; build the leaf cursor directly
against the base `Mapping.LeafCursor`, whose `asLeaf` just runs our `LeafMapping`'s `Encoder`
against the focus value with no such restriction:

```scala
RootEffect.computeCursor("exportGdpr")((path, env) =>
  env.getR[String]("user").flatTraverse { user =>
    gdprService.export(user).map(_.map(json => LeafCursor(Context(path.rootTpe), json, None, env)))
  })
```

Either way, note that `user: ID!` (or `String!`) is doing the work here, not `Long`. Grackle's
built-in scalars are exactly the five from the GraphQL spec (`Int`, `Float`, `String`, `Boolean`,
`ID`); a genuine `Long` argument needs its own custom scalar (and elaborator-side parsing of the
incoming argument value) the same way `JSON` does as a result type.

## Where to go next

- [In-memory tutorial](tutorial/in-memory-model.md) - `GenericMapping` end to end.
- [DB-backed tutorial](tutorial/db-backed-model.md) - `SqlMapping` end to end, including the query
  elaborator for filtering/sorting/pagination.
- [Compiler Phases](howto/compiler-phases.md) - installing our own compiler phase, and the
  built-in `QuerySizeValidator`.
- [Scaladoc](https://javadoc.io/doc/org.typelevel/grackle-core_2.13) for the full API of every
  type mentioned here.
