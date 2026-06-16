# Serve GraphQL from circe JSON

This how-to is for developers whose data is already JSON — test fixtures, statically-known documents, or responses from a REST/HTTP service — and who want to serve it over GraphQL with no database. It shows you how to build a `CirceMapping` over an in-memory circe `Json` value: mapping a constant subtree with `CirceField`, mapping a custom scalar with `LeafMapping`, computing a field from JSON with `CursorField`, fetching JSON from an effect with `computeJson`/`computeEncodable`, and the one rule that trips people up — opaque-subtree priority. The "why" behind cursors and leaf validation lives in [mappings and cursors](../concepts/mappings-cursors.md) and the [CirceMapping reference](../reference/circe-mapping.md); this page is the recipe.

## When to choose the circe backend

Reach for the circe backend (`grackle-circe`) when your data source is circe `Json` rather than a relational database:

- **Fixtures and tests** — back a schema with a literal JSON document so you can exercise queries without spinning up a database.
- **Statically-known data** — configuration, reference data, or any document baked into the app.
- **JSON from REST/HTTP** — you already have a response body as `Json` (or a domain value with a circe `Encoder`) and want to project a GraphQL view over it.

The backend needs only a `cats.MonadThrow[F]` — there is no connection pool, codec, or transactor to wire. If your data lives in a SQL store, use a [SQL backend](sql-backends.md) instead; you can still splice JSON-valued fields into a SQL mapping (see [the last section](#splice-json-into-a-sql-mapping)).

Add the dependency:

```scala
libraryDependencies += "org.typelevel" %% "grackle-circe" % "@VERSION@"
```

## Define the schema and a Json value

A standalone JSON-backed mapping extends `CirceMapping[F]` and provides three things: a `schema`, a `Json` value, and `typeMappings` that tie the two together. Here is the canonical example from the circe test suite. Read it top to bottom — the GraphQL schema, then the `data` value (built with the `json"…"` interpolator from `io.circe.literal._`), then the `typeMappings`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CirceData.scala", "#circe_mapping"))
```

The schema is written with the `schema"…"` interpolator (from `grackle.syntax._`), which validates at compile time and yields a bare `Schema`. Note the GraphQL nullability is ordinary SDL here: `bool: Boolean` is nullable, `children: [Child!]!` is a non-null list of non-null `Child`. The `data` value is one `Json` object whose shape mirrors the `Root` type — object fields are looked up by name, `array` is a JSON array, `choice` is the enum value as a JSON string, and `children` is a JSON array of objects that each match one of the interface's implementations.

## Map a field with CirceField

The single line that connects schema to data is `CirceField("root", data)` in the `Query` mapping. `CirceField(fieldName, value: Json)` maps a GraphQL field to a fixed `Json` subtree: everything reachable under `Query.root` is served by walking `data`. You do **not** write a field mapping for `bool`, `int`, `object`, `children` and so on — Grackle matches the JSON structurally against the `Root` type as it navigates.

Querying it:

```graphql
query {
  root {
    object {
      id
      aField
    }
  }
}
```

```json
{
  "data": {
    "root": {
      "object": {
        "id": "obj",
        "aField": 27
      }
    }
  }
}
```

Built-in scalars (`Int`, `Float`, `String`, `Boolean`, `ID`) and enums need no mapping: an enum must be a JSON string equal to a declared enum value, an `Int` is re-parsed from the JSON number, and a `Float` is passed through. A JSON key that is not a field in the schema — `hidden` in the data above — is simply unreachable: querying `root { hidden }` returns `"No field 'hidden' for type Root"` in the response `errors`.

## Map a custom scalar with LeafMapping

Custom GraphQL scalars are the one leaf type the circe backend can't resolve on its own. The schema declares `scalar BigDecimal`, so the mapping registers `LeafMapping[BigDecimal](BigDecimalType)`. That tells Grackle how the `BigDecimal` GraphQL scalar relates to the Scala type when it reads the JSON number `1.2`. Without the `LeafMapping`, a custom scalar's JSON value falls through a catch-all and is not handled as you'd expect — always add a `LeafMapping[T]` for each non-built-in scalar you serve.

## Compute a field from JSON with CursorField

Sometimes a GraphQL field has no direct JSON counterpart and must be derived. The example maps `computed` with `CursorField("computed", computeField, List("hidden"))`. A `CursorField` computes its value from the parent `Cursor`; the third argument lists **required** sibling fields that must be available even though they aren't queried. Here `computeField` reads the JSON-only `hidden` field (value `13`) via `c.fieldAs[Json]("hidden")`, takes its number, and returns `+ 1`:

```graphql
query {
  root {
    computed
  }
}
```

```json
{
  "data": {
    "root": {
      "computed": 14
    }
  }
}
```

This is the pattern for reading data that exists in the JSON but is deliberately absent from the schema: keep the value in the document, leave it out of the SDL, and pull it in through `required`. For a JSON *subtree* (rather than a scalar) computed this way, use `CursorFieldJson` instead — covered [below](#splice-json-into-a-sql-mapping).

## Run queries, including effectful roots

You run an operation with `mapping.compileAndRun(query)`, which returns the response as `F[Json]` (for `CirceMapping[IO]`, an `IO[Json]`).

When the JSON isn't already in hand — you have to fetch it from an HTTP service, read a file, or otherwise perform an effect — use a `RootEffect`. `CirceMappingLike` adds three ways to do this, shown side by side in the effect test mapping (the bodies here bump a `SignallingRef` only to prove each effect runs exactly once; in real code that's where your `F`-effect goes):

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CirceEffectData.scala", "#circe_effects"))
```

The three styles differ only in what the effect yields:

- **`RootEffect.computeJson(field)((path, env) => F[Result[Json]])`** — your effect produces a `Json`; the backend wraps it in a `CirceCursor` for you. This is the everyday choice for "fetch JSON, serve it".
- **`RootEffect.computeEncodable[A](field)((path, env) => F[Result[A]])`** — your effect produces a domain value `A`; given an implicit circe `Encoder[A]` (here `EncodeStruct`), the backend encodes it to JSON and wraps it.
- **`RootEffect.computeCursor(field)(...)`** — full control: you call `circeCursor(path, env, json)` yourself. Use this when you need to choose the cursor's `path` explicitly.

That path choice is the subtle part. `computeJson`/`computeEncodable` (and `circeCursor(p, e, json)` with the handler's `p`, as in `foo`) expect `json` to be the **value of the field** — so `foo`/`bar`/`baz` each yield just `{ "n": …, "s": … }`. The `qux` handler instead roots the cursor with `circeCursor(Path.from(p.rootTpe), e, json)`, so its `json` must be an **object containing the field name**: `{ "qux": { … } }`. Mixing these up produces a shape mismatch. For streaming roots (subscriptions), `RootStream.computeJson` and `RootStream.computeEncodable` are the `fs2.Stream` counterparts — each emitted element becomes one cursor. See [effects and batching](effects-batching.md) for how root effects fit the wider effect model.

## Opaque-subtree priority: JSON beats an explicit mapping

`CirceField` (and `CursorFieldJson`) mark their subtree as **opaque** (`subtree = true`). When the cursor looks up a field, it checks the current JSON object *first*, and only falls back to the rest of your `typeMappings` when the JSON object lacks that field. This is the rule that surprises people, so here is a mapping built to expose it:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/circe/src/test/scala/CircePrioritySuite.scala", "#circe_priority"))
```

Both `present` and `fallback` are `Barrel`s whose `monkey` is a `Monkey`, and there is an explicit `CirceField("name", Json.fromString("Steve"))` mapping on `Monkey`. The difference is the JSON:

- `present`'s JSON is `{ "monkey": { "name": "Bob" } }` — the `name` field is **present in the JSON**, so the literal `"Bob"` wins and the explicit `Monkey.name` mapping is shadowed.
- `fallback`'s JSON is `{ "monkey": {} }` — the JSON object **omits** `name`, so Grackle falls back to the explicit mapping and serves `"Steve"`.

```graphql
query {
  present  { monkey { name } }
  fallback { monkey { name } }
}
```

```json
{
  "data": {
    "present":  { "monkey": { "name": "Bob" } },
    "fallback": { "monkey": { "name": "Steve" } }
  }
}
```

The practical takeaway: an explicit field mapping for a type embedded inside a `CirceField` subtree only takes effect for keys the JSON doesn't already supply. If you want the explicit mapping to govern a field, don't put that key in the JSON.

## Splice JSON into a SQL mapping

The behavior above isn't limited to the circe module. `CirceMappingLike[F]` is a *trait* that carries the whole field-mapping surface — `CirceField`, `CursorFieldJson`, `circeCursor` — and `SqlMappingLike[F]` extends it. That single inheritance is why you can decode a database column into a JSON value inside a SQL mapping and serve it as a GraphQL subtree, using `CursorFieldJson`:

```scala mdoc:compile-only
import io.circe.Json
import grackle._
import grackle.circe.CirceMappingLike

// Inside a mapping that mixes in CirceMappingLike[F] (e.g. a SqlMapping):
trait Sketch[F[_]] extends CirceMappingLike[F] {
  // decode a sibling column (declared in `required`) into a Json subtree
  def decode(c: Cursor): Result[Json] = ???

  def jsonField: FieldMapping =
    CursorFieldJson("categories", decode, required = List("categoriesId"))
}
```

`CursorFieldJson(fieldName, f, required)` runs `f` against the parent `Cursor` — which can read the SQL-backed sibling columns named in `required` — and serves the resulting `Json` as an opaque subtree, exactly like `CirceField`. For the full SQL-side recipe (decoding a column with a circe `Encoder` and exposing it as structured GraphQL), see [storing and querying JSONB columns](jsonb-columns.md).

## See also

- [CirceMapping reference](../reference/circe-mapping.md) — every signature: `CirceField`, `CursorFieldJson`, `CirceCursor`, `circeCursor`, and the `computeJson`/`computeEncodable` syntax.
- [Mappings and cursors](../concepts/mappings-cursors.md) — how a `Cursor` walks a data source against the schema, the concept behind `CirceCursor`.
- [Custom scalars and enums](custom-scalars-enums.md) — more on `LeafMapping[T]` and scalar/enum handling.
- [Storing and querying JSONB columns](jsonb-columns.md) — splicing JSON into a SQL backend with `CursorFieldJson`.
- [Effects and batching](effects-batching.md) — where `RootEffect`/`RootStream` fit the effect model.
