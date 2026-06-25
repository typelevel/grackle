# Serve a mapping over HTTP

This page shows you how to put a `Mapping[F]` behind an HTTP endpoint so that clients can send it GraphQL queries, mutations and introspection requests. Grackle ships **no** HTTP layer of its own — a mapping is a transport-agnostic runner — so the wiring here is example code you copy into your own service. It is written for developers deploying a Grackle endpoint who are comfortable with [http4s](https://http4s.org/) and cats-effect. The snippets are the canonical ones from Grackle's `demo` module.

## Goal: expose a `Mapping[F]` as a GraphQL-over-HTTP endpoint

A `Mapping[F]` already bundles everything needed to execute a request: a `Schema`, the type mappings, and the lazily-built compiler and interpreter. The single method you call per request is:

```scala
def compileAndRun(
    text: String,
    name: Option[String] = None,
    untypedVars: Option[Json] = None,
    introspectionLevel: IntrospectionLevel = Full,
    reportUnused: Boolean = true,
    env: Env = Env.empty)(
    implicit sc: Compiler[F, F]
): F[Json]
```

It takes the GraphQL request triad — the query `text`, an optional operation `name`, and optional `variables` as a JSON object — and returns an `F[Json]` already wrapped in the spec response envelope (`{"data": ...}` or `{"errors": [...]}`). Your only job at the HTTP layer is to pull those three values out of the request and hand them to `compileAndRun`. (Subscriptions use `compileAndRunSubscription` instead — see [below](#subscriptions-have-no-built-in-transport).)

## Build the GET and POST routes

To serve a mapping, build an `HttpRoutes[F]` with one case for `GET` (the request triad arrives in the URI query string) and one for `POST` (it arrives in a JSON body). The demo's `GraphQLService.mkRoutes` does exactly this:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/GraphQLService.scala", "#service"))
```

A few things to note:

- `mkRoutes[F[_]: Concurrent](prefix: String)(mapping: Mapping[F])` is parameterised by a path `prefix`, so the same factory serves any number of mappings under different paths.
- The `GET` case matches `query`, `operationName` and `variables` from the URI and forwards them to `mapping.compileAndRun(query, op, vars)`.
- The `POST` case decodes the body `as[Json]`, then reads the top-level `query`, `operationName` and `variables` fields. A missing `query` field is rejected with `InvalidMessageBodyFailure("Missing query field")`. This handler reads only those three fields — it implements neither batched operations nor persisted queries.

### Decoding `variables` on GET, and bad-request handling

For `POST` the variables are already a JSON value inside the body. For `GET` they arrive as a URL-encoded JSON *string*, so the route installs a custom `QueryParamDecoder[Json]` that parses the string and turns a parse failure into a `ParseFailure("Invalid variables", …)`. The matcher is an `OptionalValidatingQueryParamDecoderMatcher[Json]`, so the route receives a validated result; when it is invalid the route short-circuits to `BadRequest` with the sanitized error message. This is a transport-level 400, separate from GraphQL validation: a malformed `variables` string never reaches the compiler. (Inside the compiler, `variables` must be a JSON *object*; a non-object value fails compilation with `Variables must be represented as a Json object`.)

## Build the Ember server

With routes in hand, wrap them in an Ember server. The demo's `DemoServer.mkServer` mounts the GraphQL routes alongside the static GraphQL Playground assets, adds logging and total error handling, and binds the listener:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/DemoServer.scala", "#server"))
```

The structure is:

- `resourceServiceBuilder[IO]("/assets").toRoutes <+> graphQLRoutes` — the static Playground client is combined with the GraphQL routes using the cats `<+>` (`SemigroupK`) operator, then `.orNotFound`.
- `Logger.httpApp(true, false)` wraps the app with request/response logging.
- `ErrorHandling.Recover.total(...)` installs a **total** error handler so that any uncaught `Throwable` is logged via `errorHandler` rather than crashing the server. This is what catches the effect failures raised by internal errors (see [below](#internal-errors-vs-validation-errors)).
- `EmberServerBuilder.default[IO].withHost(ip"0.0.0.0").withPort(port"8080")` binds the server, returning a `Resource[IO, Unit]`.

## Wire multiple mappings and run

Each mapping is built as a `Resource`, lifted to routes under its own prefix, and the routes are combined with `<+>` before being handed to the server. The demo's `Main` is an `IOApp` doing this for two mappings:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/Main.scala", "#main"))
```

`StarWarsMapping[IO]` and `WorldMapping[IO]` are each a `Resource[IO, Mapping[IO]]`; `.map(mkRoutes("starwars"))` / `.map(mkRoutes("world"))` turn them into routes mounted at `/starwars` and `/world`; `starWarsRoutes <+> worldRoutes` merges them; and `useForever` runs the server until the process is killed. To add another endpoint, construct another mapping, map it through `mkRoutes("yourprefix")`, and fold it into the `<+>` chain.

## Required dependencies

The wiring above lives entirely in the `demo` module and depends on http4s — Grackle itself does not. Add the http4s pieces you use:

```scala
val http4sVersion = "0.23.34"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-circe"        % http4sVersion,
  "org.http4s" %% "http4s-dsl"          % http4sVersion
)
```

`http4s-circe` supplies the `Json` entity codecs (`req.as[Json]`, `Ok(json)`), `http4s-dsl` supplies the route DSL and query-param matchers, and `http4s-ember-server` supplies `EmberServerBuilder`. Pick the http4s version that matches the rest of your stack.

## Set `introspectionLevel` for production

`compileAndRun` (and the underlying `compile`) take `introspectionLevel: IntrospectionLevel = Full`. The levels are:

- `Full` — the request may run the standard `IntrospectionQuery` over `__schema`/`__type`. GraphQL client tools such as Playground and GraphiQL need this to discover your schema.
- `TypenameOnly` — only `__typename` is allowed; `__schema`/`__type` are rejected.
- `Disabled` — all introspection is rejected.

The default is `Full`, which is why the demo's Playground works out of the box. To lock down a public endpoint, pass `introspectionLevel = Disabled` (or `TypenameOnly`) into `compileAndRun`: normal queries still run, but schema discovery is refused. Note that this also breaks Playground/GraphiQL against that endpoint.

## Internal errors vs validation errors

How a request fails determines what the client sees. Grackle's `Result` distinguishes ordinary GraphQL failures from internal errors, and the two take different paths out of `compileAndRun`:

- **GraphQL validation/execution failures** (a `Result.Failure`/`Warning`) come back as a normal **HTTP 200** response carrying an `errors` array — the standard `{"data": ..., "errors": [...]}` envelope. The route's `Ok(result)` returns them like any other success.
- **Internal errors** (`Result.InternalError`) are *not* turned into JSON. Internally, `mkResponse` calls `M.raiseError` for an internal error, so it propagates as a failure in the effect `F`. At the HTTP layer that surfaces as an **HTTP 500**, caught and logged by the total `ErrorHandling.Recover` in `mkServer`.

So never expect an internal error to appear in the response `errors` array — only `Problem`s from validation/execution failures do. See [Construct, accumulate and report errors](errors.md) for how to produce `Failure`/`Warning` results that *do* surface as GraphQL errors.

## Subscriptions have no built-in transport

Grackle exposes subscriptions only through the streaming API:

```scala
def compileAndRunSubscription(
    text: String,
    name: Option[String] = None,
    untypedVars: Option[Json] = None,
    introspectionLevel: IntrospectionLevel = Full,
    reportUnused: Boolean = true,
    env: Env = Env.empty): Stream[F, Json]
```

This returns an `fs2.Stream[F, Json]` that emits one response per upstream change. There is **no** built-in websocket or `graphql-ws` / `graphql-transport-ws` transport anywhere in Grackle, and the demo server above serves only `GET`/`POST` — it does not expose subscriptions over HTTP at all. Bridging the stream to a websocket (or Server-Sent Events) is left entirely to you: take the `Stream[F, Json]` from `compileAndRunSubscription` and feed it into your transport of choice, for example an http4s `WebSocketBuilder`.

Two cautions:

- Do **not** call `compileAndRun` on a subscription. `compileAndRun` is defined in terms of `compileAndRunSubscription` and asserts the result stream has exactly one element, so a subscription fails at runtime with `Result stream contained N results; expected exactly one.` Use `compileAndRunSubscription` for subscriptions.
- Variables, `operationName` and `env` thread through `compileAndRunSubscription` exactly as they do for queries.

See [Mutations & Subscriptions](../tutorial/mutations-subscriptions.md) for how a mapping produces a subscription stream in the first place.

## See also

- [Mutations & Subscriptions](../tutorial/mutations-subscriptions.md) — build a mapping that exposes mutations and subscription streams.
- [Run effects and batch nested fields](effects-batching.md) — drive root effects and streams from within a mapping.
- [Construct, accumulate and report errors](errors.md) — produce `Result` failures that surface in the GraphQL `errors` array.
- [Running operations reference](../reference/running-operations.md) — full signatures for `compileAndRun`, `compileAndRunSubscription`, `IntrospectionLevel` and the response envelope.
