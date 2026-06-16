# Running operations reference

A `Mapping[F]` is a self-contained, transport-agnostic GraphQL endpoint: everything needed to execute an operation hangs off the mapping instance. This page is the information-oriented reference for the two public entry points — `compileAndRun` (one query/mutation → one JSON response) and `compileAndRunSubscription` (a subscription → an `fs2.Stream` of responses) — the `compile → interpret → mkResponse` pipeline they sit on, the `Operation`/`UntypedOperation` types that flow through it, the per-request parameters (`name`, `untypedVars`, `introspectionLevel`, `reportUnused`, `env`), and the response envelope `mkResponse` produces. It is for any developer driving a mapping. Grackle ships no HTTP or websocket transport of its own — for wiring these methods to http4s see [Serve a Mapping over HTTP](../how-to/serve-over-http.md); all signatures below come from `modules/core/src/main/scala/mapping.scala`, `compiler.scala`, `operation.scala` and `queryinterpreter.scala`.

## The two entry points

Both methods live on `Mapping[F]`. `compileAndRunSubscription` is the primitive; `compileAndRun` is defined in terms of it.

| Method | Signature | Returns | Use for |
| --- | --- | --- | --- |
| `compileAndRun` | `def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty)(implicit sc: Compiler[F, F]): F[Json]` | `F[Json]` — one response | queries and mutations |
| `compileAndRunSubscription` | `def compileAndRunSubscription(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty): Stream[F, Json]` | `Stream[F, Json]` — one element per emission | subscriptions (also works for queries/mutations) |

### The exactly-one-element contract

`compileAndRun` runs `compileAndRunSubscription`, collects the stream with `.compile.toList`, and asserts the result has **exactly one** element:

| Stream length | Outcome |
| --- | --- |
| `1` | the single `Json` response |
| `0` | `M.raiseError(new IllegalStateException("Result stream was empty."))` |
| `n > 1` | `M.raiseError(new IllegalStateException(s"Result stream contained $n results; expected exactly one."))` |

A query or mutation always interprets to a single element, so `compileAndRun` is correct for them. A subscription emits one element per upstream change, so calling `compileAndRun` on a subscription **fails at runtime** (not compile time) with the `IllegalStateException` above. Use `compileAndRunSubscription` for subscriptions.

### Worked example

The following runs a subscription stream concurrently while three mutations push new values through a `SignallingRef`, so each mutation drives one element out of the subscription. It exercises both entry points against the in-memory mapping defined earlier in `SubscriptionSuite`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/subscription/SubscriptionSuite.scala", "#run_ops"))
```

`compileAndRunSubscription("subscription { watch }")` yields a `Stream[IO, Json]`; `.take(4).compile.toList.start` runs it in a fiber so it can observe later emissions. Each `compileAndRun("mutation { put(n: …) }")` returns an `IO[Json]` (one response) and, as a side effect, updates the `SignallingRef` the subscription is watching — producing the next stream element. The `IO.sleep`s only sequence the demonstration; they are not part of the API.

## The compile → interpret → mkResponse pipeline

`compileAndRunSubscription` is a thin composition of three stages. Reading its body left to right:

```text
text ──▶ compiler.compile(text, name, untypedVars,        ──▶ Result[Operation]
                          introspectionLevel,
                          reportUnused, env)

Operation ──▶ interpreter.run(op.query, op.rootTpe, env)  ──▶ Stream[F, Result[Json]]

Result[Json] ──▶ mkResponse                               ──▶ F[Json]   (per element)
```

The two members it dispatches to are also public on the mapping:

| Member | Signature | Role |
| --- | --- | --- |
| `compiler` | `lazy val compiler: QueryCompiler` | parses, validates, binds variables and elaborates `text` into a `Result[Operation]`. Built from the schema + compiler phases; accessing it for the first time runs `typeMappings.unsafeValidateIfChecked()` |
| `interpreter` | `val interpreter: QueryInterpreter[F] = new QueryInterpreter(this)` | executes a compiled `Operation` against the mapping's data |

### `QueryCompiler.compile`

```scala
def compile(
  text: String,
  name: Option[String] = None,
  untypedVars: Option[Json] = None,
  introspectionLevel: IntrospectionLevel = Full,
  reportUnused: Boolean = true,
  env: Env = Env.empty): Result[Operation]
```

`compile` runs, in order: parsing (`QueryParser.parseText`), operation selection by `name`, variable and fragment validation, field-merge validation, variable binding, directive validation, and the elaborator phases (including the `IntrospectionElaborator` gated by `introspectionLevel`). Its output is a `Result[Operation]`. See [The compiler and elaboration](../concepts/compiler-elaboration.md) for the phase model and [Elaboration phases reference](elab-phases.md) for the phase list.

### `QueryInterpreter.run`

```scala
def run(query: Query, rootTpe: Type, env: Env): Stream[F, Result[Json]]
```

`run` dispatches on `rootTpe`: when it is the schema's subscription type (`schema.subscriptionType.exists(_ =:= rootTpe)`) it drives `runSubscription` (many elements); otherwise it runs a single one-shot interpretation (one element). This is why a subscription's stream can emit repeatedly while a query/mutation's emits once — the [exactly-one-element contract](#the-exactly-one-element-contract) above follows directly from this dispatch.

## `Operation` and `UntypedOperation`

These are the two forms an operation takes either side of compilation. `parseText` produces `UntypedOperation`s; `compile` selects one and elaborates it into an `Operation`.

| Type | Signature | Stage |
| --- | --- | --- |
| `UntypedOperation` | `sealed trait UntypedOperation { val name: Option[String]; val query: Query; val variables: UntypedVarDefs; val directives: List[Directive]; def rootTpe(schema: Schema): Result[NamedType] }` | parser output (pre-compile) |
| `Operation` | `case class Operation(query: Query, rootTpe: NamedType, directives: List[Directive])` | compiler output (executable) |

`UntypedOperation` has three cases — `UntypedQuery`, `UntypedMutation`, `UntypedSubscription` — each carrying the same fields. Its `rootTpe(schema)` resolves the matching root operation type, and fails when the schema does not define one:

| Case | `rootTpe(schema)` resolves to | Failure when absent |
| --- | --- | --- |
| `UntypedQuery` | `schema.queryType` | — (every schema has a query type) |
| `UntypedMutation` | `schema.mutationType` | `"No mutation type defined in this schema."` |
| `UntypedSubscription` | `schema.subscriptionType` | `"No subscription type defined in this schema."` |

An `Operation` is what the interpreter consumes: `op.query` is the elaborated [query algebra](query-algebra.md) term, `op.rootTpe` is the resolved root operation type (used by `run` to choose query vs subscription execution), and `op.directives` are any operation-level directives.

`UntypedVarDefs` (on `UntypedOperation`) are the variable *declarations* parsed from the operation's `($x: T)` header; the request's variable *values* arrive separately as `untypedVars` and are bound to those declarations during compilation.

### Parsing

`QueryParser.parseText` is the parse-only stage feeding `compile`:

```scala
def parseText(text: String): Result[(List[UntypedOperation], List[UntypedFragment])]
```

It turns document text into the untyped operations and fragments (the pre-compile AST) and fails with `"At least one operation required"` on an empty document. "Parse" is purely syntactic; "compile" additionally validates, binds and elaborates.

## Per-request parameters

The parameters shared by `compileAndRun`, `compileAndRunSubscription` and `QueryCompiler.compile`:

| Parameter | Type / default | Meaning | Notable failures |
| --- | --- | --- | --- |
| `text` | `String` | the GraphQL document | `"At least one operation required"` if empty |
| `name` | `Option[String] = None` | `operationName` — selects one operation when the document defines several | with multiple operations and `None`: `"Operation name required to select unique operation"`; also `"No operation named …"` / `"Multiple operations named …"`; `"Query shorthand cannot be combined with multiple operations"` |
| `untypedVars` | `Option[Json] = None` | request variables, as a JSON **object** | `"Variables must be represented as a Json object"` if the `Json` is not an object |
| `introspectionLevel` | `IntrospectionLevel = Full` | how much introspection to permit (see below) | introspection fields rejected per level |
| `reportUnused` | `Boolean = true` | report unused variables/fragments as accumulated problems; set `false` to silence | — |
| `env` | `Env = Env.empty` | request-scoped context (see below) | — |

With a **single** operation, `name` may be `None`. With **multiple** operations, `name` is required.

### `IntrospectionLevel`

Defined in `compiler.scala`; controls how much GraphQL introspection (`__schema` / `__type` / `__typename`) a request may use.

| Value | Permits |
| --- | --- |
| `Full` (default) | all introspection — the standard `IntrospectionQuery` issued by GraphQL Playground / GraphiQL |
| `TypenameOnly` | only `__typename` |
| `Disabled` | no introspection; `__schema` / `__type` are rejected |

```scala
sealed trait IntrospectionLevel
object IntrospectionLevel {
  case object Full         extends IntrospectionLevel
  case object TypenameOnly extends IntrospectionLevel
  case object Disabled     extends IntrospectionLevel
}
```

Setting `Disabled` (or `TypenameOnly`) breaks client tools' schema discovery while still allowing normal queries.

### `env`

`env: Env` threads request-scoped values — auth, tenancy, feature flags — into elaborators and effectful root fields. It is reachable from a [`CursorField`](mapping-types.md) or effect handler via `cursor.env[T](key)`. For example, `compileAndRun(query, env = Env("secure" -> true))` makes `"secure"` available to a field resolver that reads `c.env[Boolean]("secure")`. See [Context & Env reference](context-env.md) for the full `Env` API.

## The response envelope

Each `Result[Json]` from the interpreter passes through `mkResponse`, the single boundary that produces the spec-shaped response. There are two overloads:

```scala
def mkResponse(result: Result[Json]): F[Json]
def mkResponse(data: Option[Json], errors: Chain[Problem]): Json
```

The `Result` overload maps each [`Result`](result-problem.md) case to a response, and — crucially — **does not encode `InternalError` as JSON**:

| `Result` case | Response |
| --- | --- |
| `Success(json)` | `{"data": json}` |
| `Warning(problems, json)` | `{"errors": […], "data": json}` — errors **and** data |
| `Failure(problems)` | `{"errors": […]}` — no `data` key |
| `InternalError(thr)` | `M.raiseError(thr)` — raised into the effect `F`, never JSON |

So GraphQL validation and execution failures (`Failure` / `Warning`) come back as a normal response with an `errors` array, while an `InternalError` propagates as an effect failure — over HTTP this becomes a 500, handled by your transport layer rather than appearing in the `errors` array. The pure `(data, errors)` overload assembles the JSON directly and, as a catch-all, emits a synthetic `{"errors": [{"message": "Invalid query"}]}` when there is neither data nor any error.

## `combineAndRun`

```scala
def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]]
```

`combineAndRun` is a lower-level staging hook, **not** part of the normal request path. It combines and executes multiple already-compiled queries, each interpreted in the context of its paired [`Cursor`](cursor.md), returning a result list aligned with the input. It is invoked internally at stage boundaries to evaluate deferred subqueries, and a composed mapping (one extending `ComposedMapping`) may override it to batch those subqueries across backends. You do not call it to serve a request — use `compileAndRun` / `compileAndRunSubscription`.

## See also

- [Serve a Mapping over HTTP](../how-to/serve-over-http.md) — wire these methods to http4s GET/POST routes and an Ember server.
- [Mutations & subscriptions tutorial](../tutorial/mutations-subscriptions.md) — build a mapping that exercises `compileAndRun` and `compileAndRunSubscription` end to end.
- [Result, Problem & ResultT reference](result-problem.md) — the `Result` ADT behind the response envelope and `mkResponse`.
- [The compiler and elaboration](../concepts/compiler-elaboration.md) — what `QueryCompiler.compile` does to produce an `Operation`.
- [The query interpreter](../concepts/query-interpreter.md) — how `QueryInterpreter.run` executes an `Operation` against a mapping.
- [Context & Env reference](context-env.md) — the `Env` you pass as the request-scoped `env` parameter.
