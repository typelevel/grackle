# Mutations & Subscriptions

So far your mappings have only *read* data. In this tutorial you extend an in-memory mapping
with a GraphQL **mutation** (a field that writes) and a GraphQL **subscription** (a field that
streams responses over time), wiring both to a single `fs2.concurrent.SignallingRef` cell of
state. Everything here compiles and runs without a database, so you can copy each step and try
it yourself. It assumes you have finished the [in-memory tutorial](in-memory-model.md) and are
comfortable with cats-effect basics. When you are done, the same patterns carry over to a real
database, which the SQL how-to pages pick up.

## Mutations are just `RootEffect` fields

Grackle has no separate "mutation engine". A GraphQL mutation is an ordinary root field on the
schema's `Mutation` object type whose field mapping is a `RootEffect`. A `RootEffect` runs an
effect in your `F[_]` — the actual write — *before* the rest of the query is interpreted, and
then yields the value that the client's selection set is rendered against. The same machinery
backs query roots, mutation roots, and (in its streaming form, `RootStream`) subscription roots.

`RootEffect`'s primary constructor is private, so you always build one through a companion
factory. The three you will meet are:

- `RootEffect.computeUnit` — perform a write and leave the elaborated query and default root
  cursor unchanged.
- `RootEffect.computeCursor` — perform a write and hand back a cursor you build yourself; this is
  the form in-memory `ValueMapping`s use, and the one in this tutorial.
- `RootEffect.computeChild` — perform a write and then rewrite the child query using data only
  known *after* the write (for example the id of a freshly inserted row).

Arguments do not reach the effect directly. They are captured into the elaboration `Env` by the
`SelectElaborator` with `Elab.env(...)`, and read back inside the effect with `env.get[T](name)`
(an `Option[T]`) or `env.getR[T](name)` (a `Result[T]`). Forgetting the `Elab.env` step is the
classic mistake — `env.get` then returns `None` (or `env.getR` fails) at runtime.

## The mapping: `get`, `put`, and `watch`

Here is the complete mapping. A single `SignallingRef[IO, Int]` holds the state. The `Query` type
exposes a `get` field that reads it, the `Mutation` type exposes a `put(n: Int): Int!` field that
writes it, and the `Subscription` type exposes a `watch: Int!` field that streams it.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/docs/src/main/scala/grackle/MutationSubscriptionMapping.scala", "#ms_mapping"))
```

Walking through the three field mappings:

- **`get`** is a `RootEffect.computeCursor("get")` whose effect is `ref.get`. It reads the current
  value and wraps it in a root cursor with `valueCursor(path, env, n)`, the helper a `ValueMapping`
  uses to point a `Cursor` at an in-memory value. `get` is on `Query`, not `Mutation` — a root
  effect is not inherently a mutation, it is just an effectful root field.
- **`put`** is a `RootEffect.computeCursor("put")` on the `Mutation` type. It reads the `n`
  argument out of the environment with `env.get[Int]("n")`, calls `ref.set(n)` to perform the
  write, and returns a cursor carrying `n` so the client can select the new value straight back.
  If `n` is somehow absent it returns a `Result.failure`, which surfaces as a GraphQL error.
- **`watch`** is a `RootStream.computeCursor("watch")` on the `Subscription` type — the streaming
  sibling of `RootEffect`. Its effect returns the `fs2.Stream` `ref.discrete`, which emits the
  ref's current value and then every subsequent distinct value. Each emitted `n` becomes one root
  cursor, and therefore one GraphQL response.

The `selectElaborator` is the other half of `put`. When it sees `put` selected with an integer
binding `n`, it runs `Elab.env("n" -> n)`, threading that argument into the `Env` the effect later
reads. (`Elab.env("n" -> n)` is the tuple-pair overload; there are also `Elab.env(name, value)`
and overloads that take a whole `Env`.) `get` and `watch` take no arguments, so they need no
elaborator case.

## Running a mutation

To exercise the mapping you need an instance and a `SignallingRef` to back it. `Mapping#compileAndRun`
compiles a single query or mutation document and returns `F[Json]` — exactly one response. Wire up
the ref, run a `put`, and inspect the JSON. (`compileAndRun` is for single-shot operations; the
subscription below uses a different entry point.)

```scala mdoc:silent
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.concurrent.SignallingRef
import grackle.docs.MutationSubscriptionMapping

val program: IO[io.circe.Json] =
  for {
    ref <- SignallingRef[IO, Int](0)
    map  = MutationSubscriptionMapping.mapping(ref)
    res <- map.compileAndRun("mutation { put(n: 42) }")
  } yield res
```

```scala mdoc
program.unsafeRunSync()
```

The mutation writes `42` into the ref and renders it back through the cursor `put` returned, so the
response is `{ "data" : { "put" : 42 } }`. A `query { get }` against the same `ref` would now read
`42` too. Because the write and the read-back both go through the one `SignallingRef`, the state
change is immediately visible.

### Serial execution of mutation roots

GraphQL requires that the top-level fields of a single mutation operation run **serially**, in the
order the client wrote them, and Grackle honours this: within one `compileAndRun`, effectful root
selections are executed in client order rather than in parallel. So a document that calls `put`
three times with aliases —

```graphql
mutation {
  one:   put(n: 1)
  two:   put(n: 2)
  three: put(n: 3)
}
```

— leaves the ref holding `3`, never `1` or `2`, because `three` always runs last. Do not rely on
parallelism between sibling mutation roots.

## Running a subscription

A subscription is not a single response; it is a *stream* of responses, one per upstream event. The
entry point is `Mapping#compileAndRunSubscription`, which returns `fs2.Stream[F, Json]` instead of
`F[Json]`. (In fact `compileAndRun` is built on top of it and asserts the stream has exactly one
element — which is why pointing `compileAndRun` at a real subscription fails.)

Subscriptions are also more constrained than queries: an operation may select **exactly one** root
field, and `RootStream` is legal **only** under the `Subscription` type. Putting a `RootStream` on
`Query` or `Mutation`, or selecting two subscription roots at once, is a runtime error.

The following program subscribes to `watch`, then drives the ref through a sequence of `put`s and
collects the emissions. Because delivery is asynchronous — the subscriber must attach to
`ref.discrete` before a value is produced for it to be observed — the driver starts the subscriber
in a fiber and spaces the mutations out, mirroring the approach in Grackle's own
`SubscriptionSuite`:

```scala mdoc:compile-only
import scala.concurrent.duration._
import cats.effect._
import fs2.concurrent.SignallingRef
import io.circe.Json
import grackle.docs.MutationSubscriptionMapping

val watched: IO[List[Json]] =
  for {
    ref <- SignallingRef[IO, Int](0)
    map  = MutationSubscriptionMapping.mapping(ref)
    fib <- map
             .compileAndRunSubscription("subscription { watch }")
             .take(4)
             .compile
             .toList
             .start
    _   <- IO.sleep(100.millis)               // let the subscriber attach first
    _   <- map.compileAndRun("mutation { put(n: 123) }")
    _   <- IO.sleep(100.millis)
    _   <- map.compileAndRun("mutation { put(n: 42) }")
    _   <- IO.sleep(100.millis)
    _   <- map.compileAndRun("mutation { put(n: 77) }")
    _   <- IO.sleep(100.millis)
    out <- fib.join
    res <- out.embedNever
  } yield res
```

`take(4)` collects four responses: the ref's initial `0` (emitted as soon as the subscriber
attaches, because `discrete` replays the current value), followed by `123`, `42`, and `77`. Each is
a full GraphQL document:

```json
[
  { "data": { "watch": 0   } },
  { "data": { "watch": 123 } },
  { "data": { "watch": 42  } },
  { "data": { "watch": 77  } }
]
```

The `IO.sleep` calls are not part of the mapping — they only paper over the race between starting
the listener and producing values, which is inherent to any push-based subscription. The Grackle
test that this example is drawn from notes the same caveat; in a real application your transport
layer (not Grackle) owns the lifecycle of the stream, and events produced before a subscriber is
attached are simply not seen by it.

## How root effects fit the bigger picture

`RootEffect` and `RootStream` are two of the ways Grackle attaches effects to a query. They run
*once, up front*, at the root. Grackle also supports effects deep inside the result tree, through
`EffectField` and an `EffectHandler`, and there it does something `RootEffect` does not: it
**batches** every occurrence of a nested effect field into a single handler call, collapsing what
would otherwise be an N+1 problem. Sibling root effects, by contrast, are not batched — each root
field runs its own effect. The mechanics of nested effects and batching are a topic of their own;
see the links below when you need them.

Two more things worth stating plainly, because they bite people coming from other GraphQL servers:

- **Grackle does not manage transactions.** Nothing wraps the write in `put` (or a SQL `INSERT`) in
  a transaction for you. The effect value is whatever you supply; if you need the write and a
  read-back to be atomic, arrange that yourself in your `F[_]` (for example a doobie `.transact(xa)`
  or a skunk session).
- **There is no built-in websocket or `graphql-ws` transport.** A subscription is a plain
  `fs2.Stream` you connect to a transport yourself. Grackle gives you the stream; carrying it to a
  client over websockets (or SSE, or anything else) is your integration's job.

## Where to next

You now have a working in-memory mutation and subscription. The same `RootEffect` and `RootStream`
patterns scale to a real database:

- [Run effects and batch nested fields](../how-to/effects-batching.md) — the how-to companion to this
  tutorial. It covers the remaining `RootEffect` constructors against SQL (`computeUnit` for an update
  keyed on an argument, `computeChild` for an insert whose id the database generates) and `EffectField`
  + `EffectHandler` for nested fields that batch to avoid N+1.
- [Effects and batching internals](../concepts/effects-batching.md) — why deferred effects exist and
  how batching works under the hood.
- [Effects reference](../reference/effects.md) — exact signatures for `RootEffect`, `RootStream`,
  and `EffectHandler`.
- [Choose and configure a SQL backend](../how-to/sql-backends.md) — pick Doobie or Skunk and wire a
  mapping to a transactor or session pool, the foundation for backing these effects with a database.
- [Serve a mapping over HTTP](../how-to/serve-over-http.md) — wiring `compileAndRun` /
  `compileAndRunSubscription` into a transport, including a `graphql-ws`-style stream.
- [How the query interpreter works](../concepts/query-interpreter.md) — how `runOneShot` and
  `runSubscription` discover and execute root effects and streams.
