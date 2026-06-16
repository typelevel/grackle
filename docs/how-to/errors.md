# Construct, accumulate and report errors

This recipe shows how to produce errors from your resolvers, elaborators and validation code so they surface correctly in a GraphQL response. Every fallible operation in Grackle yields a [`Result[A]`](../reference/result-problem.md), an `Ior`-like type with four cases: `Success` (value only), `Warning` (value *and* problems), `Failure` (problems only), and `InternalError` (a `Throwable`). You will pick the right case for client errors, partial-data warnings and internal bugs; accumulate several problems at once; lift `Option`/`Either`/effects into `Result`; and attach locations and paths to a `Problem`. It assumes cats-effect basics; for how each case maps to the response JSON, see the [Result, Problem & ResultT reference](../reference/result-problem.md).

## Choose the right case

The case you return decides what the client sees:

| Case | Carries | Response | Use for |
| --- | --- | --- | --- |
| `Success(value)` | value | `{ "data": … }` | the happy path |
| `Warning(problems, value)` | value + problems | `{ "errors": […], "data": … }` | partial data plus a non-fatal complaint |
| `Failure(problems)` | problems | `{ "errors": […] }` (no `data`) | a client-facing error, no usable value |
| `InternalError(throwable)` | a `Throwable` | raised into the effect `F` | a bug or unexpected condition |

The single most important rule: **`InternalError` never reaches the `errors` array.** At the response boundary `Mapping.mkResponse` calls `M.raiseError` for it, so it propagates as an effect-level failure (an `IO` error, say) rather than as JSON. Use `Result.failure` for anything the client should see, and `Result.internalError` only for genuine bugs.

## Return a client error

To reject a request, return a `Result.failure`. The one-argument form takes a message string; the `Problem` overload lets you attach a `path` so the client can see *where* in the result the error sits.

```scala mdoc:silent
import grackle.{Problem, Result}

val notFound: Result[Int] =
  Result.failure("No field 'foo' for type Character")

val withPath: Result[Int] =
  Result.failure(Problem("Name is required", path = List("user", "name")))
```

A `Failure` has no value, so `mkResponse` emits an errors-only response with no `data` key. This is exactly what query compilation produces for a bad query — selecting an unknown field yields `Result.Failure(NonEmptyChain(Problem("No field 'hidden' for type Root")))`, and the client receives:

```json
{ "errors": [ { "message": "No field 'hidden' for type Root" } ] }
```

## Return partial data with a warning

When you can still produce a value but want to flag something, return a `Result.warning`. It carries both the problem and the value, so the response contains **both** `errors` and `data`. Inside an elaborator, use the equivalent `Elab.warning`, which threads the problem through the elaboration `Result`.

The query-directives example wires an `@upperCase` directive whose elaborator uppercases `String` fields. Applied to a non-`String` field it does not abort — it emits an `Elab.warning` and leaves that field untouched:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/directives/QueryDirectivesSuite.scala", "#upper_query"))
```

The query tags `name` (a `String`) and `age` (an `Int`) with `@upperCase`. `name` is uppercased to `"MARY"`; `age` is the wrong type, so the elaborator yields a warning instead of transforming it. The expected response keeps the whole `data` block (`age` is still `42`) *and* carries the warning in `errors` — the defining difference between `Warning` and `Failure`, since a `Warning` preserves partial data. The choice is deliberate: the elaborator's own source (the `#upper_phase` snippet on [Write a custom query directive](query-directives.md)) carries the comment "We could make this fail the whole query by yielding `Elab.failure` here", which would have dropped `data` entirely.

## Signal an internal bug

Reserve `Result.internalError` for conditions that are not the client's fault: a `None` where your invariants guarantee a value, a corrupt cached row, an impossible match. It wraps a `Throwable` (or a message string) and, as above, surfaces as a raised effect, not as a GraphQL error.

```scala mdoc:silent
val bug: Result[Int] =
  Result.internalError(new RuntimeException("unexpected empty index"))
```

To run a side-effecting thunk and capture any non-fatal exception as an `InternalError` automatically, use `Result.catchNonFatal`:

```scala mdoc:silent
val parsed: Result[Int] =
  Result.catchNonFatal("42".toInt)        // Success(42)

val blewUp: Result[Int] =
  Result.catchNonFatal("oops".toInt)      // InternalError(NumberFormatException)
```

`catchNonFatal` returns `Success` if the body completes and `InternalError` if it throws something `NonFatal`. Because the result is an `InternalError`, that exception will be re-raised into `F` at the response boundary rather than swallowed into the `errors` array.

## Compose and accumulate

`Result` has a `MonadError` instance, so it composes in a `for`-comprehension. `flatMap` carries a `Warning`'s problem chain forward while keeping the value, and short-circuits on the first `Failure` or `InternalError`:

```scala mdoc
import grackle.Result
import grackle.syntax._   // for `.success` and `Option#toResult`

val combined: Result[Int] =
  for {
    a <- Result.warning("deprecated field used", 1)  // Warning(…, 1)
    b <- 2.success                                   // Success(2)
    c <- Option(3).toResult("missing c")             // Success(3)
  } yield a + b + c
```

```scala mdoc
combined.toProblems.toList.map(_.message)
```

The result is a `Warning` holding `6` and the single carried-forward problem. Had any step been a `Failure`, the comprehension would have short-circuited and `combined` would have been that `Failure`.

### Report every error at once

`for`/`flatMap` stop at the first failure. When you want to collect **all** problems — the way the compiler reports several schema errors in one go — reach for the `Parallel`/`Applicative` instance via `parMapN`, `mapN` or `traverse`. These accumulate problems from every operand instead of short-circuiting:

```scala mdoc
import cats.syntax.all._

val v1: Result[Int] = Result.failure("name must not be empty")
val v2: Result[Int] = Result.failure("age must be positive")

val allErrors: Result[(Int, Int)] = (v1, v2).parMapN((_, _))
```

```scala mdoc
allErrors.toProblems.toList.map(_.message)
```

Both messages appear, because `parMapN` combined the two `Failure`s into one chain (`ps0 ++ ps`). Swap `parMapN` for a `for`-comprehension and you would see only the first.

For a list of results where you want to preserve length and order while still gathering every problem, use `Result.combineAllWithDefault`. It substitutes a default for each failed element and accumulates all the problems into a single `Warning` (or returns the first `InternalError` if there is one):

```scala mdoc
val rows: List[Result[Int]] =
  List(Result.success(10), Result.failure("row 2 bad"), Result.success(30))

val merged: Result[List[Int]] =
  Result.combineAllWithDefault(rows, default = 0)
```

```scala mdoc
(merged.toOption, merged.toProblems.toList.map(_.message))
```

The failed middle row becomes `0` and its message is retained, so you get `(Some(List(10, 0, 30)), List("row 2 bad"))`.

## Lift Option, Either and effects

Most resolver code starts from an `Option`, an `Either`, or an effectful computation. The `grackle.syntax._` extensions and the `Result` companion lift these without manual pattern matching:

```scala mdoc:silent
import grackle.syntax._

def lookup(id: Int): Option[String] = if (id == 1) Some("Mary") else None

// Option -> Result: Failure when empty
val byOption: Result[String] = lookup(1).toResult("no user with id 1")

// Option -> Result, but a None here is a bug, not a client error -> InternalError
val byInvariant: Result[String] = lookup(1).toResultOrError("index out of sync")

// Either[Problem, A] or Either[String, A] -> Result
val byEither: Result[String] = Result.fromEither(Right("Mary"): Either[String, String])

// Any value -> Success
val ok: Result[String] = "Mary".success
```

`toResult` produces a `Failure` (a client error) when the `Option` is empty, while `toResultOrError` produces an `InternalError` — pick the one matching whose fault an empty value is. `Result.fromOption`/`Result.fromEither` are the companion equivalents, and both accept either a `Problem` or a plain message string.

To thread a `Result` through an effect `F` (typically `IO` or an `fs2.Stream`), wrap it in [`ResultT`](../reference/result-problem.md). Its `flatMap` preserves warning, failure and internal-error propagation across the `F` boundary, so you can mix pure `Result`s with effectful steps in one comprehension:

```scala mdoc:silent
import cats.effect.IO
import grackle.ResultT

val effectful: ResultT[IO, Int] =
  for {
    a <- ResultT.fromResult[IO, Int](Result.success(1))  // pure Result
    b <- ResultT.liftF(IO.pure(2))                        // lift an F[A]
    c <- ResultT.warning[IO, Int]("slow path", 3)         // warning inside F
  } yield a + b + c

val out: IO[Result[Int]] = effectful.value
```

`ResultT.liftF` lifts an `F[A]`, `ResultT.fromResult` lifts a pure `Result[A]`, and the `warning`/`failure`/`internalError` constructors mirror the ones on `Result`. Running `out` yields `Result.Warning` holding `6` and the `"slow path"` problem.

## Attach locations, path and extensions

A `Problem` is the GraphQL-spec error object: `Problem(message, locations, path, extensions)`. Only `message` is required; `locations` is a list of `(line, col)` pairs, `path` is the field path, and `extensions` is an optional circe `JsonObject`. Build one directly when you want a richer error than a bare message:

```scala mdoc:silent
import io.circe.JsonObject
import io.circe.syntax._

val rich: Problem =
  Problem(
    message    = "Value out of range",
    locations  = List(3 -> 12),
    path       = List("user", "age"),
    extensions = Some(JsonObject("code" -> "OUT_OF_RANGE".asJson))
  )
```

Empty fields are dropped from the JSON, so a message-only `Problem` encodes to just `{ "message": … }` while the one above carries all four keys. You can see the exact encoding rules in the `ProblemSuite` cases — empty `locations`/`path`/`extensions` simply vanish:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ProblemSuite.scala", "#problem_encoding"))
```

The first two cases show the omission rule (no `locations` key, then `message`-only); the third shows the matching `toString`, `"foo (at bar/baz: 1..2, 5..6)"`. Note also that `Problem` equality **ignores** `extensions` — `eqProblem` compares only `(message, locations, path)` — so if you assert on extensions in tests, compare the encoded JSON (`asJson`), not the `Problem` values. To add problems to an existing `Result` after the fact (turning a `Success` into a `Warning`, or accumulating onto a `Failure`), use `withProblems`.

## See also

- [Result, Problem & ResultT reference](../reference/result-problem.md) — every constructor, combinator and the exact response-mapping rules.
- [The compiler and elaboration](../concepts/compiler-elaboration.md) — where `Elab.warning`/`Elab.failure` fit in the pipeline and how problems acquire locations.
- [Write a custom query directive](query-directives.md) — the full `@upperCase` elaborator behind the warning example.
- [Run effects and batch nested fields](effects-batching.md) — using `ResultT` and effectful resolvers in anger.
- [Validate a mapping and read the failures](validate-mappings.md) — `ValidationFailure`/`Severity`, the construction-time diagnostic channel distinct from runtime `Problem`s.
