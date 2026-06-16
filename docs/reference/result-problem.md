# Result, Problem & ResultT reference

Every fallible operation in Grackle — parsing, compilation, elaboration, query interpretation, mapping validation, effects — returns a `Result[A]`. This page is the information-oriented reference for the `Result` ADT and its combinators, constructors and typeclass instances; the `ResultT[F, A]` monad transformer; the GraphQL-spec-shaped `Problem` type and its JSON encoding; the `Mapping.mkResponse` boundary that turns a `Result[Json]` into a response envelope; and the separate `ValidationFailure`/`Severity` diagnostic channel. It is for any developer constructing or handling errors. For a task-oriented walkthrough see [Construct, accumulate and report errors](../how-to/errors.md); all signatures below come from `result.scala`, `problem.scala`, `mapping.scala`, `validationfailure.scala` and `syntax.scala` under `modules/core/src/main/scala/`.

## The `Result` ADT

`Result[+T]` is a sealed trait with four cases — an `Ior`-like type that carries a value, a non-empty chain of [`Problem`](#problem)s, or both, plus a fourth case for internal `Throwable`s.

| Case | Signature | Carries | Surfaces as |
| --- | --- | --- | --- |
| `Success` | `final case class Success[+T](value: T)` | value only | `{"data": …}` |
| `Warning` | `final case class Warning[+T](problems: NonEmptyChain[Problem], value: T)` | value **and** problems | `{"errors": […], "data": …}` |
| `Failure` | `final case class Failure(problems: NonEmptyChain[Problem])` | problems only | `{"errors": […]}` (no `data`) |
| `InternalError` | `final case class InternalError(error: Throwable)` | a `Throwable` | raised into the effect `F` |

`Warning` generalises `Ior.Both`: it keeps partial data while still reporting problems. `InternalError` is **not** a GraphQL error — it is a bug or unexpected condition and is raised into `F` rather than encoded into the `errors` array (see [`mkResponse`](#mapping-results-to-the-response) and the [hard invariant on internal errors](../concepts/architecture.md)).

### Predicates

| Member | Signature | True for | Notes |
| --- | --- | --- | --- |
| `hasValue` | `def hasValue: Boolean` | `Success`, `Warning` | the cases for which `toOption` is non-empty |
| `hasProblems` | `def hasProblems: Boolean` | `Warning`, `Failure` | the cases that contribute to the `errors` array |
| `isFailure` | `def isFailure: Boolean` | `Failure` | problems but no value |
| `isInternalError` | `def isInternalError: Boolean` | `InternalError` | isolates the bug case |
| `toProblems` | `def toProblems: Chain[Problem]` | — | accumulated problems as a `Chain`; empty for `Success`/`InternalError` |

## Combinators

Instance methods on `Result[T]`. They short-circuit on `Failure`/`InternalError` and carry `Warning` problem chains forward.

| Member | Signature | Behaviour |
| --- | --- | --- |
| `map` | `def map[U](f: T => U): Result[U]` | maps the value of `Success`/`Warning`; passes `Failure`/`InternalError` through |
| `flatMap` | `def flatMap[U](f: T => Result[U]): Result[U]` | monadic bind; merges a `Warning`'s chain into the continuation (`problems ++ fps`), short-circuits on `Failure`/`InternalError` |
| `traverse` | `def traverse[F[_], U](f: T => F[U])(implicit F: Applicative[F]): F[Result[U]]` | effectful traversal of the value |
| `fold` | `def fold[U](failure: NonEmptyChain[Problem] => U, success: T => U, warning: (NonEmptyChain[Problem], T) => U, error: Throwable => U): U` | exhaustive eliminator over all four cases |
| `foldLeft` / `foldRight` | `def foldLeft[U](u: U)(f: (U, T) => U): U` / `def foldRight[U](lu: Eval[U])(f: (T, Eval[U]) => Eval[U]): Eval[U]` | fold over the value (no-op for `Failure`/`InternalError`) |
| `combine` | `def combine[U >: T](that: Result[U])(implicit S: Semigroup[U]): Result[U]` | `Ior`-like accumulation; merges values via `S` and concatenates problems. `Failure` combined with a value-bearing result is **downgraded to `Warning`**; `InternalError` dominates |
| `withProblems` | `def withProblems(problems: NonEmptyChain[Problem]): Result[T]` | attach problems: `Success`→`Warning`, `Warning`/`Failure` accumulate, `InternalError` unchanged |
| `getOrElse` | `def getOrElse[U >: T](ifNone: => U): U` | the value, or a default for `Failure`/`InternalError` |
| `exists` / `forall` | `def exists(p: T => Boolean): Boolean` / `def forall(p: T => Boolean): Boolean` | predicate over the value (via `toOption`) |
| `toOption` | `def toOption: Option[T]` | `Some` for `Success`/`Warning`, `None` otherwise |
| `toEither` | `def toEither: Either[Either[Throwable, NonEmptyChain[Problem]], T]` | nested `Either`; the left distinguishes `InternalError` (inner `Left`) from `Failure` (inner `Right`) |
| `===` | `def ===[TT >: T](that: Result[TT])(implicit TT: Eq[TT]): Boolean` | structural equality used by the `Eq` instance |

## Constructors

On the `Result` companion object. `apply`, `pure` and `success` are synonyms producing a `Success`.

| Member | Signature | Yields |
| --- | --- | --- |
| `apply` / `pure` / `success` | `def apply[A](a: A): Result[A]` (and `pure`, `success`) | `Success(a)` |
| `unit` | `val unit: Result[Unit]` | `Success(())` |
| `warning` | `def warning[A](warning: Problem, value: A): Result[A]` / `def warning[A](warning: String, value: A): Result[A]` | `Warning(NonEmptyChain(problem), value)`; the `String` form wraps the message in a `Problem` with no path/locations |
| `failure` | `def failure[A](s: String): Result[A]` / `def failure[A](p: Problem): Result[A]` | `Failure(NonEmptyChain(problem))` |
| `internalError` | `def internalError[A](err: Throwable): Result[A]` / `def internalError[A](err: String): Result[A]` | `InternalError`; the `String` form wraps the message in a new `Throwable` |
| `fromOption` | `def fromOption[A](oa: Option[A], ifNone: => Problem): Result[A]` (plus a `String` overload) | `Success` or `Failure(ifNone)` when empty |
| `fromEither` | `def fromEither[A](ea: Either[Problem, A]): Result[A]` (plus a `String`-left overload) | `Left`→`Failure`, `Right`→`Success` |
| `fromProblems` | `def fromProblems(problems: Seq[Problem]): Result[Unit]` / `def fromProblems(problems: Chain[Problem]): Result[Unit]` | `Result.unit` when empty, else `Failure` |
| `catchNonFatal` | `def catchNonFatal[T](body: => T): Result[T]` | `Success`, or `InternalError` on a `NonFatal` throwable |
| `combineAllWithDefault` | `def combineAllWithDefault[T](ress: List[Result[T]], default: => T): Result[List[T]]` | combines a list preserving order/length by substituting `default` for failed elements, accumulating all problems; returns the first `InternalError` if any |

The `String` overloads of `fromOption`/`fromEither` (and `Option#toResult`) are disambiguated from the `Problem` variants by an implicit `DummyImplicit`, since they would otherwise erase to the same signature.

### Constructing the four cases

The four cases and the predicates that distinguish them, in a single compiled block:

```scala mdoc:silent
import grackle.{Problem, Result}

val ok:    Result[Int] = Result.Success(42)
val ok2:   Result[Int] = Result(42)                 // == Result.success(42) == Result.pure(42)
val warn:  Result[Int] = Result.warning("deprecated field used", 42)
val fail:  Result[Int] = Result.failure("No field 'foo' for type Character")
val failP: Result[Int] = Result.failure(Problem("boom", path = List("user", "name")))
val boom:  Result[Int] = Result.internalError(new RuntimeException("bug"))

assert(ok.toOption == Some(42))
assert(warn.hasValue && warn.hasProblems)   // Warning carries both
assert(fail.isFailure && fail.toOption.isEmpty)
assert(boom.isInternalError)
```

`warn` is a `Warning` — it has both a value and a problem; `fail` is a `Failure` — a problem and no value; `boom` is an `InternalError` and so contributes nothing to `toProblems`.

### Composition and accumulation

`flatMap` (and the `for`-comprehension sugar) carries a `Warning`'s problems forward while keeping the value, and short-circuits the first `Failure`/`InternalError`:

```scala mdoc:silent
import grackle.syntax._   // for `.success`, `Option#toResult`

val composed: Result[Int] =
  for {
    a <- Result.warning("w1", 1)            // Warning("w1", 1)
    b <- 2.success                          // Success(2)
    c <- Option(3).toResult("missing c")    // Success(3)
  } yield a + b + c

assert(composed.toOption == Some(6))
assert(composed.toProblems.toList.map(_.message) == List("w1"))
```

The result is a `Warning(6)` carrying `w1` — the warning's value survived the chain. A `Failure` anywhere in the comprehension would have short-circuited to `Failure`. To collect problems from *independent* operations instead of stopping at the first, use the [`Parallel`/`Applicative`](#typeclass-instances) instances (`parMapN`, `mapN`, `traverse`).

## Typeclass instances

Defined in `ResultInstances` / `ResultInstances0` and summoned implicitly. The pivotal distinction is **accumulation vs short-circuit**: the `Monad`/`flatMap` path stops at the first `Failure`/`InternalError`, while the `Applicative`/`Parallel`/`Semigroup` path accumulates problems from every operand.

| Instance | Signature | Notes |
| --- | --- | --- |
| `MonadError` | `implicit val grackleMonadErrorForResult: MonadError[Result, Either[Throwable, NonEmptyChain[Problem]]]` | the error type `Either[Throwable, NEC[Problem]]` unifies `InternalError` (left) and `Failure` (right) for `raiseError`/`handleErrorWith`. `flatMap` short-circuits |
| `Parallel` | `implicit def grackleParallelForResult[E]: Parallel.Aux[Result, Result]` | its `Applicative.ap` **accumulates** problems from both operands — enables `parMapN` error accumulation; `InternalError` still dominates |
| `Semigroup` | `implicit def grackleSemigroupForResult[A: Semigroup]: Semigroup[Result[A]]` | defined as `_ combine _`; accumulates, downgrading `Failure` to `Warning` when the other side has a value |
| `Traverse` | `implicit val grackleTraverseFunctorForResult: Traverse[Result]` | `size`/`get` treat `hasValue` as one element |
| `Eq` | `implicit def grackleEqForResult[A: Eq]: Eq[Result[A]]` | defined as `_ === _` |

Use `parMapN` / `mapN` / `traverse` when you want **all** errors reported at once — this is how the compiler surfaces multiple validation errors in a single response rather than failing on the first.

## `ResultT[F, A]`

`ResultT` is the monad transformer that lifts `Result` into an effect `F` (typically a cats-effect `IO` or an `fs2.Stream`). It threads `Result` through `F` in `for`-comprehensions while preserving `Warning`/`Failure`/`InternalError` propagation and problem accumulation across the `F` boundary; it is used pervasively in `queryinterpreter.scala` and `mapping.scala`.

```scala
final case class ResultT[F[_], A](value: F[Result[A]])
```

| Member / constructor | Signature |
| --- | --- |
| `map` | `def map[B](f: A => B)(implicit F: Functor[F]): ResultT[F, B]` |
| `flatMap` | `def flatMap[B](f: A => ResultT[F, B])(implicit F: Monad[F]): ResultT[F, B]` |
| `liftF` | `def liftF[F[_]: Functor, A](fa: F[A]): ResultT[F, A]` |
| `pure` / `unit` | `def pure[F[_], A](a: A)(implicit F: Applicative[F]): ResultT[F, A]` / `def unit[F[_]](implicit F: Applicative[F]): ResultT[F, Unit]` |
| `fromResult` | `def fromResult[F[_], A](a: Result[A])(implicit F: Applicative[F]): ResultT[F, A]` |
| `success` | `def success[F[_]: Applicative, A](a: A): ResultT[F, A]` |
| `warning` | `def warning[F[_]: Applicative, A](warning: Problem, value: A): ResultT[F, A]` — overloaded for `String` and `NonEmptyChain[Problem]` |
| `failure` | `def failure[F[_]: Applicative, A](p: Problem): ResultT[F, A]` — overloaded for `String` and `NonEmptyChain[Problem]` |
| `internalError` | `def internalError[F[_]: Applicative, A](err: Throwable): ResultT[F, A]` — overloaded for `String` |

To recover the underlying effect of nested `Result`s, read `.value: F[Result[A]]`. Its `flatMap` mirrors `Result#flatMap`: a `Warning` in the outer step has its problems concatenated onto the inner step's outcome, and `Failure`/`InternalError` short-circuit.

## `Problem`

`Problem` is the GraphQL-spec error object that is serialised into the `errors` array of a response.

```scala
final case class Problem(
  message: String,
  locations: List[(Int, Int)] = Nil,
  path: List[String] = Nil,
  extensions: Option[JsonObject] = None
)
```

| Field | Type | Meaning |
| --- | --- | --- |
| `message` | `String` | human-readable error text; always emitted |
| `locations` | `List[(Int, Int)]` | source `(line, col)` pairs; encoded as `{"line", "col"}` objects |
| `path` | `List[String]` | response path to the offending field |
| `extensions` | `Option[JsonObject]` | arbitrary structured detail |

### JSON encoding

`Problem.ProblemEncoder` (an `implicit val ProblemEncoder: Encoder[Problem]`) emits `message` always, but **omits any empty `locations`/`path`/`extensions`**; `locations` become `{"line", "col"}` objects. The `toString` renders a compact human-readable form. The following are verbatim worked examples from `ProblemSuite`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ProblemSuite.scala", "#problem_encoding"))
```

The first two assertions show the encoding: with empty `locations` only `message` and `path` appear, and `Problem("foo")` encodes to just `{"message": "foo"}`. The last assertion shows `toString`: `Problem("foo", List(1 -> 2, 5 -> 6), List("bar", "baz"))` renders as `"foo (at bar/baz: 1..2, 5..6)"`. Both locations there are ranges; the snippet does not exercise a point location, but the same `toString` collapses one — a `(a, b)` pair with `a == b`, e.g. `Problem("foo", List(3 -> 3))` — to the bare number `"foo (at 3)"` rather than `"3..3"`.

### Equality

`eqProblem` is `Eq.by(p => (p.message, p.locations, p.path))` — it **ignores `extensions`**. Two `Problem`s that differ only in their `extensions` compare equal; a test that needs to assert on `extensions` must compare the encoded JSON (`asJson`), not the `Problem` values.

## Mapping results to the response

`Mapping.mkResponse` is the single boundary that turns a `Result[Json]` into a GraphQL response envelope. There are two overloads:

```scala
def mkResponse(result: Result[Json]): F[Json]
def mkResponse(data: Option[Json], errors: Chain[Problem]): Json
```

The `Result` overload raises `InternalError` into the effect with `M.raiseError`, and otherwise delegates to the data/errors overload using `result.toOption` and `result.toProblems`:

| `Result` case | Response |
| --- | --- |
| `Success(json)` | `{"data": json}` |
| `Warning(problems, json)` | `{"errors": [...], "data": json}` — errors **and** data |
| `Failure(problems)` | `{"errors": [...]}` — no `data` key |
| `InternalError(thr)` | `M.raiseError(thr)` — raised into `F`, never JSON |

The data/errors overload assembles the JSON directly. As a catch-all, when `data` is `None` **and** the error chain is empty it emits a synthetic `{"errors": [{"message": "Invalid query"}]}`; an empty `Result` should not reach this path in normal flow, but a custom interpreter returning neither data nor problems will get this message.

## `ValidationFailure` and `Severity`

`ValidationFailure` is a **separate diagnostic axis** from `Problem`, used only when building or validating a `Schema` or `Mapping` — not for per-request GraphQL errors. Do not confuse `Result.Warning` (a runtime `Problem`) with `Severity.Warning` (a construction-time diagnostic).

| Type | Signature | Purpose |
| --- | --- | --- |
| `ValidationFailure` | `abstract class ValidationFailure(val severity: Severity) extends AnsiColor` | a coloured, padded `formattedMessage` / `toErrorMessage` diagnostic |
| `Severity` | `sealed trait Severity` with cases `Error`, `Warning`, `Info` | severity with an `Order` instance (`Error`=3 > `Warning`=2 > `Info`=1), so validators can threshold by severity |
| `ValidationException` | `final case class ValidationException(failures: NonEmptyList[ValidationFailure]) extends RuntimeException with NoStackTrace` | aggregates failures; `getMessage` concatenates each failure's `toErrorMessage` |

See [Validate a mapping and read the failures](../how-to/validate-mappings.md) for how these surface when constructing a mapping.

## `grackle.syntax` extension methods

Imported via `import grackle.syntax._`, these lift plain values and `Option`s into `Result`:

| Method | Signature | Yields |
| --- | --- | --- |
| `success` | `def success: Result[A]` (on any `A`) | `Result.success(a)` |
| `toResult` | `def toResult(ifNone: => Problem): Result[T]` / `def toResult(ifNone: => String): Result[T]` (on `Option[T]`) | `Success` or `Failure(ifNone)` when empty |
| `toResultOrError` | `def toResultOrError(ifNone: => Throwable): Result[T]` / `def toResultOrError(ifNone: => String): Result[T]` (on `Option[T]`) | `Success` or `InternalError(ifNone)` when empty |

## See also

- [Construct, accumulate and report errors](../how-to/errors.md) — task-oriented recipes for returning, accumulating and reading these errors.
- [Architecture overview](../concepts/architecture.md) — where `Result` sits in the compile → interpret → respond pipeline.
- [Running operations reference](running-operations.md) — `compileAndRun` and the response envelope `mkResponse` produces.
- [The compiler and elaboration](../concepts/compiler-elaboration.md) — where `Problem`s acquire their locations and paths during elaboration.
