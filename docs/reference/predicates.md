# Predicates & terms reference

This page is the reference for the `Term[T]` / `Path` / `Predicate` algebra in `grackle.Predicate` (module `grackle-core`), plus the SQL-module `Like`. These are the leaves you assemble into a [`Filter`](filtering-paging-nodes.md) — and the terms you sort by in an [`OrderBy`](filtering-paging-nodes.md) — when you wire user arguments into a query during [elaboration](../concepts/compiler-elaboration.md). It is written for developers building filters; for the surrounding `Query` nodes (`Filter`, `OrderBy`, `Offset`, `Limit`, `Count`) see [Filtering & paging nodes](filtering-paging-nodes.md), and for the task-oriented walkthrough see [How to filter, order and page](../how-to/filtering-ordering-paging.md).

## `Term[T]`

`Term[T]` is a *reified* function `Cursor => Result[T]`. It is deliberately **not** an arbitrary `Cursor => Boolean`: interpreters introspect terms (the SQL module turns them into `WHERE` / `ORDER BY` clauses rather than running them in memory), so a `Term` must expose its structure. Every predicate, path and literal in this page is a `Term`.

`Term[T] extends Product with Serializable` and declares:

| Member | Signature | Purpose |
| --- | --- | --- |
| `apply` | `def apply(c: Cursor): Result[T]` | Evaluate the term against a [`Cursor`](cursor.md), yielding a [`Result[T]`](result-problem.md). |
| `children` | `def children: List[Term[_]]` | The immediate sub-terms (e.g. `Eql(x, y).children == List(x, y)`). |
| `fold` | `def fold[Acc](acc: Acc)(f: (Acc, Term[_]) => Acc): Acc` | Left-fold over this term and all descendants. |
| `exists` | `def exists(f: Term[_] => Boolean): Boolean` | True if `f` holds for this term or any descendant. |
| `forall` | `def forall(f: Term[_] => Boolean): Boolean` | True if `f` holds for this term and every descendant. |
| `forallR` | `def forallR(f: Term[_] => Result[Boolean]): Result[Boolean]` | Short-circuiting `forall` whose test returns a `Result[Boolean]`. |

`Term` is invariant by design — the source carries the note that *making it covariant crashes Scala 3*. Do not attempt to make `Term` or `Predicate` covariant.

## `Path` and the `Type / "field"` syntax

A `Path` is a typed cursor path rooted at a schema [`Type`](schema-sdl.md). You build one with the `/` operator on a `Type` (`CountryType / "population"`) and an implicit converts it to a `Term`. `Path` declares:

| Member | Signature | Purpose |
| --- | --- | --- |
| `rootTpe` | `def rootTpe: Type` | The type the path starts from. |
| `path` | `def path: List[String]` | The field names walked so far. |
| `tpe` | `def tpe: Type` | The type at the current end of the path. |
| `asTerm` | `def asTerm[A]: Term[A]` | Convert to a `Term` — see `UniquePath` vs `ListPath` below. |
| `/` | `def /(elem: String): Path` | Extend the path by one field. |
| `%` | `def %(ntpe: NamedType): Path` | Narrow the current type to a subtype (throws if `ntpe` is not a subtype). |
| `isRoot` | `def isRoot: Boolean` | `path.isEmpty`. |

Construct a path with `Path.from(tpe)` (returns an empty path rooted at `tpe`) or, idiomatically, with `tpe / "field"`, which delegates to `Path.from`. The `Type./` operator lives in `grackle.schema`.

### `UniquePath` vs `ListPath`

`asTerm` chooses the concrete term based on whether the path is list-valued, computed from `rootTpe.pathIsList(path)`:

| `Term` | Signature | Chosen when | Yields |
| --- | --- | --- | --- |
| `PathTerm.UniquePath` | `case class UniquePath[A](path: List[String]) extends Term[A]` | the path is scalar | `A` — errors at runtime with *"Expected exactly one element for path …"* unless the focus is a single `ScalarFocus`. |
| `PathTerm.ListPath` | `case class ListPath[A](path: List[String]) extends Term[List[A]]` | `rootTpe.pathIsList(path)` is true | `List[A]`. |

Two implicits drive the conversion: `Term.path2Term[A]` (scalar, higher priority) and `TermLow.path2ListTerm[A]` (list). This is why `Contains` takes a `Term[List[T]]` for a list field while `Eql` takes a `Term[T]` for a scalar — the path's shape selects the term. A scalar path used where a `List` term is expected (or vice versa) still *compiles* via these implicits but fails at runtime.

## `Const` literals

`Const[T]` is the literal term — the right-hand side of most comparisons:

| `Term` | Signature | Purpose |
| --- | --- | --- |
| `Predicate.Const` | `case class Const[T](v: T) extends Term[T]` | A constant; `apply` always succeeds with `v`. E.g. `Eql(CountryType / "code", Const(code))`. |

## Predicate ADT

`Predicate extends Term[Boolean]` — it is exactly a `Term` that yields a boolean, and it is the thing a [`Filter`](filtering-paging-nodes.md) holds. All of the following live in `object grackle.Predicate`.

### Constants and boolean combinators

| Predicate | Signature | Semantics |
| --- | --- | --- |
| `True` | `case object True extends Predicate` | Always matches; identity for `and`, absorbing for `or`. |
| `False` | `case object False extends Predicate` | Never matches; absorbing for `and`, identity for `or`. |
| `And` | `case class And(x: Predicate, y: Predicate) extends Predicate` | `x && y`. |
| `Or` | `case class Or(x: Predicate, y: Predicate) extends Predicate` | `x \|\| y`. |
| `Not` | `case class Not(x: Predicate) extends Predicate` | `!x`. |

Prefer the smart constructors over hand-nesting when you have a list of predicates:

| Constructor | Signature | Behaviour |
| --- | --- | --- |
| `Predicate.and` | `def and(props: List[Predicate]): Predicate` | Folds into an `And` chain, short-circuiting on `False`; an empty list yields `True`. |
| `Predicate.or` | `def or(props: List[Predicate]): Predicate` | Folds into an `Or` chain, short-circuiting on `True`; an empty list yields `False`. |
| `And.combineAll` | `def combineAll(preds: List[Predicate]): Predicate` | Right-folds the list with `And`; an empty list yields `True`, a singleton yields itself. |

### Comparisons and membership

| Predicate | Signature | Requires | Semantics |
| --- | --- | --- | --- |
| `Eql` | `case class Eql[T: Eq](x: Term[T], y: Term[T])` | `Eq[T]` | `x === y`. Exposes `eqInstance` and `subst`. |
| `NEql` | `case class NEql[T: Eq](x: Term[T], y: Term[T])` | `Eq[T]` | `x =!= y`. |
| `Contains` | `case class Contains[T: Eq](x: Term[List[T]], y: Term[T])` | `Eq[T]` | the list term `x` contains element `y`. |
| `Lt` | `case class Lt[T: Order](x: Term[T], y: Term[T])` | `Order[T]` | `x < y`. |
| `LtEql` | `case class LtEql[T: Order](x: Term[T], y: Term[T])` | `Order[T]` | `x <= y`. |
| `Gt` | `case class Gt[T: Order](x: Term[T], y: Term[T])` | `Order[T]` | `x > y`. |
| `GtEql` | `case class GtEql[T: Order](x: Term[T], y: Term[T])` | `Order[T]` | `x >= y`. |
| `In` | `case class In[T: Eq](x: Term[T], y: List[T])` | `Eq[T]` | `x` is one of the static list `y`. |
| `IsNull` | `case class IsNull[T](x: Term[Option[T]], isNull: Boolean)` | — | `x.isEmpty == isNull`; `x` must be an `Option`-valued term. |

`In` has a companion helper `In.fromEqls[T](eqls: List[Eql[T]]): Option[In[T]]` which collapses a homogeneous list of `Eql`-against-`Const` over the *same* term into a single `In`, and returns `None` otherwise.

### String predicates (core)

| Predicate | Signature | Semantics |
| --- | --- | --- |
| `Matches` | `case class Matches(x: Term[String], r: Regex) extends Predicate` | `r.matches(x)` — full regex match. |
| `StartsWith` | `case class StartsWith(x: Term[String], prefix: String) extends Predicate` | `x.startsWith(prefix)`. |

## Term transformers (non-boolean)

These are `Term`s that produce non-boolean values; use them to transform a sub-term before comparing it. They are *not* predicates.

| Transformer | Signature | Result |
| --- | --- | --- |
| `AndB` | `case class AndB(x: Term[Int], y: Term[Int]) extends Term[Int]` | `x & y` (bitwise AND). |
| `OrB` | `case class OrB(x: Term[Int], y: Term[Int]) extends Term[Int]` | `x \| y` (bitwise OR). |
| `XorB` | `case class XorB(x: Term[Int], y: Term[Int]) extends Term[Int]` | `x ^ y` (bitwise XOR). |
| `NotB` | `case class NotB(x: Term[Int]) extends Term[Int]` | `~x` (bitwise complement). |
| `ToUpperCase` | `case class ToUpperCase(x: Term[String]) extends Term[String]` | `x.toUpperCase`. |
| `ToLowerCase` | `case class ToLowerCase(x: Term[String]) extends Term[String]` | `x.toLowerCase`. |

## `Like` (sql-core module)

`Like` is **not** in the core `Predicate` ADT — it lives in `grackle.sql` (module `grackle-sql-core`, with separate scala-2 and scala-3 sources) and is only meaningful against a SQL backend, where it compiles to `LIKE` / `ILIKE`.

| Predicate | Signature |
| --- | --- |
| `grackle.sql.Like` | `case class Like(x: Term[String] \| Term[Option[String]], pattern: String, caseInsensitive: Boolean) extends Predicate` |

Notes:

- The first parameter is the union type `Term[String] | Term[Option[String]]`, so `Like` accepts both a non-nullable and a nullable string term. A `None` focus never matches.
- `pattern` uses SQL wildcards: `%` matches any run of characters, `_` matches a single character. In-memory it is translated to a regex (`%` → `.*`, `_` → `.`); against SQL it compiles to `LIKE`/`ILIKE`.
- `caseInsensitive = true` wraps the regex in `(?i:…)` (and selects `ILIKE` on SQL backends).

## Required typeclasses

The element type of a comparison must match the field's *mapped* type, including `Option`, and must carry the right instance:

| Predicate(s) | Instance required |
| --- | --- |
| `Eql`, `NEql`, `In`, `Contains` | `Eq[T]` |
| `Lt`, `LtEql`, `Gt`, `GtEql` (and `OrderSelection`) | `Order[T]` |
| `IsNull` | none — but `x` must be a `Term[Option[T]]`, so the path must be a nullable field |
| `Matches`, `StartsWith` | none — `x` is fixed to `Term[String]` |
| `Like` | none — `x` is `Term[String] \| Term[Option[String]]`, so it accepts a nullable or non-nullable string field |

Forgetting the instance, or comparing the wrong static type, is a compile error. The element type must follow the field exactly: use `OrderSelection[Option[String]]` for a nullable string field, and `IsNull[Int]` over an `Option[Int]` field.

## The vocabulary in a real elaborator

Predicates are installed during compilation inside a `SelectElaborator`, which matches `(ParentType, fieldName, List(Binding(...)))` and returns an `Elab[Unit]` — most often `Elab.transformChild(child => …)` to wrap the child query with a `Filter`. The world-database test mapping exercises most of the algebra at once:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlWorldMapping.scala", "#world_elaborator"))
```

Reading the cases against the tables above:

- `country` / `city` / `language` use `Eql(path, Const(value))` wrapped in `Unique(Filter(...))` for single-result lookups.
- `countries` builds `GtEql(... population, Const(min))` for filtering and `OrderSelection[Int]` / `OrderSelection[String]` keys for ordering, gated on the GraphQL arguments.
- `cities` uses `Like(CityType / "name", namePattern, true)` — the case-insensitive SQL wildcard match.
- `languages` uses `In(CityType / "language", languages)` for membership against a static list.
- `search` combines `And(Not(Lt(...)), Not(Lt(...)))` — note `Const(Option(year))` because `indepyear` is a nullable field.
- `search2` uses `IsNull[Int](CountryType / "indepyear", isNull = !indep)` over the `Option`-valued `indepyear` term.

Each of these terms is inert data: the SQL interpreter introspects it via `children` and compiles it to a `WHERE` clause, while an in-memory mapping runs `apply` against each candidate `Cursor`. The `Query` nodes that wrap these predicates (`Filter`, `OrderBy`, `Offset`, `Limit`, `Count`) are documented separately.

## See also

- [Filtering & paging nodes](filtering-paging-nodes.md) — the `Filter` / `OrderBy` / `Offset` / `Limit` / `Count` `Query` nodes and the `FilterOrderByOffsetLimit` assembler that hold these predicates.
- [How to filter, order and page](../how-to/filtering-ordering-paging.md) — the task-oriented recipe that wires arguments to predicates and paging nodes.
- [Cursor](cursor.md) — what a `Term` is evaluated against.
- [Query algebra](query-algebra.md) — the `Query` ADT these predicates filter over.
- [Compiler & elaboration](../concepts/compiler-elaboration.md) — how `SelectElaborator` installs predicates during compilation.
