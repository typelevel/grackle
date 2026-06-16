# Validate a mapping and read the failures

This page is a recipe for checking that a [`Mapping`](../concepts/mappings-cursors.md) actually covers the schema it claims to serve, and for reading the failures it reports when it does not. It is aimed at developers debugging mapping setup: you have written a `schema` and a `typeMappings` catalog, and you want to know — before you run a single query — which types and fields are unmapped, ambiguous, or mapped with the wrong kind of `TypeMapping`. Validation never inspects query text; it unfolds the schema and checks the catalog against it.

## Run validation: `validate`, `unsafeValidate`, `validateInto`

Every `Mapping[F]` exposes three validation entry points. They all take a minimum `Severity` and differ only in how they report failures.

```scala mdoc:compile-only
import grackle._

val mapping: Mapping[cats.effect.IO] = grackle.docs.QuickStartMapping

// 1. Collect failures as data (no throwing). Default severity is Warning.
val failures: List[ValidationFailure] = mapping.validate()

// 2. Raise a ValidationException in F if any failure meets the threshold.
val checked: cats.effect.IO[Unit] = mapping.validateInto[cats.effect.IO]()

// 3. Throw a ValidationException synchronously (handy in a test or a REPL).
mapping.unsafeValidate()
```

- `validate(severity)` returns a `List[ValidationFailure]`, filtered to those whose severity is greater than or equal to `severity`. This is the form to reach for when you want to inspect, count, or render failures yourself.
- `validateInto[G](severity)` (for any `ApplicativeError[G, Throwable]`) raises a `ValidationException` wrapping the failures, or yields `().pure[G]` when there are none. Use it to fail an effectful startup.
- `unsafeValidate(severity)` is the same check run through `Either[Throwable, *]` and then `throw`n — convenient in tests and the REPL, not for production paths.

A failure is *not* a GraphQL error. Validation failures are a separate `ValidationFailure` channel, surfaced as a list or as a thrown `ValidationException`; they never appear in the `errors` array of a query [`Result`](../reference/result-problem.md). Render a failure for humans with `toErrorMessage`, which prints a colourised, multi-line explanation pointing at the schema and mapping source positions.

### Severity and thresholds

`ValidationFailure.Severity` has three cases, ordered `Error > Warning > Info`:

```scala mdoc:silent
import grackle.ValidationFailure.Severity

val error: Severity   = Severity.Error    // a mapping that is definitely wrong
val warning: Severity = Severity.Warning  // suspicious but not fatal (e.g. an unused type mapping)
val info: Severity    = Severity.Info     // informational
```

Because `validate` keeps failures at or above the threshold, the default `Severity.Warning` reports both errors and warnings but drops `Info`. Pass `Severity.Error` to ignore warnings (for example, to tolerate an unused type mapping while still rejecting a genuinely broken one):

```scala mdoc:compile-only
import grackle.ValidationFailure.Severity

val errorsOnly = grackle.docs.QuickStartMapping.validate(Severity.Error)
```

## Checked vs unchecked catalogs, and when validation runs

A [`TypeMappings`](../reference/mapping-types.md) catalog is either *checked* or *unchecked*. The constructor you use decides which:

- `TypeMappings(...)` and the implicit conversion from a plain `List[TypeMapping]` (so writing `val typeMappings = List(...)` in a mapping) build a **checked** catalog.
- `TypeMappings.unchecked(...)` builds an **unchecked** catalog. (The old `TypeMappings.unsafe(...)` is deprecated in favour of `unchecked`.)

Validation runs **lazily**, the first time the mapping's `compiler` is forced. At that point a checked catalog calls `unsafeValidate()` internally, so a broken checked mapping throws a `ValidationException` the first time you compile a query through it. An unchecked catalog skips that automatic step entirely — you can still call `validate()` on it explicitly, but nothing runs on your behalf.

This is why the test mappings below all use `TypeMappings.unchecked(...)`: they deliberately hold broken catalogs so that constructing them and calling `validate()` does not throw, letting the test inspect the returned failures.

## The `ValidationFailure` catalog

`validate` unfolds the schema starting from the root operation types (`Query`, and `Mutation`/`Subscription` if present), walking every reachable type and field and matching it against the catalog. Each problem it finds is one of the following `ValidationFailure` subtypes. All are `Severity.Error` except `UnusedTypeMapping`, which is a `Severity.Warning`.

| Failure | Severity | Triggered when |
| --- | --- | --- |
| `MissingTypeMapping` | Error | A schema type reachable from a root has no `TypeMapping` in the catalog. |
| `AmbiguousTypeMappings` | Error | Two or more equally specific mappings match the same type at the same path (a `MappingPredicate` priority tie). |
| `MissingFieldMapping` | Error | An object mapping exists for a type but declares no `FieldMapping` for one of the type's schema fields. |
| `DeclaredFieldMappingIsHidden` | Error | A field that the schema declares is mapped but marked `hidden = true`. |
| `ObjectTypeExpected` | Error | An `ObjectMapping` is used for a scalar or enum type. |
| `LeafTypeExpected` | Error | A `LeafMapping` is used for an object/interface/union type. |
| `ReferencedTypeDoesNotExist` | Error | A `TypeMapping` references a type name absent from the schema. |
| `UnusedTypeMapping` | Warning | A `TypeMapping` is never reached by unfolding the schema. |
| `UnusedFieldMapping` | Error | An object mapping declares a `FieldMapping` for a field the schema's type does not have. |

Two points worth internalising. First, "unused" cuts both ways but at different severities: an unreachable *type* mapping is only a warning, whereas a field mapping for a non-existent field is an error (a likely typo in a field name). Second, hidden fields are fine as long as the schema does not declare them — `CursorField`s used purely as internal attributes are expected to be hidden; it is only hiding a *declared* field that fails.

## Worked example: a missing field mapping

The mapping below maps `Query` and the object type `Foo`, but the `ObjectMapping` for `Foo` is empty (`Nil`), so the declared field `Foo.bar` has no `FieldMapping`. The catalog is built with `TypeMappings.unchecked` so that `validate()` can be called without the lazy automatic check throwing first.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/mapping/MappingValidatorSuite.scala", "#validator"))
```

`M.validate()` returns a one-element list, `List(M.MissingFieldMapping(_, f))` with `f.name == "bar"`. Note that `MissingFieldMapping` is path-dependent on the mapping instance: it is `M.MissingFieldMapping`, matched against the value `M`, because the whole failure catalog is nested inside the `Mapping` trait. Calling `f.toErrorMessage` would print that `Foo.bar: String` is defined by the schema but the `ObjectMapping` for `Foo` does not define a mapping for it, with source positions for both.

The other failures are triggered the same way — by an unchecked catalog that is wrong in exactly one place:

- **`MissingTypeMapping`** — drop the `ObjectMapping` for `Foo` entirely. Unfolding `Query.foo: Foo` then reaches `Foo` with nothing in the catalog.
- **`AmbiguousTypeMappings`** — provide two equally specific mappings for `Foo`, e.g. an `ObjectMapping(schema.ref("Foo"))(...)` (a `TypeMatch`) alongside an `ObjectMapping(MappingPredicate.PathMatch(Path.from(schema.ref("Foo"))))(...)` that ties on priority.
- **`ObjectTypeExpected`** — declare `scalar Foo` in the schema but map it with an `ObjectMapping`.
- **`LeafTypeExpected`** — declare `type Foo { bar: String }` but map it with a `LeafMapping[String]`. (Enums and lists of leaves are valid `LeafMapping` targets, so those do *not* fail.)
- **`ReferencedTypeDoesNotExist`** — map `schema.uncheckedRef("Foo")` when the schema has no `Foo`. `uncheckedRef` lets you build the reference precisely so validation can flag it.
- **`UnusedFieldMapping`** — add a `CursorField("quz", ...)` to `Foo` when the schema's `Foo` only has `bar`.
- **`DeclaredFieldMappingIsHidden`** — map `Foo.bar` with `CursorField[String]("bar", _ => ???, Nil, hidden = true)`. The same `hidden = true` on a *non-declared* attribute field is fine.

The matching `unsafeValidate` path is just the throwing form of the above: given a catalog with a `ReferencedTypeDoesNotExist`-class problem, `M.unsafeValidate()` raises a `ValidationException` rather than returning a list.

## SQL-specific consistency rules

`SqlMapping` overrides `validateTypeMapping`/`validateFieldMapping` to add a layer of relational checks on top of the core catalog: object mappings need a key column, associative fields must be keys, interfaces and unions need a discriminator, an object's columns must live in a single table, and union field mappings must be hidden. These run through the same `validate`/`unsafeValidate` machinery and report their own `ValidationFailure`s. See the [SqlMapping reference](../reference/sql-mapping.md) for the full list of SQL consistency rules and the failures each one raises.

## See also

- [Mappings and cursors](../concepts/mappings-cursors.md) — what a `Mapping` and its `TypeMappings` catalog are.
- [Mapping types reference](../reference/mapping-types.md) — the `TypeMapping`/`FieldMapping` constructors named above.
- [Construct, accumulate and report errors](errors.md) — the separate `Result`/`Problem` channel that surfaces in a query's `errors` array.
- [Running operations reference](../reference/running-operations.md) — where `compiler` (and the lazy validation it triggers) fits into `compileAndRun`.
- [SqlMapping reference](../reference/sql-mapping.md) — SQL-specific consistency rules.
