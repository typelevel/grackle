# Mappings and cursors

This page explains how a Grackle `Mapping` ties a GraphQL schema to your data, and how that wiring is realised at run time by `Cursor`s. It is for developers building or debugging mappings who want to understand *why* the pieces are shaped the way they are: the two-layer catalog (a `TypeMapping` selects a GraphQL type, a `FieldMapping` wires one of its fields to data), how field resolution dispatches per backend, and the `focus`/`tpe` duality that every cursor walks. It assumes you are comfortable with cats-effect and have seen a mapping before; for exact signatures see the [mapping types](../reference/mapping-types.md) and [cursor](../reference/cursor.md) references.

## The two-layer model: TypeMapping and FieldMapping

A `Mapping[F]` holds three things: the `schema`, a `TypeMappings` catalog, and a `selectElaborator`. The catalog is where the schema-to-data correspondence lives, and it is organised in two layers.

The outer layer is the **`TypeMapping`**. Each one maps a whole GraphQL type, and there are exactly two concrete kinds:

- **`ObjectMapping`** maps an object, interface, or union type. It carries a sequence of `FieldMapping`s — one per field you expose.
- **`LeafMapping[T]`** maps a scalar or enum type to a circe `Encoder[T]`. It is the *leaf* construct: there is no `PrimitiveMapping` in Grackle. You only declare `LeafMapping`s for your own scalars and enums; built-in leaf mappings for `String`, `Int`, `Float`, `Boolean`, and `ID` are appended to the catalog automatically, so you never write those yourself.

The inner layer is the **`FieldMapping`**, nested inside an `ObjectMapping`, that wires a single field to data. The catalog of field mappings includes `ValueField` (apply a function to the parent value), `CursorField` (compute a leaf from the parent `Cursor`), `Delegate` (hand the field off to another `Mapping` for cross-backend composition), and the effectful `EffectField`, `RootEffect`, and `RootStream`. The circe backend adds `CirceField` (a constant `Json`) and `CursorFieldJson` (a computed `Json` subtree) on top of these.

Here is a real `ObjectMapping` catalog, from the SQL demo's `WorldMapping`. The mapping is SQL-backed, so the field mappings are `SqlField`/`SqlObject`, but the *shape* is the universal one — an `ObjectMapping` for each GraphQL object type, each holding a list of field mappings:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#type_mappings"))
```

Note `SqlField("countrycode", city.countrycode, hidden = true)`: a *hidden* field mapping has no corresponding declared schema field — it exists only so other field mappings (here, the join that resolves `country`) can depend on it. Every *declared* schema field, by contrast, must have a non-hidden field mapping, or validation reports a `MissingFieldMapping`.

Leaf mappings sit alongside object mappings in the same catalog. A typical custom-scalar block looks like this:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/ScalarsSuite.scala", "#scalars_leafmappings"))
```

Each `LeafMapping[T](tpe)` needs an implicit circe `Encoder[T]` in scope; that encoder is what turns a `java.util.UUID` or your `Genre` enum into JSON at a leaf position.

A `List` of these mappings compiles where a `TypeMappings` is expected because of an implicit conversion (`TypeMappings.fromList`). That conversion always produces a *checked* catalog. A checked catalog is validated against an unfolding of the schema — but lazily, the first time the `compiler` is forced, not at construction. So a mapping with a missing or ambiguous type mapping can look fine until the first query is compiled, at which point validation throws. To skip validation you must opt in explicitly with `TypeMappings.unchecked(...)`.

## MappingPredicate: matching a type to a context

A `TypeMapping` does not name its type directly; it carries a `MappingPredicate` that decides *when* the mapping applies and at what priority. When the interpreter needs the mapping for a type at some position, the catalog asks every candidate predicate for an `Option[Int]` priority and the highest wins. A tie between two equally-specific mappings is an `AmbiguousTypeMappings` error, not a silent pick.

There are three predicates:

- **`TypeMatch(tpe)`** matches the type *anywhere* it appears, at priority `0`. This is the default you get from the convenience constructors `ObjectMapping(tpe)(...)` and `LeafMapping[T](tpe)`.
- **`PathMatch(path)`** matches the type only when it is reached via a specific field path. Because its priority grows with path length, a path-specific mapping outranks a bare `TypeMatch` for the same type — which is exactly how you give one GraphQL type two different mappings depending on where it occurs in a query. Prefer `PathMatch` for context-sensitive mappings.
- **`PrefixedTypeMatch(prefix, tpe)`** is the legacy `PrefixedMapping` semantics, kept for backwards compatibility; new code should reach for `PathMatch` instead.

This priority-ordered, context-aware lookup is why a single schema type can be backed by different data in different parts of a query without ambiguity.

## From field selection to a child cursor

Once the catalog is built, resolving a selected field is a two-step dispatch.

`mkCursorForField` (the entry point, and `final`) does the **catalog lookup**: given the parent cursor and a field name, it finds the applicable `ObjectMapping` for the parent's context and pulls out the matching `FieldMapping`. It then hands that field mapping to `mkCursorForMappedField`.

`mkCursorForMappedField` does the **per-backend dispatch**. It is the protected override point each backend specialises. The base `Mapping` implementation handles the backend-agnostic cases — `EffectMapping` and `CursorField`. Each concrete mapping then extends it for its own field-mapping types:

- `ValueMappingLike` handles `ValueField` by applying its function `f: T => Any` to the parent focus, producing the child value.
- `CirceMappingLike` handles `CirceField` and `CursorFieldJson`, producing cursors over `Json`.
- `ComposedMapping` returns a `ComposedCursor` for `Delegate` fields, which defers the sub-selection to another mapping.

So the flow for every field is the same: lookup resolves *which* field mapping applies; dispatch turns that field mapping into a child `Cursor` of the right concrete type. The cursor is the thing the interpreter actually walks.

## The focus/tpe duality

A `Cursor` is Grackle's read-only navigator over your backing data during interpretation. Each cursor pairs two things:

- a **`focus`**: the current runtime value, typed `Any` — it could be a Scala case class, an `io.circe.Json` node, a SQL result row, anything; and
- a **`tpe`**: the GraphQL `Type` the focus is *expected* to represent, derived from the cursor's `Context`.

Every navigation method matches on `(tpe, focus)` *together*. This duality is the heart of how interpretation stays type-directed while the underlying data stays untyped. The cleanest place to see it is `ValueCursor`, the reference implementation over plain Scala values:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/main/scala/valuemapping.scala", "#value_cursor"))
```

Read `isList`, `asList`, and `asNullable`: each one matches on the pair. `isList` is true only when `tpe` is a `ListType` **and** `focus` is a `List[_]` — a `ListType` paired with a non-list focus reports `false`, and `asList` would return a `Result.internalError`. `asNullable` similarly insists `tpe` is a `NullableType` and `focus` an `Option[_]`, returning `Result[Option[Cursor]]`: `Some(child)` when the option is defined, `None` when empty (the interpreter renders `None` as `Json.Null`). `narrowsTo`/`narrow` handle interface and union narrowing — and `ValueCursor` decides membership at run time with the `ValueObjectMapping`'s `ClassTag` (`classTag.runtimeClass.isInstance(focus)`), which is why you must supply the correct concrete type parameter, e.g. `ValueObjectMapping[Human](...)`. Finally, `field` delegates straight back to `mkCursorForField`, re-entering the lookup-and-dispatch cycle one level deeper.

Two things to notice about the design. First, every navigation method returns `Result` and has a permissive error default (inherited from `AbstractCursor`): a cursor only overrides the cases its model actually supports, and calling the wrong method for a focus yields an error `Result`, never a thrown exception. Second, `preunique` does *not* return a unique element — it re-types the focus as `tpe.nonNull.list` so the `Unique` query operator can subsequently run a list traversal and pull out the single match. The `focus`/`tpe` agreement must always hold: a cursor that reports a `tpe` the query does not expect surfaces as a "Mismatched query and cursor type" internal error rather than silently mis-walking.

That dispatch — picking `asLeaf` vs `runFields` vs `runList` vs `asNullable` from `(query, tpe.dealias)` — happens in the interpreter's `runValue`, covered in [how the query interpreter works](query-interpreter.md). What matters here is that the cursor exposes exactly the type-driven operations the interpreter needs, and matches each one against `(tpe, focus)`.

## Concrete cursors per backend

Because the `Cursor` trait is only this navigation contract, each backend ships its own implementation over its own focus type:

- **`ValueCursor`** (above) walks plain Scala values; `focus` is `Any` and narrowing uses a `ClassTag`.
- **`CirceCursor`** walks an `io.circe.Json` document; `focus` is `Json`, `asLeaf` validates the JSON shape against the GraphQL scalar/enum type, and `narrowsTo` checks that the required fields are present.
- **Generic-derivation cursors** are produced by `CursorBuilder[T]` instances derived from your Scala types; they are the most direct way to obtain a cursor by hand.
- **SQL cursors** walk the rows of a compiled SQL result.

They all extend either `AbstractCursor` (which supplies the error defaults so a backend overrides only what it supports) or `ProxyCursor` (which delegates to an underlying cursor and overrides a few methods — `NullCursor`, `ListTransformCursor`, and friends are built this way). The interpreter never cares which concrete cursor it holds; it only calls the navigation methods, and each cursor answers them against its own focus.

## See also

- [How the query interpreter works](query-interpreter.md) — how `runValue`/`runFields`/`runList` drive a cursor to build the result JSON.
- [The compiler and elaboration](compiler-elaboration.md) — how `selectElaborator` rewrites a query into the algebra the interpreter walks.
- [How cross-mapping delegation executes](composition.md) — what `Delegate`, `ComposedCursor`, and `combineAndRun` do across backends.
- [Mapping types reference](../reference/mapping-types.md) — exact signatures for every `TypeMapping` and `FieldMapping`.
- [Cursor reference](../reference/cursor.md) and [Context & Env reference](../reference/context-env.md) — the full `Cursor`, `Context`, and `Env` APIs.
