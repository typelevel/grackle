# Introspection, fragments and variables

This page explains what Grackle's compiler does to a GraphQL document *before* your
own mapping logic ever sees it: it answers introspection queries, evaluates `@skip`
and `@include`, expands fragments into type-narrowed selections, substitutes operation
variables, and coerces every input value against the schema. Almost all of this is
automatic and runs in fixed phases ahead of your [elaborators](compiler-elaboration.md).
It is aimed at developers debugging query compilation who want to know *why* a compiled
[`Query`](../reference/query-algebra.md) looks the way it does, and which knobs they
actually control.

These behaviours all happen in the first stage of the pipeline described in
[Architecture overview](architecture.md): query text → `QueryCompiler` (parse, type-check,
elaborate) → `Query` algebra → interpreter. Concretely, `compile` assembles its phase list
as the (optional) `IntrospectionElaborator`, then `VariablesSkipAndFragmentElaborator`,
then `MergeFields`, and only then your phases:

```text
parse → IntrospectionElaborator? → VariablesSkipAndFragmentElaborator → MergeFields → <your phases> → Query
                │                          │
                │                          ├─ variable substitution
                │                          ├─ @skip / @include
                │                          ├─ fragment expansion + Narrow(T, …)
                │                          └─ leaf/subselection validation
                └─ rewrite __schema/__type/__typename into Introspect nodes
```

Everything below is one of those built-in phases (or the shared input-coercion routine they
call), with the small surface you configure called out at the end.

## Introspection is a separate mapping over a meta-schema

GraphQL introspection — the `__schema`, `__type` and `__typename` meta-fields a client uses
to discover a server's types — is not something you implement. Grackle supplies it for any
schema automatically. You never write a mapping for the `__Type`, `__Field` or `__Schema`
meta-types.

It works in two parts. At compile time the `IntrospectionElaborator` phase rewrites any
selection named `__schema`, `__type` or `__typename` into an `Introspect(schema, child)`
node in the [query algebra](../reference/query-algebra.md). At run time an `Introspect`
node is dispatched to a dedicated interpreter, `Introspection.interpreter(targetSchema)`,
which is itself an ordinary `ValueMapping` over a fixed meta-schema (`Introspection.schema`).
That meta-mapping exposes the *target* schema's types, fields and directives as plain Scala
values, so an introspection query about your schema is answered by running a small,
self-contained Grackle mapping whose "data" is your schema's own `Type`s and `Field`s.

Because it is a distinct schema, introspection is routed by schema identity: the field
elaborator switches to its introspection logic only when the schema *is* `Introspection.schema`.
`__typename` is special-cased — the interpreter answers `Introspect(_, Select("__typename"))`
directly without spinning up the full meta-mapping — which is why `__typename` keeps working
in places `__schema` would not.

How much introspection a given query may use is the one thing you choose, via the
`introspectionLevel` argument to `compile`/`compileAndRun`. It is a
`QueryCompiler.IntrospectionLevel` with three cases:

- **`Full`** (the default) — `__schema`, `__type` and `__typename` are all allowed.
- **`TypenameOnly`** — only `__typename` is permitted; `__schema`/`__type` are rejected with
  `"Introspection is disabled"`.
- **`Disabled`** — the `IntrospectionElaborator` phase is *omitted entirely*
  (`IntrospectionElaborator(Disabled)` returns `None`).

That omission has a consequence worth internalising: under `Disabled`, `__schema`/`__type`
are never rewritten into `Introspect` nodes, so they fall through to ordinary field
resolution and fail like any other unknown field — `"No field '__type' for type Query"` —
rather than with a tidy "introspection is disabled" message. `__typename` is blocked with
`"Introspection is disabled"`. So the *same* query produces *different* errors under
`TypenameOnly` versus `Disabled`; do not assume a single uniform message when you turn
introspection off for a production endpoint.

## `@skip` and `@include` are evaluated and stripped early

`@skip` and `@include` are built-in directives (`DirectiveDef.Skip`, `DirectiveDef.Include`)
allowed on fields, fragment spreads and inline fragments. They are resolved by the
`VariablesSkipAndFragmentElaborator` phase, which runs before any of your phases. The rule
is: a node is dropped when `(skip && if) || (include && !if)`. The `if` argument may be a
literal `BooleanValue` or a Boolean variable; a missing argument, a non-Boolean value, or a
duplicate `skip`/`include` on the same node fails compilation.

The following test compiles a query that exercises every combination, with the two Boolean
variables `$yup = true` and `$nope = false`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/SkipIncludeSuite.scala", "#skip_include"))
```

Read the `expected` value at the bottom: of the four aliased selections only `b` and `c`
survive. `a: field @skip(if: $yup)` and `d: field @include(if: $nope)` are gone — not
emitted as nulls, but absent from the compiled `Group` altogether. A skipped subtree becomes
`Query.Empty` and disappears from the algebra; the data mapping is never asked about it.

The important structural point is *when* this happens. `@skip`/`@include` are filtered out of
the directive list inside this phase, before any remaining directives are handed to your
[custom-directive](../how-to/query-directives.md) logic. So `skip`/`include` are invisible to
phases that run later, and a skipped node never reaches them at all. If you write a phase that
inspects directives, prepend it so it runs before this elaborator.

## Fragments expand into `Group` and `Narrow`

Named fragment spreads (`...userFragment`) and inline fragments (`... on User { … }`) are
also expanded by `VariablesSkipAndFragmentElaborator`, at compile time. The expansion depends
on the relationship between the context type and the fragment's type condition. If the context
type is already a subtype of the fragment's type condition, the fragment's children are inlined
directly. Otherwise they are wrapped in a `Narrow(T, child)` node that restricts that part of
the selection to instances of type `T`. The "does this fragment apply" test follows the GraphQL
spec's *fragment spread is possible* rule, computed from the schema's subtype relationships.

The next snippet selects from `profiles`, whose type is an interface, and spreads two fragments
plus `__typename`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/compiler/FragmentSuite.scala", "#fragment_typed"))
```

The `expected` algebra shows exactly how the document is rewritten:

- `id` stays a plain `Select` — it is a field of the interface, valid for every member.
- `__typename` becomes an `Introspect(schema, Select("__typename"))` node, per the previous
  section.
- `...userFragment` and `...pageFragment` — whose definitions carry the type conditions
  `on User` and `on Page` — each become a `Narrow(User, …)` / `Narrow(Page, …)`, because `User`
  and `Page` are *subtypes* of the `profiles` interface, not the interface itself. At run time a
  `Narrow(T, …)` selects its children only for objects whose runtime type is `T`.

A fragment whose type condition is the context type (or a supertype of it) would inline
*without* a `Narrow`. The flip side is that a fragment on a supertype may only select fields
that exist on that supertype: selecting a subtype-only field through a supertype fragment fails
at compile time (for example `"No field 'name' for type Profile"`), and you must narrow
explicitly with `... on User { name }` inside it.

Fragment validation is strict and happens up front, before elaboration produces the algebra
above. Undefined fragments, duplicate fragment names, fragment cycles
(`"Fragment cycle starting from 'x'"`), unused fragments, and type-incompatible spreads are all
rejected. The unused check is paired with the unused-variable check and can be turned off with
`reportUnused = false` on `compile`.

## Variables: definition, typing and JSON coercion

Operation variables (`query doSearch($pattern: Pattern) { … }`) are declared in the document
and supplied separately as JSON through the `untypedVars` argument. Compilation treats them in
three steps:

1. The `$name: T` definitions are turned into `InputValue` definitions resolved against the
   target schema — this is where an unknown variable type is caught.
2. The supplied JSON values are coerced against those definitions by `Value.checkVarValue`
   (see the next section), producing query-algebra `Value`s.
3. Variable *references* inside arguments, list literals and input objects are only resolved
   later, during the `VariablesSkipAndFragmentElaborator` phase, by substituting each
   `VariableRef` with its bound value. An unbound reference fails with
   `"Variable 'x' is undefined"`.

Variables get the same dual definedness check as fragments: a reference to an undeclared
variable fails (`"Variable 'x' is undefined"`), and a *declared but never used* variable also
fails (`"Variable 'x' is unused"`) unless you pass `reportUnused = false`. This is why a
`@skip(if: $cond)` whose `$cond` you forgot to declare is a compile error, and a leftover
declared variable is too.

## Input coercion: defaults, absent vs null, ID widening and `@oneOf`

Every input value — whether a literal argument written in the query or a JSON variable value —
is coerced against its `InputValue` definition before it reaches your mapping. There are two
parallel routines: `Value.checkValue` for literal query values (`Value` → `Value`) and
`Value.checkVarValue` for incoming JSON (`Json` → `Value`). They apply the same rules:

- **Defaulting.** An omitted argument or input-object field that has a `defaultValue` is filled
  with that default.
- **Absent vs null.** This distinction is load-bearing. An *omitted* nullable argument or field
  coerces to `AbsentValue`; an *explicit* `null` coerces to `NullValue`. They are different
  cases of the `Value` ADT, and your elaborators and cursors must handle both. For example, a
  top-level `field(arg: null)` yields `NullValue` while `field` with no argument yields
  `AbsentValue`.
- **ID widening.** A `StringValue` or `IntValue` supplied for an `ID`-typed input is widened to
  `IDValue` (the integer via its string form). A literal `id: 123` and a JSON variable `123`
  both arrive as `IDValue("123")`, so never rely on receiving an `IntValue` for an `ID` argument.
- **Custom-scalar widening.** For a non-built-in `ScalarType`, any `Int`/`Float`/`String`/`Boolean`
  literal is accepted without validation. Grackle does not check the *contents* of a custom
  scalar here — that is your mapping's job (see
  [Define custom scalars and enums](../how-to/custom-scalars-enums.md)).
- **Enum validation.** Enum values are checked against the schema's declared values.
- **Recursion and unknown fields.** Lists and input objects are coerced element by element and
  field by field; an unknown input-object field is rejected. The error wording differs by path
  — literal arguments say `"… for input object value of type X in field 'f' of type 'Query'"`,
  while variable values say `"… in input object value of type X in variable values"`.

A worked illustration: given `input Pattern { name: String  age: Int  id: ID  userType: …  date: … }`,
the variable JSON `{ "name": "Foo", "age": 23, "id": 123 }` coerces to an `ObjectValue` whose
`name` is a `StringValue`, `age` an `IntValue`, `id` an `IDValue("123")` (Int widened to ID),
and whose unsupplied `userType` and `date` fields are present as `AbsentValue` — not dropped,
not null.

### `@oneOf` input objects

An input object declared `input X @oneOf` must have *exactly one* member present and non-null.
Coercion enforces this: zero present members, more than one present, or a present-but-`null`
member each fail with a specific message such as
`"Exactly one key must be specified for oneOf input object X …"`. The same rule applies to both
literal arguments and variable values. The `@oneOf` constraint is also surfaced through
introspection on `__Type.isOneOf`, and the `oneOf` directive appears in `__schema.directives`,
because `oneOf` is one of the built-in directive definitions (alongside `skip`, `include`,
`deprecated` and `specifiedBy`) merged into every schema.

## What is automatic and what you configure

Almost all of this subsystem is fixed behaviour you cannot turn off:

- introspection elaboration and execution, including `__typename` and the deprecation filtering
  of introspection results;
- `@skip`/`@include` evaluation;
- fragment expansion, type narrowing and fragment validation;
- variable substitution and the validation of variable definitions;
- input-value coercion, including defaults, `AbsentValue`/`NullValue`, ID and custom-scalar
  widening, enum checks and `@oneOf`;
- merging of duplicate fields.

What you actually configure is a short list:

- **`introspectionLevel`** per query (`Full` / `TypenameOnly` / `Disabled`).
- **`reportUnused`** per query, to suppress the unused-variable / unused-fragment errors.
- **Custom query directives** — declared in your SDL (so they pass location and argument
  validation) and interpreted by a `QueryCompiler.Phase` you prepend to your phases. Declaring
  the directive only validates *placement*; behaviour comes from the phase. See
  [Write a custom query directive](../how-to/query-directives.md).
- **`SelectElaborator.select`** logic, which receives already-coerced arguments and the
  surviving (non-`skip`/`include`) directives for each field — the place your own per-field
  argument handling lives.

## See also

- [The compiler and elaboration](compiler-elaboration.md) — the phase pipeline these
  behaviours plug into.
- [Write a custom query directive](../how-to/query-directives.md) — the how-to recipe for the
  one extension point in this subsystem.
- [Query algebra reference](../reference/query-algebra.md) — the `Select`/`Group`/`Narrow`/`Introspect`/`Empty`
  nodes this page refers to.
- [Elab monad & compiler phases reference](../reference/elab-phases.md) — the `Phase` and `Elab`
  API for writing your own phases.
- [Nullability and lists](nullability-lists.md) — how the `NullableType` wrapper relates to the
  `AbsentValue`/`NullValue` distinction.
