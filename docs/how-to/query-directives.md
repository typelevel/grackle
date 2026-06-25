# Write a custom query directive

This how-to shows you how to add a custom GraphQL query directive — a marker like `@upperCase` that a client attaches to a field to change how that field is resolved — and make Grackle act on it. A query directive in Grackle is two things: a declaration in your SDL (so the directive is recognised and placement-checked) and a [compiler `Phase`](../concepts/compiler-elaboration.md) that rewrites the query algebra wherever the directive appears. The worked example is `@upperCase`, which upper-cases the result of any `String` field it is applied to. This page is for advanced users customising query behaviour; for the built-in `@skip`/`@include` directives, which need none of this, see the note at the end.

## Step 1: declare the directive in your SDL

A directive must be declared in your schema before a query may use it. The declaration registers the directive in `schema.directives` and tells Grackle where it is allowed to appear; without it, a query carrying `@upperCase` fails compilation with `Undefined directive 'upperCase'`, and using it in the wrong place fails with `Directive 'upperCase' is not allowed on <LOCATION>`. These checks run in `Directive.validateDirectivesForQuery` before any phase executes.

Declare the directive with a directive definition and the locations it may appear at:

```graphql
type Query {
  user: User!
}
type User {
  name: String!
  handle: String!
  age: Int!
}

directive @upperCase on FIELD
```

`on FIELD` means `@upperCase` may be attached only to a field selection. The location names come from the GraphQL spec (`FIELD`, `FRAGMENT_SPREAD`, `INLINE_FRAGMENT`, and so on); list several with `|` if a directive is valid in more than one place. Declaring the directive only validates *placement* — it gives the directive no behaviour. The behaviour comes from the phase you write next.

## Step 2: write a `Phase` that matches the directive

Behaviour lives in a [`QueryCompiler.Phase`](../reference/elab-phases.md). A phase's `transform` walks every node of the query algebra; you override it, match the nodes that carry your directive, and rewrite them. Custom directives are still attached to `UntypedSelect` nodes at this stage — the pre-elaboration form of a field selection that carries its raw arguments and directives — so you pattern-match on `UntypedSelect` and inspect its `directives` list.

Here is the `@upperCase` phase:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/directives/QueryDirectivesSuite.scala", "#upper_phase"))
```

Walking through it:

- The match guard `directives.exists(_.name == "upperCase")` selects only the fields the client tagged. Every other node falls through to the final `case _ => super.transform(query)`, which applies the default recursive traversal unchanged.
- `Elab.context` reads the current [`Context`](../reference/context-env.md) — the elaborator's position in the schema — from the [`Elab` monad](../reference/elab-phases.md) that threads state through the traversal. `c.forField(nme, alias)` then computes the `Context` for the field being selected, lifted into `Elab` with `Elab.liftR`, so you can inspect that field's type.
- `fc.tpe =:= ScalarType.StringType` checks the field is actually a `String`. When it is, `super.transform(query)` elaborates the child as normal and the result is wrapped in `TransformCursor(toUpperCase, _)`. `TransformCursor` is a query-algebra node that hands the interpreter a function `Cursor => Result[Cursor]`, applied to the cursor before the child is interpreted — this is the general hook for post-processing a field's resolved value.
- `toUpperCase` builds that function with `FieldTransformCursor[String](c, _.toUpperCase.success)`. `FieldTransformCursor[T]` wraps a cursor so that, when its leaf value of type `T` is read, the supplied `T => Result[T]` is applied — here, upper-casing the `String`. The result type carries the leaf type, so the transform is type-checked against the field.

The directive name (`"upperCase"`) is matched as a plain string; nothing ties the phase to the SDL declaration except that they agree on the name. Step 1 guarantees the directive is well-placed; Step 2 decides what it does.

## Step 3: prepend the phase in `compilerPhases`

A `Mapping`'s `compilerPhases` lists the phases run during compilation, in order. The default is `selectElaborator`, `componentElaborator`, `effectElaborator`. Your directive phase must run **before** them: `selectElaborator` rewrites `UntypedSelect` into the typed `Select` node, after which the `UntypedSelect` you match on no longer exists. Override `compilerPhases` to put your phase first.

Here is the full mapping, with the phase from Step 2 and the `compilerPhases` override at the bottom:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/directives/QueryDirectivesSuite.scala", "#upper_mapping"))
```

The final line — `List(upperCaseElaborator, selectElaborator, componentElaborator, effectElaborator)` — places `upperCaseElaborator` ahead of the standard three. (The `// #upper_phase` comments are documentation markers in the source, not part of the code.)

There is one more ordering fact to keep in mind. Grackle always auto-prepends its own phases ahead of everything in `compilerPhases`: introspection elaboration, then `VariablesSkipAndFragmentElaborator`, then `MergeFields`. So by the time your phase runs, variables have been substituted into directive arguments and fragments have been expanded. `VariablesSkipAndFragmentElaborator` removes `@skip`/`@include` from each node's directive list (after acting on them) but leaves your custom directives in place, still attached to the `UntypedSelect`. They stay there until `selectElaborator` rewrites the `UntypedSelect` into a typed `Select` — so prepending your phase to `compilerPhases` (rather than appending) is what puts it in the window where the `UntypedSelect`, and your directive on it, still exist.

## Result: applying the directive

With the mapping wired up, the directive takes effect at query time. This query upper-cases `name`, leaves `handle` untouched, and — because `age` is an `Int`, not a `String` — triggers the warning branch:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/directives/QueryDirectivesSuite.scala", "#upper_query"))
```

The response carries **both** data and an error. `name` is `"MARY"`, `handle` stays `"mary"`, and `age` is the unmodified `42`, while the `errors` array reports `'upperCase' directive may only be applied to fields of type String`. This is the shape of a `Warning` result: the query still produces data, and the problem is surfaced alongside it.

## Failing versus warning

The phase chose to *warn* on misuse, not abort. That choice is the difference between two `Elab` combinators in the `else` branch:

- `Elab.warning(msg) *> super.transform(query)` records a `Problem` and continues, so the field still resolves (here, unchanged because the transform was skipped). The compiled query succeeds with a `Warning`, and the `Problem` appears in the response `errors` array next to the `data`, as above.
- `Elab.failure(msg)` would instead make compilation produce a `Failure`. The query would not run; the response would carry `errors` and no `data` for that operation.

Pick `warning` when a misapplied directive should degrade gracefully, and `failure` when it should reject the whole query. Both surface through the GraphQL `errors` array. (Note that this is distinct from an *internal* error raised into the effect `F` — see [error handling](errors.md) for that distinction.)

## Note: `@skip` and `@include` are handled for you

You do not write a phase for the two built-in directives. `@skip(if:)` and `@include(if:)` are evaluated automatically by `VariablesSkipAndFragmentElaborator`: a guarded subtree that is skipped becomes `Query.Empty` and disappears from the result, and `@skip`/`@include` themselves are removed from each node's directive list once they have been acted on. So a custom phase that runs after `VariablesSkipAndFragmentElaborator` (the normal case, where your phase still precedes `selectElaborator`) never sees `@skip`/`@include` — and never needs to, since they have already taken effect. Reserve custom phases for directives you have declared yourself.

## See also

- [Compiler and elaboration](../concepts/compiler-elaboration.md) — how a query string becomes the `Query` algebra, and where phases fit.
- [Introspection, fragments and variables](../concepts/introspection-fragments-variables.md) — the built-in phases (`@skip`/`@include`, fragments, variables) that run before yours.
- [Elaboration phases reference](../reference/elab-phases.md) — the `Phase` trait and the `Elab` monad combinators (`context`, `warning`, `failure`, `transformChild`).
- [Query algebra reference](../reference/query-algebra.md) — the `Query` nodes, including `UntypedSelect` and `TransformCursor`.
- [Schema directives](schema-directives.md) — declaring directives that annotate the *schema* rather than a query.
