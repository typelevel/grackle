# Define and use schema directives

A *schema directive* is a piece of typed metadata you attach to part of your schema — a type, a field, an
enum value — in the SDL, and then read back off the schema model to drive your own behaviour (authorization,
caching hints, persisted-query tags, and so on). This how-to shows you how to declare directives, apply them,
read the applied [`Directive`](../reference/schema-sdl.md) values off the model, and use the five built-ins
(`@skip`, `@include`, `@deprecated`, `@specifiedBy`, `@oneOf`) through their first-class accessors. It assumes
you are comfortable defining a [`Schema`](../reference/schema-sdl.md) with the `schema"..."` interpolator.

## Declare a directive

Declare a directive in your SDL with the standard GraphQL syntax:

```graphql
directive @name(arg1: Type1, arg2: Type2 = default) [repeatable] on LOC1 | LOC2 | ...
```

The arguments are an ordinary input-value list (each may have a default), `repeatable` is optional, and the
`on` clause lists one or more *directive locations* the directive may be applied to. A declaration parses into
a `DirectiveDef`:

```scala
case class DirectiveDef(
    name: String,
    description: Option[String],
    args: List[InputValue],
    isRepeatable: Boolean,
    locations: List[DirectiveLocation]
)
```

Every parsed schema silently gains the five built-in definitions (`DirectiveDef.builtIns`: `skip`, `include`,
`deprecated`, `specifiedBy`, `oneOf`) appended to whatever you declare, so you never declare those yourself.
Note that `SchemaRenderer` (and therefore `Schema.toString`) deliberately omits the built-ins, so a
round-tripped schema will not print them even though `schema.directives` still contains them.

## The directive location set

Each location in the `on` clause is one of these `DirectiveLocation` values. They split into two groups —
*executable* locations (where a directive appears in a query document) and *type-system* locations (where it
appears in the schema):

| Group | Locations |
| --- | --- |
| Executable | `QUERY`, `MUTATION`, `SUBSCRIPTION`, `FIELD`, `FRAGMENT_DEFINITION`, `FRAGMENT_SPREAD`, `INLINE_FRAGMENT`, `VARIABLE_DEFINITION` |
| Type system | `SCHEMA`, `SCALAR`, `OBJECT`, `FIELD_DEFINITION`, `ARGUMENT_DEFINITION`, `INTERFACE`, `UNION`, `ENUM`, `ENUM_VALUE`, `INPUT_OBJECT`, `INPUT_FIELD_DEFINITION` |

Schema directives (the subject of this page) use type-system locations. Directives at executable locations
belong on queries — see [Query directives](query-directives.md) for those.

## Apply directives to types, fields, and enum values

Apply a declared directive with `@name(args)` at any location it permits. The following schema declares two
custom directives, `@authenticated` and `@hasRole`, and applies them to an object type, several fields, and
uses an enum as an argument value.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/directives/SchemaDirectivesSuite.scala", "#schema_directives"))
```

Both `directive` declarations list `FIELD_DEFINITION | OBJECT`, so each may sit on a whole object type or on an
individual field. `@authenticated` appears both ways here — on the `user` field and on the whole `Mutation`
type — while `@hasRole(requires: ADMIN)` sits on the `email`, `phone`, and `backup` fields. The
`requires: ADMIN` argument is an `EnumValue`; because the declaration gives `requires` a default of `ADMIN`,
you could also write a bare `@hasRole`. Because this uses the `schema"..."` interpolator, the whole thing —
including every applied directive — is validated at compile time.

## Read applied directive values off the model

An applied directive is a `Directive(name, args)` where `args` is a `List[Binding]`:

```scala
case class Directive(name: String, args: List[Binding])
```

Every part of the model exposes the directives applied to it through a `directives: List[Directive]` member:
`NamedType.directives` (so any type, reached via `schema.ref(name)` or `.dealias`), `Field.directives`,
`EnumValueDefinition.directives`, `InputValue.directives`, and `ScalarType.directives`. Type and field
directives are exactly what you read in an elaborator to drive behaviour. Here you reach the `email` field of
the `User` type and read the `@hasRole` directive applied to it:

```scala mdoc:silent
import grackle.Schema
import grackle.syntax._

val schema: Schema =
  schema"""
    type Query {
      user: User!
    }
    type User {
      name: String!
      email: String! @hasRole(requires: ADMIN)
    }
    directive @hasRole(requires: Role = ADMIN) on FIELD_DEFINITION | OBJECT
    enum Role { ADMIN  USER }
  """

val emailField = schema.ref("User").dealias.fieldInfo("email").get
```

```scala mdoc
emailField.directives
```

`fieldInfo(name)` returns the `Field`, and its `directives` is the list of applied directives — here a single
`Directive("hasRole", List(Binding("requires", EnumValue("ADMIN"))))`. Use `.dealias` to resolve the
`TypeRef` that `schema.ref` returns into the underlying `ObjectType` before calling `fieldInfo`. To read a
type-level directive instead, call `.directives` directly on the type (for example
`schema.ref("Mutation").directives`).

### Driving behaviour from directives in an elaborator

Reading directives only matters when something acts on them. A [`Phase`](../reference/elab-phases.md) in the
[query compiler](../concepts/compiler-elaboration.md) can inspect the directives on the current field and its
enclosing type and decide what to do. This phase reads the `@authenticated` and `@hasRole` directives from the
schema and, for an unauthorized field, either fails the query or nulls the field with a warning:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/directives/SchemaDirectivesSuite.scala", "#permissions_phase"))
```

The key line gathers the directives in scope from both the enclosing type and the selected field:
`c.tpe.directives ++ c.tpe.fieldInfo(name).map(_.directives).getOrElse(Nil)`. It then tests `requiresAuth`
with `dirs.exists(_.name == "authenticated")` and extracts the required role by filtering for `hasRole`,
pulling the `requires` binding, and matching its `EnumValue`. When access is denied it either calls
`Elab.failure` (which fails the whole query) or, when `nullAndWarn` is set, emits an `Elab.warning` and rewrites
the selection with a `TransformCursor` that nulls the field, so it comes back as `null` alongside a
[`Problem`](../reference/result-problem.md) in the response `errors`. The warning surfaces in the GraphQL
`errors` array because it is carried on a `Result.Warning`; an `Elab.failure` likewise surfaces as an error.
Neither is an `InternalError` — that case is raised into the effect `F` rather than returned in the JSON
`errors`.

## Built-in directives and their accessors

The five built-in directives have first-class accessors on the model, so you read them through a named method
rather than by scanning `directives` yourself:

| Directive | Applies to (locations) | Accessor | Returns |
| --- | --- | --- | --- |
| `@deprecated(reason: String = "No longer supported")` | `FIELD_DEFINITION`, `ARGUMENT_DEFINITION`, `INPUT_FIELD_DEFINITION`, `ENUM_VALUE` | `isDeprecated` / `deprecationReason` | `Boolean` / `Option[String]` |
| `@specifiedBy(url: String!)` | `SCALAR` | `specifiedByURL` | `Option[String]` |
| `@oneOf` | `INPUT_OBJECT` | `isOneOf` | `Boolean` |
| `@skip(if: Boolean!)` | `FIELD`, `FRAGMENT_SPREAD`, `INLINE_FRAGMENT` | handled by the compiler | — |
| `@include(if: Boolean!)` | `FIELD`, `FRAGMENT_SPREAD`, `INLINE_FRAGMENT` | handled by the compiler | — |

`isDeprecated`/`deprecationReason` come from the `Deprecatable` mixin and are available on every element that
can be deprecated: `Field`, `InputValue`, and `EnumValueDefinition`. `specifiedByURL` is on `ScalarType` and
returns the `url` argument of an applied `@specifiedBy`. `isOneOf` is on `InputObjectType`. `@skip` and
`@include` are *executable* directives consumed automatically by the compiler's
[`VariablesSkipAndFragmentElaborator`](../concepts/introspection-fragments-variables.md) — you do not read
them off the model.

Here you read the deprecation accessors off a deprecated field:

```scala mdoc:silent
val deprSchema: Schema =
  schema"""
    type Query {
      legacyId: ID @deprecated(reason: "Use id instead")
      id: ID!
    }
  """

val legacy = deprSchema.ref("Query").dealias.fieldInfo("legacyId").get
```

```scala mdoc
legacy.isDeprecated
legacy.deprecationReason
```

## How directives are validated

When a schema is constructed, the parser runs `SchemaValidator.validateSchema`, which calls
`Directive.validateDirectivesForSchema` over every applied directive. For each occurrence it checks the
following, accumulating a [`Problem`](../reference/result-problem.md) for each violation:

- **Definition exists.** An applied directive whose name has no `DirectiveDef` yields `Undefined directive
  '<name>'`.
- **Location is allowed.** Applying a directive at a location not listed in its `on` clause yields `Directive
  '<name>' is not allowed on <LOCATION>`.
- **Repeatability.** A non-`repeatable` directive applied more than once at the same location yields `Directive
  '<name>' may not occur more than once`.
- **Argument types.** Each argument is coerced and type-checked against the definition's `InputValue` list.
  An unrecognised argument is rejected with `Unknown argument(s) '<arg>' in directive <name>`, and a value of
  the wrong type (or a missing required one) fails. For example, omitting the required `url` on `@specifiedBy`
  reports `Value of type String required for 'url' in directive specifiedBy`.

Where these `Problem`s end up depends on how you built the schema. The compile-time `schema"..."` interpolator
turns any validation failure into a **compile error** (prefixed `Invalid schema:`), so an invalid directive
application never reaches runtime. The runtime factory `Schema(text): Result[Schema]` instead returns a
`Result.Failure(NonEmptyChain[Problem])` you can inspect — use it when the SDL is not known statically and you
need to handle validation problems yourself.

## See also

- [Define custom scalars and enums](custom-scalars-enums.md) — where `@specifiedBy` and `@oneOf` come into play.
- [Use query directives](query-directives.md) — `@skip`/`@include` and your own executable directives.
- [The schema model and SDL](../reference/schema-sdl.md) — the full reference for `Schema`, `Directive`, and `DirectiveDef`.
- [Compiler and elaboration](../concepts/compiler-elaboration.md) — how a `Phase` reads directives during query compilation.
