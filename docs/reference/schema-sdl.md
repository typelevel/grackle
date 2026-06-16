# Schema & SDL reference

This page is an information-oriented reference for Grackle's schema model and its SDL front end: the `Schema` trait and its companion factory, the `Type`/`NamedType` hierarchy, the type operators, directive definitions, the parser entry points, and the renderer that turns a `Schema` back into SDL. It is for developers looking up exact signatures and semantics; for narrative on the model see [The schema model](../concepts/schema-model.md) and [Nullability and lists](../concepts/nullability-lists.md). All members below live in `modules/core/src/main/scala/schema.scala` unless noted, and you bring them into scope with `import grackle._` (and `import grackle.syntax._` for the interpolators).

## Entry points: `schema"..."` vs `Schema(text)`

There are two ways to build a `Schema` from SDL text. They differ in *when* validation runs and in *what* they return.

| Entry point | Import | Validated | Returns | Use when |
| --- | --- | --- | --- | --- |
| `schema"""..."""` | `grackle.syntax._` | compile time | bare `Schema` | the SDL is a literal known at compile time |
| `Schema(text)` | `grackle._` | runtime | `Result[Schema]` | the SDL is dynamic, or you want to inspect validation `Problem`s |

The `schema"..."` string interpolator is the idiomatic choice. It parses and fully validates the SDL **at compile time**, so an invalid schema is a compile error (its message is prefixed `Invalid schema:` followed by 🐞 bullets). It expands to `Schema(s, CompiletimeParsers.schemaParser).toOption.get`, which is why it hands back a bare `Schema` rather than a `Result[Schema]`. Here is the canonical definition from the StarWars demo — a `Query` root, an `enum`, an `interface`, and two object types that implement it:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#schema"))
```

Note the SDL above uses ordinary GraphQL nullability: `Character!` is non-null, `Character` is nullable, `[Character!]` is a nullable list of non-null elements. How those map onto the internal model is covered under [Type modifiers and the non-null-by-default convention](#type-modifiers-and-the-non-null-by-default-convention).

The runtime factory `Schema(text): Result[Schema]` is the companion `apply`. It accumulates validation failures into a `Result.Failure(NonEmptyChain[Problem])` instead of failing the build, so it is what you reach for when the SDL is not statically known or when you want to read the errors:

```scala mdoc:silent
import grackle.{Result, Schema}

val parsed: Result[Schema] =
  Schema("""
    type Query {
      episodeById(id: String!): Episod
    }
    type Episode {
      id: String!
    }
  """)

val names: Result[List[String]] =
  parsed.map(_.types.map(_.name))   // Failure here: "Reference to undefined type 'Episod'"
```

| Companion member | Signature |
| --- | --- |
| `Schema.apply` | `def apply(schemaText: String)(implicit pos: SourcePos): Result[Schema]` |
| `Schema.apply` (custom parser) | `def apply(schemaText: String, parser: SchemaParser)(implicit pos: SourcePos): Result[Schema]` |

> Both forms require an implicit `SourcePos`; the interpolators supply it for you.

## The `Schema` trait

A `Schema` is an immutable collection of `NamedType` declarations plus directive definitions and extensions, with the lookup helpers that mappings and elaborators use to wire themselves to types by name.

| Member | Signature | Notes |
| --- | --- | --- |
| `pos` | `def pos: SourcePos` | source position of the definition |
| `baseTypes` | `def baseTypes: List[NamedType]` | declared types *before* extensions are merged |
| `types` | `lazy val types: List[NamedType]` | `baseTypes` with any `extend` merged in |
| `directives` | `def directives: List[DirectiveDef]` | declared directives plus the five built-ins |
| `schemaExtensions` | `def schemaExtensions: List[SchemaExtension]` | `extend schema ...` blocks |
| `typeExtensions` | `def typeExtensions: List[TypeExtension]` | `extend type/interface/...` blocks |
| `definition` | `def definition(name: String): Option[NamedType]` | look up a type by name |
| `ref` | `def ref(tpnme: String): TypeRef` | checked by-name ref; throws `IllegalArgumentException` if undefined |
| `uncheckedRef` | `def uncheckedRef(tpnme: String): TypeRef` | unchecked by-name ref (may dangle) |
| `uncheckedRef` | `def uncheckedRef(tpe: NamedType): TypeRef` | unchecked ref from a known `NamedType` |
| `baseSchemaType` | `def baseSchemaType: NamedType` | the `schema { ... }` root type, before extensions |
| `schemaType` | `lazy val schemaType: NamedType` | root type with extensions merged |
| `queryType` | `def queryType: NamedType` | the `query` root; **throws** `NoSuchElementException` if absent |
| `mutationType` | `def mutationType: Option[NamedType]` | the `mutation` root, if any |
| `subscriptionType` | `def subscriptionType: Option[NamedType]` | the `subscription` root, if any |
| `isRootType` | `def isRootType(tpe: Type): Boolean` | is this one of the operation roots |
| `implementations` | `def implementations(it: InterfaceType): List[ObjectType]` | object types implementing an interface |
| `subtypes` | `def subtypes(tpe: NamedType): Set[NamedType]` | all types `<:<` the given type |

> `queryType` is non-optional and ends in `.get`; a schema with no query root throws `NoSuchElementException`. `mutationType`/`subscriptionType` are safe `Option`s. If you omit an explicit `schema { query mutation subscription }` block, Grackle synthesises a default root referencing the types named `Query`, `Mutation`, and `Subscription` — but only those that actually exist.

Obtaining a `TypeRef` with `schema.ref(name)` is the normal way to refer to a type from a mapping or elaborator. The result is a lazy, by-name reference (see [`TypeRef`](#namedtype-the-named-hierarchy) below) and is the one type form designed to be safe to compare with `==`.

## `Type`: the root of the hierarchy

`Type` is the sealed root of every GraphQL type. It carries all the navigation, nullability, list, and subtyping operators; concrete cases are the named types, plus the two unnamed modifiers `ListType` and `NullableType`.

```text
Type
├─ NamedType            (has a schema-defined `name`)
│   ├─ ScalarType
│   ├─ EnumType
│   ├─ InterfaceType    ─┐ TypeWithFields
│   ├─ ObjectType       ─┘
│   ├─ UnionType
│   ├─ InputObjectType
│   └─ TypeRef          (by-name reference; only `==`-safe form)
├─ ListType(ofType)     (unnamed modifier — a list)
└─ NullableType(ofType) (unnamed modifier — optionality)
```

### `NamedType`: the named hierarchy

`NamedType` is the subset of `Type` that has a schema-defined `name`, a `description`, and `directives`. `dealias` on a `NamedType` returns itself, except for `TypeRef`, which resolves to its target.

```scala
sealed trait NamedType extends Type {
  def name: String
  def description: Option[String]
  def directives: List[Directive]
}
```

| Case | Signature (selected fields) | Notes |
| --- | --- | --- |
| `ScalarType` | `case class ScalarType(name, description, directives)` | `isBuiltIn: Boolean`, `specifiedByURL: Option[String]`. Companion exposes `IntType`, `FloatType`, `StringType`, `BooleanType`, `IDType`, and `builtIn(name)` |
| `EnumType` | `case class EnumType(name, description, enumValues: List[EnumValueDefinition], directives)` | `hasValue(name)`, `value(name): Option[EnumValue]`, `valueDefinition(name)` |
| `InterfaceType` | `case class InterfaceType(name, description, fields: List[Field], interfaces: List[NamedType], directives)` | a `TypeWithFields`; may itself implement interfaces |
| `ObjectType` | `case class ObjectType(name, description, fields: List[Field], interfaces: List[NamedType], directives)` | a `TypeWithFields` |
| `UnionType` | `case class UnionType(name, description, members: List[NamedType], directives)` | members must be object types (validated) |
| `InputObjectType` | `case class InputObjectType(name, description, inputFields: List[InputValue], directives)` | `inputFieldInfo(name)`, `isOneOf: Boolean` |
| `TypeRef` | `case class TypeRef private[grackle] (schema: Schema, name: String)` | `dealias` resolves to the target; `exists` reports whether it is defined |

> `TypeRef`'s constructor is `private[grackle]` — you cannot `new TypeRef(...)`. Always obtain one via `schema.ref` (checked) or `schema.uncheckedRef` (unchecked). An unchecked ref to an undefined name produces a dangling `TypeRef` whose `exists` is `false` and whose navigation methods return `None`/`false`.

The field-bearing types (`ObjectType`, `InterfaceType`) share these member structures:

| Type | Signature | Mixin |
| --- | --- | --- |
| `Field` | `case class Field(name, description, args: List[InputValue], tpe: Type, directives: List[Directive])` | `Deprecatable` |
| `InputValue` | `case class InputValue(name, description, tpe: Type, defaultValue: Option[Value], directives)` | `Deprecatable` |
| `EnumValueDefinition` | `case class EnumValueDefinition(name, description, directives)` | `Deprecatable` |

`Deprecatable` supplies `isDeprecated: Boolean` and `deprecationReason: Option[String]`, sourced from an applied `@deprecated` directive.

### Type modifiers and the non-null-by-default convention

`ListType` and `NullableType` are the two unnamed `Type` cases. They wrap another `Type`:

```scala
case class ListType(ofType: Type) extends Type
case class NullableType(ofType: Type) extends Type
```

In Grackle's model **types are non-null by default**. A bare `ScalarType` or `TypeRef` is already non-null; optionality is represented by an explicit `NullableType` wrapper. This is the *opposite* of GraphQL SDL, where `String` is nullable and `String!` is non-null. The parser therefore wraps every non-`!` type in `NullableType`:

| SDL | Internal model |
| --- | --- |
| `String!` | `StringType` |
| `String` | `NullableType(StringType)` |
| `[String!]!` | `ListType(StringType)` |
| `[String!]` | `NullableType(ListType(StringType))` |
| `[String]` | `NullableType(ListType(NullableType(StringType)))` |

Write SDL with ordinary GraphQL nullability — the inversion is an internal detail you only meet when you pattern-match on the `Type` ADT. The parser bounds list-modifier nesting at `maxListTypeDepth` (default 5); see [Parser configuration](#parser-configuration). For the full rationale see [Nullability and lists](../concepts/nullability-lists.md).

## Type operators

Every `Type` carries these operators. Prefer `=:=` over `==`: `==` distinguishes a type from a `TypeRef` alias to it, which is almost never what you want. (The one designed exception is `TypeRef`s obtained from the *same* schema via `schema.ref` — those *are* `==`-comparable, and that is exactly why elaborator pattern matches use them.)

| Operator | Signature | Meaning |
| --- | --- | --- |
| `=:=` | `def =:=(other: Type): Boolean` | alias-aware equivalence (sees through `TypeRef`/aliases) |
| `<:<` | `def <:<(other: Type): Boolean` | subtype relation (object `<:<` interface, member `<:<` union, list/nullable covariance) |
| `nominal_=:=` | `def nominal_=:=(other: Type): Boolean` | compares by underlying name |
| `dealias` | `def dealias: Type` | strips `TypeRef` aliasing |
| `isNullable` | `def isNullable: Boolean` | is the outermost wrapper a `NullableType` |
| `nullable` | `def nullable: Type` | wrap in `NullableType` (idempotent) |
| `nonNull` | `def nonNull: Type` | strip an outer `NullableType` |
| `isList` | `def isList: Boolean` | is the (non-null) type a `ListType` |
| `item` | `def item: Option[Type]` | element type of a list, if any |
| `list` | `def list: Type` | wrap in `ListType` |
| `underlying` | `def underlying: Type` | strip *all* list/nullable modifiers |
| `underlyingObject` | `def underlyingObject: Option[NamedType]` | underlying object/interface/union, if any |
| `underlyingNamed` | `def underlyingNamed: NamedType` | the underlying `NamedType` |
| `underlyingField` | `def underlyingField(fieldName: String): Option[Type]` | a field's type after stripping modifiers |
| `isLeaf` | `def isLeaf: Boolean` | is the type a scalar or enum |
| `asNamed` | `def asNamed: Option[NamedType]` | this as a `NamedType`, if it is one |
| `field` | `def field(fieldName: String): Option[Type]` | the result type of a named field |
| `fieldInfo` | `def fieldInfo(fieldName: String): Option[Field]` | the full `Field` (args, directives, …) |
| `hasField` | `def hasField(fieldName: String): Boolean` | does the field exist |
| `path` | `def path(fns: List[String]): Option[Type]` | follow a chain of field names |
| `pathIsList` | `def pathIsList(fns: List[String]): Boolean` | does a path traverse a list |
| `pathIsNullable` | `def pathIsNullable(fns: List[String]): Boolean` | does a path traverse an optional |
| `/` | `def /(pathElement: String): Path` | build a `Path` for predicates |
| `directives` | `def directives: List[Directive]` | applied directives on the type |

`field`/`fieldInfo` resolve through `dealias`, so they work on a `TypeRef` for a defined type. `field` returns the field's *result type* (with modifiers); `fieldInfo` returns the whole `Field`, from which you read arguments and applied directives.

## Directives

A directive declaration in SDL — `directive @name(args) [repeatable] on LOC1 | LOC2` — parses into a `DirectiveDef`. An applied occurrence (`@name(...)` on a type, field, enum value, …) becomes a `Directive`.

| Type | Signature |
| --- | --- |
| `DirectiveDef` | `case class DirectiveDef(name, description, args: List[InputValue], isRepeatable: Boolean, locations: List[DirectiveLocation])` |
| `Directive` | `case class Directive(name: String, args: List[Binding])` |

Read applied directives off the model via `Type.directives`, `Field.directives`, etc. — for example `schema.ref("User").dealias.fieldInfo("email").get.directives`. To define your own, see [Define and use schema directives](../how-to/schema-directives.md).

The companion `object DirectiveDef` holds the **five built-in directive definitions** that are appended to every parsed schema, and `validateDirectivesForSchema` checks every applied occurrence against its definition's locations, repeatability, and argument types:

| Built-in | Purpose | First-class accessor |
| --- | --- | --- |
| `DirectiveDef.Skip` (`@skip`) | conditionally omit a field | — |
| `DirectiveDef.Include` (`@include`) | conditionally include a field | — |
| `DirectiveDef.Deprecated` (`@deprecated`) | mark a field/enum value deprecated | `isDeprecated`, `deprecationReason` (via `Deprecatable`) |
| `DirectiveDef.SpecifiedBy` (`@specifiedBy`) | record a scalar's specification URL | `ScalarType.specifiedByURL` |
| `DirectiveDef.OneOf` (`@oneOf`) | mark an input object as exactly-one-of | `InputObjectType.isOneOf` |

`DirectiveDef.builtIns` is the `List[DirectiveDef]` of all five. They are always present in `schema.directives` even though `SchemaRenderer` deliberately omits them from `toString`.

| Validation entry point | Signature |
| --- | --- |
| `Directive.fromAst` | `def fromAst(d: Ast.Directive): Result[Directive]` |
| `Directive.validateDirectivesForSchema` | `def validateDirectivesForSchema(schema: Schema): List[Problem]` |
| `Directive.validateDirectives` | `def validateDirectives(schema: Schema, location: Ast.DirectiveLocation, directives: List[Directive], vars: Vars): List[Problem]` |

### `Ast.DirectiveLocation`

The complete set of locations a directive may be declared `on`, defined in `modules/core/src/main/scala/ast.scala`:

```text
Executable locations            Type-system locations
────────────────────            ─────────────────────
QUERY                           SCHEMA
MUTATION                        SCALAR
SUBSCRIPTION                    OBJECT
FIELD                           FIELD_DEFINITION
FRAGMENT_DEFINITION             ARGUMENT_DEFINITION
FRAGMENT_SPREAD                 INTERFACE
INLINE_FRAGMENT                 UNION
VARIABLE_DEFINITION             ENUM
                                ENUM_VALUE
                                INPUT_OBJECT
                                INPUT_FIELD_DEFINITION
```

## Validation

`SchemaParser.parseDocument` runs `SchemaValidator.validateSchema` on construction, accumulating a `Problem` per structural failure. Both `schema"..."` (at compile time) and `Schema(text)` (into a `Result.Failure(NonEmptyChain[Problem])`) report these. The checks include:

| Check | Example failure |
| --- | --- |
| References to undefined types | `Reference to undefined type 'Episod'` |
| Duplicate type / field / enum-value definitions | duplicate declaration rejected |
| Non-object or duplicate union members | `Non-object type ... included in union ...` |
| Interface implementation conformance | missing field, non-`<:<` field type, or mismatched args |
| Transitive-interface obligations | `Type X does not directly implement transitively implemented interface Y` |
| Interface implements-cycles | `Interface cycle starting from ...` |
| Empty composite types | `object type X must define at least one field` |
| Type-extension target mismatches | cannot apply an object extension to a non-object |
| Directive validity | location / repeatability / argument-type checks |

> Interface conformance checks field arguments by exact match (name and type, via `==` not `<:<`). `@oneOf` input objects may not declare any non-nullable field — doing so fails at parse time (`oneOf input object type X may not have non-nullable field(s): ...`). For working with the resulting `Problem`s, see the [`Result`, `Problem` & `ResultT` reference](result-problem.md), and to validate a *mapping* against its schema see [Validate a mapping and read the failures](../how-to/validate-mappings.md).

## Custom scalars

A custom scalar is declared in SDL with `scalar Name` (optionally `@specifiedBy(url: ...)`). It produces a `ScalarType` with `isBuiltIn == false`. **The schema layer records only the scalar's name** — it does not know how to encode or decode the scalar's values. That belongs to the mapping layer, supplied via `LeafMapping[A]` plus `Value` extractors in the mapping/elaborator. Defining `scalar UUID` alone does nothing at runtime until a `LeafMapping` and the relevant `Value` pattern-matching are provided.

The five built-in scalar names — `Int`, `Float`, `String`, `Boolean`, `ID` — are always available without declaration; re-declaring one via `scalar Int` is accepted and maps to the built-in. A reference to an undeclared, non-built-in scalar (a typo such as `Long`) fails validation with `Reference to undefined type 'Long'`. See [Define custom scalars and enums](../how-to/custom-scalars-enums.md) for the end-to-end wiring.

## Parser configuration

The SDL/query parser is `GraphQLParser` (cats-parse based), in `modules/core/src/main/scala/parser.scala`. `SchemaParser` lowers parsed SDL into a validated `Schema`.

| Type | Signature |
| --- | --- |
| `GraphQLParser` | `trait GraphQLParser { def parseText(text: String): Result[Ast.Document] }` |
| `GraphQLParser` companion | `def apply(config: Config): GraphQLParser`; `val defaultConfig: Config` |
| `SchemaParser` | `trait SchemaParser { def parseText(text: String)(implicit pos: SourcePos): Result[Schema]; def parseDocument(doc: Ast.Document)(implicit sourcePos: SourcePos): Result[Schema] }` |
| `SchemaParser` companion | `def apply(parser: GraphQLParser): SchemaParser` |

`GraphQLParser.Config` bounds parse depth and width. Exceeding any limit is a parse failure; supply a custom `Config` to raise them.

| `Config` field | Default | Bounds |
| --- | --- | --- |
| `maxSelectionDepth` | `100` | nesting depth of a selection set |
| `maxSelectionWidth` | `1000` | number of selections at one level |
| `maxInputValueDepth` | `5` | nesting depth of an input value literal |
| `maxListTypeDepth` | `5` | nesting depth of `[...]` list type modifiers |
| `terseError` | `true` | shorter error rendering |

To build the stack explicitly:

```scala mdoc:silent
import grackle.{GraphQLParser, SchemaParser}

val parser       = GraphQLParser(GraphQLParser.defaultConfig)
val schemaParser = SchemaParser(parser)
```

> `grackle.Ast` is the untyped parse tree that `GraphQLParser.parseText` produces (`Document = List[Definition]`, `TypeDefinition`, `FieldDefinition`, `Ast.Type.{Named,List,NonNull}`, `Ast.Value`, `DirectiveLocation`, …). `SchemaParser.parseDocument` lowers an `Ast.Document` into the typed `Schema`. The `doc"..."` interpolator yields a raw `Ast.Document`.

## Rendering: `SchemaRenderer` and `Schema.toString`

`Schema.toString` delegates to `SchemaRenderer.renderSchema`, producing SDL. Built-in directive definitions are omitted from the output, so a round-tripped schema will not show `@skip`/`@include`/`@deprecated`/`@specifiedBy`/`@oneOf` definitions even though `schema.directives` contains them.

| Renderer member | Signature |
| --- | --- |
| `renderSchema` | `def renderSchema(schema: Schema): String` |
| `renderType` | `def renderType(tpe: Type): String` |
| `renderTypeDefn` | `def renderTypeDefn(tpe: NamedType): String` |
| `renderDirective` | `def renderDirective(d: Directive): String` |
| `renderValue` | `def renderValue(value: Value): String` |

The round trip — parse SDL to a `Schema`, then render it back — is exercised directly by the test suite. This case parses a three-type schema (including a default-valued argument `author(id: Int! = 23)`) and asserts that `schemaParser.parseText(schema).map(_.toString)` reproduces the original SDL:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/core/src/test/scala/sdl/SDLSuite.scala", "#sdl_roundtrip"))
```

The default value survives the round trip, and the rendered output matches the input byte-for-byte (`assertEquals(ser, schema.success)`).

## See also

- [Quick start: your first query](../getting-started/quick-start.md) — define a schema and run a query end to end.
- [The schema model](../concepts/schema-model.md) — the rationale behind `Type`, `NamedType`, and `TypeRef`.
- [Nullability and lists](../concepts/nullability-lists.md) — why the model inverts SDL nullability, in depth.
- [Define custom scalars and enums](../how-to/custom-scalars-enums.md) — wire `scalar`/`enum` declarations to JVM types.
- [Define and use schema directives](../how-to/schema-directives.md) — declare, apply, and read directives.
- [`Result`, `Problem` & `ResultT` reference](result-problem.md) — the error model that validation reports through.
