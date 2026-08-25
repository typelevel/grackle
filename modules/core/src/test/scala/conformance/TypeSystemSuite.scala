// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package conformance

import io.circe.literal._

import grackle.syntax._

/**
 * Conformance test cases for section 3, Type System.
 *
 * Several examples in this section are a fragment of a schema, or a selection set which needs a
 * schema. Each such test case adds the definitions which complete the example. A comment marks
 * every addition.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Type-System
 */
final class TypeSystemSuite extends ConformanceSuite {

  // 3.2 Type System Descriptions
  // https://spec.graphql.org/September2025/#sec-Descriptions

  validSchema("every definition of a schema can carry a description".fail)("""
    '''
    A simple GraphQL schema which is well described.
    '''
    schema {
      query: Query
    }

    '''
    Root type for all your query operations
    '''
    type Query {
      '''
      Translates a string from a given language into a different language.
      '''
      translate(
        "The original language that `text` is provided in."
        fromLanguage: Language

        "The translated language to be returned."
        toLanguage: Language

        "The text to be translated."
        text: String
      ): String
    }

    '''
    The set of languages supported by `translate`.
    '''
    enum Language {
      "English"
      EN

      "French"
      FR

      "Chinese"
      CH
    }
  """)

  // 3.3.1 Root Operation Types
  // https://spec.graphql.org/September2025/#sec-Root-Operation-Types

  validQuery("a query operation selects a field of the query root type", MyNameSchema)("""
    query {
      myName
    }
  """)

  validSchema("the query root operation type provides the field which that query selects")("""
    type Query {
      myName: String
    }
  """)

  validQuery("a mutation operation selects a field of the mutation root type", SetNameSchema)(
    """
    mutation {
      setName(name: "Zuck") {
        newName
      }
    }
  """)

  validSchema("a schema definition can name a query and a mutation root operation type")("""
    schema {
      query: MyQueryRootType
      mutation: MyMutationRootType
    }

    type MyQueryRootType {
      someField: String
    }

    type MyMutationRootType {
      setSomeField(to: String): String
    }
  """)

  validSchema("a schema definition can be omitted when the root types use the default names")(
    """
    type Query {
      someField: String
    }
  """)

  validSchema("a type named Mutation is not a root type when a schema definition says so")("""
    schema {
      query: Query
    }

    type Query {
      latestVirus: Virus
    }

    type Virus {
      name: String
      mutations: [Mutation]
    }

    type Mutation {
      name: String
    }
  """)

  validSchema("a schema definition can carry a description".fail)("""
    '''
    Example schema
    '''
    schema {
      query: Query
      mutation: Mutation
    }

    type Query {
      someField: String
    }

    type Mutation {
      someMutation: String
    }
  """)

  // 3.5 Scalars
  // https://spec.graphql.org/September2025/#sec-Scalars

  validSchema("a custom scalar can point at the specification which defines it")("""
    scalar UUID @specifiedBy(url: "https://tools.ietf.org/html/rfc4122")
    scalar URL @specifiedBy(url: "https://tools.ietf.org/html/rfc3986")
    scalar DateTime
      @specifiedBy(url: "https://scalars.graphql.org/andimarek/date-time")

    # Added to complete the example: a query root type.
    type Query { id: UUID url: URL at: DateTime }
  """)

  // 3.6 Objects
  // https://spec.graphql.org/September2025/#sec-Objects

  validSchema("an object type defines a set of fields")("""
    type Person {
      name: String
      age: Int
      picture: Url
    }

    # Added to complete the example: the `Url` scalar and a query root type.
    scalar Url
    type Query { person: Person }
  """)

  validQuery("a selection set requests the fields of an object type", PersonSchema)("""
    {
      name
      age
      picture
    }
  """)

  validQuery("the order of the requested fields is free", PersonSchema)("""
    {
      age
      name
    }
  """)

  validSchema("a field of an object type can have that same object type")("""
    type Person {
      name: String
      age: Int
      picture: Url
      relationship: Person
    }

    # Added to complete the example: the `Url` scalar and a query root type.
    scalar Url
    type Query { person: Person }
  """)

  invalidQuery("a field of an object type needs a selection set", RelationshipSchema)("""
    {
      name
      relationship
    }
  """)

  validQuery("a field of an object type with a selection set", RelationshipSchema)("""
    {
      name
      relationship {
        name
      }
    }
  """)

  // 3.6 Objects, Field Ordering
  // https://spec.graphql.org/September2025/#sec-Objects.Field-Ordering

  // The specification states an ordered result for each example of this subject. It numbers the
  // values of that result by position, so two examples state a different value for one field.
  // Each test case below therefore compares the response keys, in order, against the keys of the
  // stated result.

  // The stated result is {"foo": 1, "bar": 2, "baz": 3, "qux": 4}.
  yieldsFieldOrder(
    "a fragment spread before other fields keeps its position",
    TypeSystemMappings.Ordering)("""
    {
      foo
      ...Frag
      qux
    }

    fragment Frag on Query {
      bar
      baz
    }
  """)(List("foo", "bar", "baz", "qux"))

  yieldsFieldOrder(
    "a repeated field keeps the position of its first use",
    TypeSystemMappings.Ordering)("""
    {
      foo
      ...Matching
      bar
    }

    fragment Matching on Query {
      bar
      qux
      foo
    }
  """)(List("foo", "bar", "qux"))

  // The stated result is {"bar": 1, "foo": 2}.
  yieldsFieldOrder(
    "a field which a directive excludes does not affect the field order",
    TypeSystemMappings.Ordering)("""
    {
      foo @skip(if: true)
      bar
      foo
    }
  """)(List("bar", "foo"))

  // 3.6.1 Field Arguments
  // https://spec.graphql.org/September2025/#sec-Field-Arguments

  validSchema("a field can declare arguments")("""
    type Person {
      name: String
      picture(size: Int): Url
    }

    # Added to complete the example: the `Url` scalar and a query root type.
    scalar Url
    type Query { person: Person }
  """)

  validQuery("a selection set supplies the argument of a field", PictureSchema)("""
    {
      name
      picture(size: 600)
    }
  """)

  // 3.6.2 Field Deprecation
  // https://spec.graphql.org/September2025/#sec-Field-Deprecation

  validSchema("a field can be deprecated")("""
    type ExampleType {
      oldField: String @deprecated
    }

    # Added to complete the example: a query root type.
    type Query { example: ExampleType }
  """)

  // 3.6.3 Object Extensions
  // https://spec.graphql.org/September2025/#sec-Object-Extensions

  validSchema("an object extension can add a field")("""
    extend type Story {
      isHiddenLocally: Boolean
    }

    # Added to complete the example: the `Story` type and a query root type.
    type Story { id: ID }
    type Query { story: Story }
  """)

  validSchema("an object extension can add a directive only")("""
    extend type User @addedDirective

    # Added to complete the example: the directive, the `User` type and a query root type.
    directive @addedDirective on OBJECT
    type User { id: ID }
    type Query { user: User }
  """)

  // 3.7 Interfaces
  // https://spec.graphql.org/September2025/#sec-Interfaces

  validSchema("an object type can implement more than one interface")("""
    interface NamedEntity {
      name: String
    }

    interface ValuedEntity {
      value: Int
    }

    type Person implements NamedEntity {
      name: String
      age: Int
    }

    type Business implements NamedEntity & ValuedEntity {
      name: String
      value: Int
      employeeCount: Int
    }

    # Added to complete the example: a query root type.
    type Query { person: Person business: Business }
  """)

  validSchema("a field can have an interface type")("""
    type Contact {
      entity: NamedEntity
      phoneNumber: String
      address: String
    }

    # Added to complete the example: the interface and a query root type.
    interface NamedEntity { name: String }
    type Person implements NamedEntity { name: String age: Int }
    type Query { contact: Contact }
  """)

  validQuery("a selection set can request the fields of an interface", ContactSchema)("""
    {
      entity {
        name
      }
      phoneNumber
    }
  """)

  invalidQuery("a selection set cannot request a field of one implementation", ContactSchema)(
    """
    {
      entity {
        name
        age
      }
      phoneNumber
    }
  """)

  validQuery("an inline fragment reaches the fields of one implementation", ContactSchema)("""
    {
      entity {
        name
        ... on Person {
          age
        }
      }
      phoneNumber
    }
  """)

  validSchema("an interface can implement another interface")("""
    interface Node {
      id: ID!
    }

    interface Resource implements Node {
      id: ID!
      url: String
    }

    # Added to complete the example: a query root type.
    type Query { resource: Resource }
  """)

  validSchema("an interface must declare every transitively implemented interface")("""
    interface Node {
      id: ID!
    }

    interface Resource implements Node {
      id: ID!
      url: String
    }

    interface Image implements Resource & Node {
      id: ID!
      url: String
      thumbnail: String
    }

    # Added to complete the example: a query root type.
    type Query { image: Image }
  """)

  invalidSchema("two interfaces cannot implement each other")("""
    interface Node implements Named & Node {
      id: ID!
      name: String
    }

    interface Named implements Node & Named {
      id: ID!
      name: String
    }

    # Added to complete the example: a query root type.
    type Query { node: Node }
  """)

  // 3.7.1 Interface Extensions
  // https://spec.graphql.org/September2025/#sec-Interface-Extensions

  validSchema("an interface extension adds a field to the interface and its implementations")(
    """
    extend interface NamedEntity {
      nickname: String
    }

    extend type Person {
      nickname: String
    }

    extend type Business {
      nickname: String
    }

    # Added to complete the example: the base definitions and a query root type.
    interface NamedEntity { name: String }
    type Person implements NamedEntity { name: String age: Int }
    type Business implements NamedEntity { name: String employeeCount: Int }
    type Query { person: Person business: Business }
  """)

  validSchema("an interface extension can add a directive only")("""
    extend interface NamedEntity @addedDirective

    # Added to complete the example: the directive, the interface and a query root type.
    directive @addedDirective on INTERFACE
    interface NamedEntity { name: String }
    type Person implements NamedEntity { name: String }
    type Query { entity: NamedEntity }
  """)

  // 3.8 Unions
  // https://spec.graphql.org/September2025/#sec-Unions

  validSchema("a union type lists the object types which it can be")("""
    union SearchResult = Photo | Person

    type Person {
      name: String
      age: Int
    }

    type Photo {
      height: Int
      width: Int
    }

    type SearchQuery {
      firstSearchResult: SearchResult
    }

    # Added to complete the example: a query root type.
    schema { query: SearchQuery }
  """)

  invalidQuery("a selection set cannot request a field directly on a union", SearchSchema)("""
    {
      firstSearchResult {
        name
        height
      }
    }
  """)

  validQuery("an inline fragment reaches the fields of one member of a union", SearchSchema)("""
    {
      firstSearchResult {
        ... on Person {
          name
        }
        ... on Photo {
          height
        }
      }
    }
  """)

  validSchema("a union can start with a leading vertical bar")("""
    union SearchResult =
      | Photo
      | Person

    # Added to complete the example: the member types and a query root type.
    type Person { name: String }
    type Photo { height: Int }
    type Query { result: SearchResult }
  """)

  // 3.9 Enums
  // https://spec.graphql.org/September2025/#sec-Enums

  validSchema("an enum type lists its values")("""
    enum Direction {
      NORTH
      EAST
      SOUTH
      WEST
    }

    # Added to complete the example: a query root type.
    type Query { direction: Direction }
  """)

  // 3.10 Input Objects
  // https://spec.graphql.org/September2025/#sec-Input-Objects

  validSchema("an input object type defines a set of input fields")("""
    input Point2D {
      x: Float
      y: Float
    }

    # Added to complete the example: a query root type.
    type Query { nearest(point: Point2D): String }
  """)

  validSchema("an input object can refer to itself through a nullable field")("""
    input Example {
      self: Example
      value: String
    }

    # Added to complete the example: a query root type.
    type Query { example(arg: Example): String }
  """)

  validSchema("an input object can refer to itself through a list field")("""
    input Example {
      self: [Example!]!
      value: String
    }

    # Added to complete the example: a query root type.
    type Query { example(arg: Example): String }
  """)

  invalidSchema("an input object cannot refer to itself through a non-null field".fail)("""
    input Example {
      value: String
      self: Example!
    }

    # Added to complete the example: a query root type.
    type Query { example(arg: Example): String }
  """)

  invalidSchema("two input objects cannot form a cycle of non-null fields".fail)("""
    input First {
      second: Second!
      value: String
    }

    input Second {
      first: First!
      value: String
    }

    # Added to complete the example: a query root type.
    type Query { example(arg: First): String }
  """)

  validSchema("an input object field can be non-null")("""
    input ExampleInputObject {
      a: String
      b: Int!
    }

    # Added to complete the example: a query root type.
    type Query { example(arg: ExampleInputObject): String }
  """)

  // 3.10.1 OneOf Input Objects
  // https://spec.graphql.org/September2025/#sec-OneOf-Input-Objects

  validSchema("a oneOf input object accepts exactly one of its fields")("""
    input ExampleOneOfInputObject @oneOf {
      a: String
      b: Int
    }

    # Added to complete the example: a query root type.
    type Query { example(arg: ExampleOneOfInputObject): String }
  """)

  // 3.12 Non-Null
  // https://spec.graphql.org/September2025/#sec-Non-Null

  invalidQuery("a non-null argument cannot be omitted", NonNullArgSchema)("""
    {
      fieldWithNonNullArg
    }
  """)

  invalidQuery("a non-null argument cannot take the literal null", NonNullArgSchema)("""
    {
      fieldWithNonNullArg(nonNullArg: null)
    }
  """)

  invalidQuery(
    "a nullable variable cannot be supplied to a non-null argument",
    NonNullArgSchema,
    json"""{"var": "x"}""")("""
    query withNullableVariable($var: String) {
      fieldWithNonNullArg(nonNullArg: $var)
    }
  """)

  // 3.13 Directives
  // https://spec.graphql.org/September2025/#sec-Type-System.Directives

  // The specification puts a directive definition and a fragment definition in one document. A
  // request accepts executable definitions only, so this test case checks the grammar.
  parses("a custom directive definition and a use of that directive")("""
    directive @example on FIELD

    fragment SomeFragment on SomeType {
      field @example
    }
  """)

  validSchema("a directive definition can list its locations with a leading vertical bar")("""
    directive @example on
      | FIELD
      | FRAGMENT_SPREAD
      | INLINE_FRAGMENT

    # Added to complete the example: a query root type.
    type Query { field: String }
  """)

  validSchema("a directive can apply to a field definition and to an argument definition")("""
    directive @example on FIELD_DEFINITION | ARGUMENT_DEFINITION

    type SomeType {
      field(arg: Int @example): String @example
    }

    # Added to complete the example: a query root type.
    type Query { some: SomeType }
  """)

  validSchema("a repeatable directive can apply more than once at one location")("""
    directive @delegateField(name: String!) repeatable on OBJECT | INTERFACE

    type Book @delegateField(name: "pageCount") @delegateField(name: "author") {
      id: ID!
    }

    extend type Book @delegateField(name: "index")

    # Added to complete the example: a query root type.
    type Query { book: Book }
  """)

  invalidSchema("a directive cannot refer to itself".fail)("""
    directive @invalidExample(arg: String @invalidExample) on ARGUMENT_DEFINITION

    # Added to complete the example: a query root type.
    type Query { field: String }
  """)

  // 3.13.1 @skip
  // https://spec.graphql.org/September2025/#sec--skip

  validQuery(
    "@skip excludes a field when its argument is true",
    ExperimentalSchema,
    json"""{"someTest": true}""")("""
    query myQuery($someTest: Boolean!) {
      experimentalField @skip(if: $someTest)
    }
  """)

  // 3.13.2 @include
  // https://spec.graphql.org/September2025/#sec--include

  validQuery(
    "@include keeps a field when its argument is true",
    ExperimentalSchema,
    json"""{"someTest": true}""")("""
    query myQuery($someTest: Boolean!) {
      experimentalField @include(if: $someTest)
    }
  """)

  // 3.13.3 @deprecated
  // https://spec.graphql.org/September2025/#sec--deprecated

  validSchema("@deprecated applies to a field definition and to an argument definition")("""
    type ExampleType {
      newField: String
      oldField: String @deprecated(reason: "Use `newField`.")

      anotherField(
        newArg: String
        oldArg: String @deprecated(reason: "Use `newArg`.")
      ): String
    }

    # Added to complete the example: a query root type.
    type Query { example: ExampleType }
  """)

  invalidSchema("@deprecated cannot apply to a required argument".fail)("""
    type ExampleType {
      invalidField(
        newArg: String
        oldArg: String! @deprecated(reason: "Use `newArg`.")
      ): String
    }

    # Added to complete the example: a query root type.
    type Query { example: ExampleType }
  """)

  // 3.13.4 @specifiedBy
  // https://spec.graphql.org/September2025/#sec--specifiedBy

  validSchema("@specifiedBy applies to a custom scalar")("""
    scalar UUID @specifiedBy(url: "https://tools.ietf.org/html/rfc4122")

    # Added to complete the example: a query root type.
    type Query { id: UUID }
  """)

  // 3.13.5 @oneOf
  // https://spec.graphql.org/September2025/#sec--oneOf

  validSchema("@oneOf applies to an input object")("""
    input UserUniqueCondition @oneOf {
      id: ID
      username: String
      organizationAndEmail: OrganizationAndEmailInput
    }

    # Added to complete the example: the nested input type and a query root type.
    input OrganizationAndEmailInput { organization: String email: String }
    type Query { user(by: UserUniqueCondition): String }
  """)

  // -- Schemas which complete the examples above -----------------------------------------------

  lazy val MyNameSchema = schema"type Query { myName: String }"

  lazy val SetNameSchema = schema"""
    type Query { placeholder: Boolean }
    type Mutation { setName(name: String): SetNameResult }
    type SetNameResult { newName: String }
  """

  // The specification writes these selection sets against `Person`, so `Person` is the query
  // root operation type here.
  lazy val PersonSchema = schema"""
    scalar Url
    schema { query: Person }
    type Person { name: String age: Int picture: Url }
  """

  lazy val RelationshipSchema = schema"""
    scalar Url
    schema { query: Person }
    type Person { name: String age: Int picture: Url relationship: Person }
  """

  lazy val PictureSchema = schema"""
    scalar Url
    schema { query: Person }
    type Person { name: String picture(size: Int): Url }
  """

  lazy val ContactSchema = schema"""
    schema { query: Contact }
    interface NamedEntity { name: String }
    type Person implements NamedEntity { name: String age: Int }
    type Contact { entity: NamedEntity phoneNumber: String address: String }
  """

  lazy val SearchSchema = schema"""
    schema { query: SearchQuery }
    union SearchResult = Photo | Person
    type Person { name: String age: Int }
    type Photo { height: Int width: Int }
    type SearchQuery { firstSearchResult: SearchResult }
  """

  lazy val NonNullArgSchema =
    schema"type Query { fieldWithNonNullArg(nonNullArg: String!): String }"

  lazy val ExperimentalSchema = schema"type Query { experimentalField: String }"
}
