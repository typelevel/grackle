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

import grackle.Schema

/**
 * The schema which the examples of section 5, Validation, run against.
 *
 * [[base]] holds the schema of the introduction to section 5. [[sdl]] adds the definitions
 * which the later examples introduce, and the definitions which complete those examples.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation
 */
object ValidationSchema {

  /**
   * The example schema of the introduction to section 5.
   */
  val base: String = """
    type Query {
      dog: Dog
      findDog(searchBy: FindDogInput): Dog
    }

    type Mutation {
      addPet(pet: PetInput!): Pet
      addPets(pets: [PetInput!]!): [Pet]
    }

    enum DogCommand {
      SIT
      DOWN
      HEEL
    }

    type Dog implements Pet {
      name: String!
      nickname: String
      barkVolume: Int
      doesKnowCommand(dogCommand: DogCommand!): Boolean!
      isHouseTrained(atOtherHomes: Boolean): Boolean!
      owner: Human
    }

    interface Sentient {
      name: String!
    }

    interface Pet {
      name: String!
    }

    type Alien implements Sentient {
      name: String!
      homePlanet: String
    }

    type Human implements Sentient {
      name: String!
      pets: [Pet!]
    }

    enum CatCommand {
      JUMP
    }

    type Cat implements Pet {
      name: String!
      nickname: String
      doesKnowCommand(catCommand: CatCommand!): Boolean!
      meowVolume: Int
    }

    union CatOrDog = Cat | Dog
    union DogOrHuman = Dog | Human
    union HumanOrAlien = Human | Alien

    input FindDogInput {
      name: String
      owner: String
    }

    input CatInput {
      name: String!
      nickname: String
      meowVolume: Int
    }

    input DogInput {
      name: String!
      nickname: String
      barkVolume: Int
    }

    input PetInput @oneOf {
      cat: CatInput
      dog: DogInput
    }
  """

  /**
   * The definitions which rule 5.3.3, Leaf Field Selections, adds to [[base]].
   */
  val leafFields: String = """
    extend type Query {
      human: Human
      pet: Pet
      catOrDog: CatOrDog
    }
  """

  /**
   * The definitions which rule 5.4.1, Argument Names, adds to [[base]].
   */
  val arguments: String = """
    type Arguments {
      multipleRequirements(x: Int!, y: Int!): Int!
      booleanArgField(booleanArg: Boolean): Boolean
      floatArgField(floatArg: Float): Float
      intArgField(intArg: Int): Int
      nonNullBooleanArgField(nonNullBooleanArg: Boolean!): Boolean!
      booleanListArgField(booleanListArg: [Boolean]!): [Boolean]
      optionalNonNullBooleanArgField(optionalBooleanArg: Boolean! = false): Boolean!
    }

    extend type Query {
      arguments: Arguments
    }
  """

  /**
   * The definitions which rule 5.8.2, Variables Are Input Types, adds to [[base]].
   */
  val variables: String = """
    extend type Query {
      booleanList(booleanListArg: [Boolean!]): Boolean
    }
  """

  /**
   * The definitions which the examples of section 5 add to [[base]].
   */
  val fromExamples: String = leafFields + arguments + variables

  /**
   * The definitions which complete the examples of section 5.
   *
   * The specification names these types and fields in its examples without defining them. A
   * counter-example must fail for the reason which the rule states, not because a name is
   * missing, so this suite defines them.
   */
  val completions: String = """
    # Rule 5.2.4.1, Single Root Field, needs a subscription root type.
    type Subscription {
      newMessage: Message
      disallowedSecondRootField: Boolean
    }

    type Message {
      body: String
      sender: String
    }

    # Rule 5.2.2.1, Operation Name Uniqueness, selects `mutateDog`.
    extend type Mutation {
      mutateDog: DogMutation
    }

    type DogMutation {
      id: ID
    }

    # Rule 5.8.5, All Variable Usages Are Allowed, selects `nonNullBooleanListField`.
    extend type Arguments {
      nonNullBooleanListField(nonNullBooleanListArg: [Boolean]!): [Boolean]
    }

    # Rule 5.5.2.3.4, Abstract Spreads in Abstract Scope, writes these two interfaces inline.
    interface Node {
      id: ID!
    }

    interface Resource implements Node {
      id: ID!
      url: String
    }

    type Image implements Resource & Node {
      id: ID!
      url: String
      thumbnail: String
    }

    # Fields which let a driver operation spread the fragments of the examples.
    extend type Query {
      cat: Cat
      sentient: Sentient
      humanOrAlien: HumanOrAlien
      dogOrHuman: DogOrHuman
      node: Node
    }
  """

  /**
   * The complete schema.
   */
  val sdl: String = base + fromExamples + completions

  /**
   * The parsed form of [[sdl]].
   */
  lazy val schema: Schema = ConformanceSuite.mkSchema(sdl)
}

/**
 * Base class for the conformance suites of section 5, Validation.
 *
 * Every suite of section 5 runs its query test cases against [[ValidationSchema.schema]].
 */
abstract protected[conformance] class ValidationSuite extends ConformanceSuite {
  override lazy val defaultSchema: Schema = ValidationSchema.schema
}
