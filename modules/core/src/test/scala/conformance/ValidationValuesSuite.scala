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
 * Conformance test cases for section 5.6, Values.
 *
 * Two examples of this section mix fragment definitions and operations. Each such test case
 * adds a driver operation which spreads the fragments, as [[ValidationFieldsSuite]] describes.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Values
 */
final class ValidationValuesSuite extends ValidationSuite {

  // 5.6.1 Values of Correct Type
  // https://spec.graphql.org/September2025/#sec-Values-of-Correct-Type

  // The specification writes the example and the counter-example of this rule as one block each.
  // Each block holds several operations and fragments, and each one of those is a separate case.
  // One test case per block would pass while one case fails, so each case has its own test case
  // here.

  validQuery("a Boolean literal at a Boolean location")("""
    query driver {
      arguments { ...goodBooleanArg }
    }

    fragment goodBooleanArg on Arguments {
      booleanArgField(booleanArg: true)
    }
  """)

  // Grackle rejects the Int literal `123` at a `Float` location. Section 3.5.2, Float, requires
  // that coercion.
  validQuery("an Int literal at a Float location".fail)("""
    query driver {
      arguments { ...coercedIntIntoFloatArg }
    }

    fragment coercedIntIntoFloatArg on Arguments {
      # Note: The input coercion rules for Float allow Int literals.
      floatArgField(floatArg: 123)
    }
  """)

  validQuery("an input object literal as the default value of a variable")("""
    query goodComplexDefaultValue($search: FindDogInput = { name: "Fido" }) {
      findDog(searchBy: $search) {
        name
      }
    }
  """)

  validQuery("a oneOf input object literal as the default value of a variable")("""
    mutation addPet($pet: PetInput! = { cat: { name: "Brontie" } }) {
      addPet(pet: $pet) {
        name
      }
    }
  """)

  invalidQuery("a String literal at an Int location")("""
    query driver {
      arguments { ...stringIntoInt }
    }

    fragment stringIntoInt on Arguments {
      intArgField(intArg: "123")
    }
  """)

  invalidQuery("an Int literal at a String location inside an input object")("""
    query badComplexValue {
      findDog(searchBy: { name: 123 }) {
        name
      }
    }
  """)

  invalidQuery("a oneOf input object literal with no field")("""
    mutation oneOfWithNoFields {
      addPet(pet: {}) {
        name
      }
    }
  """)

  // The rule counts the fields which the literal writes, so it rejects this document whatever
  // value the request supplies for `$dog`. Grackle counts the fields after it substitutes the
  // variable value, so it accepts the document when the request supplies no value.
  invalidQuery("a oneOf input object literal with two fields".fail)("""
    mutation oneOfWithTwoFields($dog: DogInput) {
      addPet(pet: { cat: { name: "Brontie" }, dog: $dog }) {
        name
      }
    }
  """)

  // Rule 5.8.5 forbids a nullable variable at the field of a oneOf input object. Grackle has no
  // check for that rule, so it accepts the document once `$dog` has a value.
  invalidQuery(
    "a nullable variable at the field of a oneOf input object inside a list".fail,
    vars = json"""{"dog": {"name": "Fido"}}""")("""
    mutation listOfOneOfWithNullableVariable($dog: DogInput) {
      addPets(pets: [{ dog: $dog }]) {
        name
      }
    }
  """)

  // 5.6.2 Input Object Field Names
  // https://spec.graphql.org/September2025/#sec-Input-Object-Field-Names

  validQuery("an input object field name must be defined on the input object type")("""
    {
      findDog(searchBy: { name: "Fido" }) {
        name
      }
    }
  """)

  invalidQuery("an input object field name which the input object type does not define")("""
    {
      findDog(searchBy: { favoriteCookieFlavor: "Bacon" }) {
        name
      }
    }
  """)

  // 5.6.3 Input Object Field Uniqueness
  // https://spec.graphql.org/September2025/#sec-Input-Object-Field-Uniqueness

  invalidQuery("an input object must not name one field twice".fail, FieldArgSchema)("""
    {
      field(arg: { field: true, field: false })
    }
  """)

  // -- Schemas which complete the examples above -----------------------------------------------

  lazy val FieldArgSchema = schema"""
    type Query { field(arg: ExampleInput): Boolean }
    input ExampleInput { field: Boolean }
  """
}
