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

/**
 * Conformance test cases for section 5.8, Variables.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation.Variables
 */
final class ValidationVariablesSuite extends ValidationSuite {

  // 5.8.1 Variable Uniqueness
  // https://spec.graphql.org/September2025/#sec-Variable-Uniqueness

  invalidQuery("one operation must not declare a variable name twice".fail)("""
    query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
  """)

  validQuery("two operations can declare the same variable name")("""
    query A($atOtherHomes: Boolean) {
      ...HouseTrainedFragment
    }

    query B($atOtherHomes: Boolean) {
      ...HouseTrainedFragment
    }

    fragment HouseTrainedFragment on Query {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
  """)

  // 5.8.2 Variables Are Input Types
  // https://spec.graphql.org/September2025/#sec-Variables-Are-Input-Types

  validSchema("the query root type gains a field with a list argument")(
    ValidationSchema.base + ValidationSchema.variables)

  validQuery("a variable can have a scalar, an enum or an input object type")("""
    query takesBoolean($atOtherHomes: Boolean) {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }

    query takesComplexInput($search: FindDogInput) {
      findDog(searchBy: $search) {
        name
      }
    }

    query TakesListOfBooleanBang($booleans: [Boolean!]) {
      booleanList(booleanListArg: $booleans)
    }
  """)

  // The specification writes the next four operations as one counter-example block, and leaves
  // the selection set of each one empty. Grackle rejects each operation because its variable is
  // unused, which is rule 5.8.4, not rule 5.8.2. Each operation has its own test case, so that
  // one operation cannot hide another.

  invalidQuery("a variable must not have an object type")("""
    query takesCat($cat: Cat) {
      # ...
    }
  """)

  invalidQuery("a variable must not have a non-null object type")("""
    query takesDogBang($dog: Dog!) {
      # ...
    }
  """)

  invalidQuery("a variable must not have a list of interface type")("""
    query takesListOfPet($pets: [Pet]) {
      # ...
    }
  """)

  invalidQuery("a variable must not have a union type")("""
    query takesCatOrDog($catOrDog: CatOrDog) {
      # ...
    }
  """)

  // The four test cases above pass for the reason of rule 5.8.4, so this test case isolates rule
  // 5.8.2. The operation uses the variable, which leaves rule 5.8.2 as the only reason to reject
  // the document. Grackle has no check for that rule and accepts the document. Rule 5.8.5 also
  // forbids this usage, and grackle has no check for that rule either.
  invalidQuery("a variable of object type which the operation uses".fail)("""
    query takesCat($cat: Cat) {
      findDog(searchBy: $cat) {
        name
      }
    }
  """)

  // 5.8.3 All Variable Uses Defined
  // https://spec.graphql.org/September2025/#sec-All-Variable-Uses-Defined

  validQuery("a variable use inside the operation which declares it")("""
    query variableIsDefined($atOtherHomes: Boolean) {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
  """)

  invalidQuery("a variable use without a declaration")("""
    query variableIsNotDefined {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
  """)

  validQuery("a variable use inside a fragment which the operation spreads")("""
    query variableIsDefinedUsedInSingleFragment($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  invalidQuery("a variable use inside a fragment without a declaration")("""
    query variableIsNotDefinedUsedInSingleFragment {
      dog {
        ...isHouseTrainedFragment
      }
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  invalidQuery("a variable use inside a nested fragment without a declaration")("""
    query variableIsNotDefinedUsedInNestedFragment {
      dog {
        ...outerHouseTrainedFragment
      }
    }

    fragment outerHouseTrainedFragment on Dog {
      ...isHouseTrainedFragment
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  validQuery(
    "every operation which reaches a fragment declares the variables of that fragment")("""
    query houseTrainedQueryOne($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    query houseTrainedQueryTwo($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  invalidQuery("one operation which reaches a fragment lacks the declaration")("""
    query houseTrainedQueryOne($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    query houseTrainedQueryTwoNotDefined {
      dog {
        ...isHouseTrainedFragment
      }
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  // 5.8.4 All Variables Used
  // https://spec.graphql.org/September2025/#sec-All-Variables-Used

  invalidQuery("a declared variable which the operation never uses")("""
    query variableUnused($atOtherHomes: Boolean) {
      dog {
        isHouseTrained
      }
    }
  """)

  validQuery("a fragment which the operation spreads can use the variable")("""
    query variableUsedInFragment($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  invalidQuery("a fragment which the operation spreads does not use the variable")("""
    query variableNotUsedWithinFragment($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedWithoutVariableFragment
      }
    }

    fragment isHouseTrainedWithoutVariableFragment on Dog {
      isHouseTrained
    }
  """)

  invalidQuery("one operation of a document declares a variable which it never uses")("""
    query queryWithUsedVar($atOtherHomes: Boolean) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    query queryWithExtraVar($atOtherHomes: Boolean, $extra: Int) {
      dog {
        ...isHouseTrainedFragment
      }
    }

    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }
  """)

  // 5.8.5 All Variable Usages Are Allowed
  // https://spec.graphql.org/September2025/#sec-All-Variable-Usages-Are-Allowed

  // Rule 5.8.5 rejects a document for the declared type of a variable, whatever value the
  // request supplies for it. Grackle has no check for the rule. It reports a value which does
  // not fit the argument, which is a different check, and it runs that check after it
  // substitutes the variable value. A counter-example whose declared type can hold a value which
  // fits the argument therefore needs such a value here. Without one, the test case would pass
  // because the value is absent, not because the rule holds.

  invalidQuery("an Int variable cannot go into a Boolean argument".fail)("""
    query intCannotGoIntoBoolean($intArg: Int) {
      arguments {
        booleanArgField(booleanArg: $intArg)
      }
    }
  """)

  invalidQuery("a list variable cannot go into a non-list argument".fail)("""
    query booleanListCannotGoIntoBoolean($booleanListArg: [Boolean]) {
      arguments {
        booleanArgField(booleanArg: $booleanListArg)
      }
    }
  """)

  invalidQuery(
    "a nullable variable cannot go into a non-null argument".fail,
    vars = json"""{"booleanArg": true}""")("""
    query booleanArgQuery($booleanArg: Boolean) {
      arguments {
        nonNullBooleanArgField(nonNullBooleanArg: $booleanArg)
      }
    }
  """)

  validQuery(
    "a non-null list variable can go into a nullable list argument",
    vars = json"""{"nonNullBooleanList": [true]}""")("""
    query nonNullListToList($nonNullBooleanList: [Boolean]!) {
      arguments {
        booleanListArgField(booleanListArg: $nonNullBooleanList)
      }
    }
  """)

  invalidQuery(
    "a nullable list variable cannot go into a non-null list argument".fail,
    vars = json"""{"booleanList": [true]}""")("""
    query listToNonNullList($booleanList: [Boolean]) {
      arguments {
        nonNullBooleanListField(nonNullBooleanListArg: $booleanList)
      }
    }
  """)

  validQuery(
    "a non-null variable can go into a oneOf input field",
    vars = json"""{"cat": {"name": "Brontie"}}""")("""
    mutation addCat($cat: CatInput!) {
      addPet(pet: { cat: $cat }) {
        name
      }
    }

    mutation addCatWithDefault($cat: CatInput! = { name: "Brontie" }) {
      addPet(pet: { cat: $cat }) {
        name
      }
    }
  """)

  invalidQuery(
    "a nullable variable cannot go into a oneOf input field".fail,
    vars = json"""{"cat": {"name": "Brontie"}}""")("""
    mutation addNullableCat($cat: CatInput) {
      addPet(pet: { cat: $cat }) {
        name
      }
    }
  """)

  validQuery("a nullable variable can go into a non-null argument which has a default".fail)("""
    query booleanArgQueryWithDefault($booleanArg: Boolean) {
      arguments {
        optionalNonNullBooleanArgField(optionalBooleanArg: $booleanArg)
      }
    }
  """)

  validQuery("a nullable variable with a default can go into a non-null argument")("""
    query booleanArgQueryWithDefault($booleanArg: Boolean = true) {
      arguments {
        nonNullBooleanArgField(nonNullBooleanArg: $booleanArg)
      }
    }
  """)
}
