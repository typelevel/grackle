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
 * Conformance test cases for section 5.3, Fields.
 *
 * The specification writes most of these examples as fragment definitions only. A request needs
 * at least one operation, so each test case adds a driver operation which spreads the
 * fragments. The driver gives each fragment its own aliased parent field, so that two fragments
 * never merge into one selection set.
 *
 * A counter-example block which holds more than one fragment becomes one test case per
 * fragment. A document is rejected as a whole, so one test case for the whole block would pass
 * while one fragment fails.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation.Fields
 */
final class ValidationFieldsSuite extends ValidationSuite {

  // 5.3.1 Field Selections
  // https://spec.graphql.org/September2025/#sec-Field-Selections

  invalidQuery("a field which the type of the selection set does not define")("""
    query driver {
      dog { ...fieldNotDefined }
    }

    fragment fieldNotDefined on Dog {
      meowVolume
    }
  """)

  invalidQuery("an alias cannot rename an undefined field to a defined one")("""
    query driver {
      dog { ...aliasedLyingFieldTargetNotDefined }
    }

    fragment aliasedLyingFieldTargetNotDefined on Dog {
      barkVolume: kawVolume
    }
  """)

  validQuery("a selection on an interface can request a field of that interface")("""
    query driver {
      pet { ...interfaceFieldSelection }
    }

    fragment interfaceFieldSelection on Pet {
      name
    }
  """)

  invalidQuery("a selection on an interface cannot request a field of one implementation")("""
    query driver {
      pet { ...definedOnImplementersButNotInterface }
    }

    fragment definedOnImplementersButNotInterface on Pet {
      nickname
    }
  """)

  validQuery("a selection on a union can request __typename and use inline fragments")("""
    query driver {
      catOrDog { ...inDirectFieldSelectionOnUnion }
    }

    fragment inDirectFieldSelectionOnUnion on CatOrDog {
      __typename
      ... on Pet {
        name
      }
      ... on Dog {
        barkVolume
      }
    }
  """)

  invalidQuery("a selection on a union cannot request a field directly")("""
    query driver {
      catOrDog { ...directFieldSelectionOnUnion }
    }

    fragment directFieldSelectionOnUnion on CatOrDog {
      name
      barkVolume
    }
  """)

  // 5.3.2 Field Selection Merging
  // https://spec.graphql.org/September2025/#sec-Field-Selection-Merging

  validQuery("two identical fields merge")("""
    query driver {
      a: dog { ...mergeIdenticalFields }
      b: dog { ...mergeIdenticalAliasesAndFields }
    }

    fragment mergeIdenticalFields on Dog {
      name
      name
    }

    fragment mergeIdenticalAliasesAndFields on Dog {
      otherName: name
      otherName: name
    }
  """)

  invalidQuery("one response key must not point at two different fields")("""
    query driver {
      dog { ...conflictingBecauseAlias }
    }

    fragment conflictingBecauseAlias on Dog {
      name: nickname
      name
    }
  """)

  validQuery(
    "two identical fields with identical arguments merge",
    vars = json"""{"dogCommand": "SIT"}""")("""
    query driver($dogCommand: DogCommand!) {
      a: dog { ...mergeIdenticalFieldsWithIdenticalArgs }
      b: dog { ...mergeIdenticalFieldsWithIdenticalValues }
    }

    fragment mergeIdenticalFieldsWithIdenticalArgs on Dog {
      doesKnowCommand(dogCommand: SIT)
      doesKnowCommand(dogCommand: SIT)
    }

    fragment mergeIdenticalFieldsWithIdenticalValues on Dog {
      doesKnowCommand(dogCommand: $dogCommand)
      doesKnowCommand(dogCommand: $dogCommand)
    }
  """)

  // The specification writes the four fragments below as one counter-example block. Each
  // fragment is a separate case, so each one has its own test case.

  invalidQuery("two literal arguments with different values conflict")("""
    query driver {
      dog { ...conflictingArgsOnValues }
    }

    fragment conflictingArgsOnValues on Dog {
      doesKnowCommand(dogCommand: SIT)
      doesKnowCommand(dogCommand: HEEL)
    }
  """)

  invalidQuery(
    "a literal argument and a variable argument conflict",
    vars = json"""{"dogCommand": "SIT"}""")("""
    query driver($dogCommand: DogCommand!) {
      dog { ...conflictingArgsValueAndVar }
    }

    fragment conflictingArgsValueAndVar on Dog {
      doesKnowCommand(dogCommand: SIT)
      doesKnowCommand(dogCommand: $dogCommand)
    }
  """)

  invalidQuery(
    "two different variable arguments conflict",
    vars = json"""{"varOne": "SIT", "varTwo": "HEEL"}""")("""
    query driver($varOne: DogCommand!, $varTwo: DogCommand!) {
      dog { ...conflictingArgsWithVars }
    }

    fragment conflictingArgsWithVars on Dog {
      doesKnowCommand(dogCommand: $varOne)
      doesKnowCommand(dogCommand: $varTwo)
    }
  """)

  invalidQuery("an argument and an absent argument conflict")("""
    query driver {
      dog { ...differingArgs }
    }

    fragment differingArgs on Dog {
      doesKnowCommand(dogCommand: SIT)
      doesKnowCommand
    }
  """)

  validQuery("two fields of mutually exclusive types can differ")("""
    query driver {
      a: pet { ...safeDifferingFields }
      b: pet { ...safeDifferingArgs }
    }

    fragment safeDifferingFields on Pet {
      ... on Dog {
        volume: barkVolume
      }
      ... on Cat {
        volume: meowVolume
      }
    }

    fragment safeDifferingArgs on Pet {
      ... on Dog {
        doesKnowCommand(dogCommand: SIT)
      }
      ... on Cat {
        doesKnowCommand(catCommand: JUMP)
      }
    }
  """)

  invalidQuery("two fields of mutually exclusive types must return the same type")("""
    query driver {
      pet { ...conflictingDifferingResponses }
    }

    fragment conflictingDifferingResponses on Pet {
      ... on Dog {
        someValue: nickname
      }
      ... on Cat {
        someValue: meowVolume
      }
    }
  """)

  // 5.3.3 Leaf Field Selections
  // https://spec.graphql.org/September2025/#sec-Leaf-Field-Selections

  validQuery("a field of a scalar type takes no selection set")("""
    query driver {
      dog { ...scalarSelection }
    }

    fragment scalarSelection on Dog {
      barkVolume
    }
  """)

  invalidQuery("a field of a scalar type must not take a selection set")("""
    query driver {
      dog { ...scalarSelectionsNotAllowedOnInt }
    }

    fragment scalarSelectionsNotAllowedOnInt on Dog {
      barkVolume {
        sinceWhen
      }
    }
  """)

  validSchema("the query root type gains a field of object, interface and union type")(
    ValidationSchema.base + ValidationSchema.leafFields)

  invalidQuery("a field of object type must take a selection set")("""
    query directQueryOnObjectWithoutSubFields {
      human
    }
  """)

  invalidQuery("a field of interface type must take a selection set")("""
    query directQueryOnInterfaceWithoutSubFields {
      pet
    }
  """)

  invalidQuery("a field of union type must take a selection set")("""
    query directQueryOnUnionWithoutSubFields {
      catOrDog
    }
  """)

  validQuery("a field of object type with a selection set")("""
    query directQueryOnObjectWithSubFields {
      human {
        name
      }
    }
  """)
}
