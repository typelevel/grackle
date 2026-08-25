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

/**
 * Conformance test cases for section 5.4, Arguments.
 *
 * Each test case adds a driver operation, as [[ValidationFieldsSuite]] describes.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation.Arguments
 */
final class ValidationArgumentsSuite extends ValidationSuite {

  // 5.4.1 Argument Names
  // https://spec.graphql.org/September2025/#sec-Argument-Names

  validQuery("an argument name must be defined on the field or on the directive")("""
    query driver {
      a: dog { ...argOnRequiredArg }
      b: dog { ...argOnOptional }
    }

    fragment argOnRequiredArg on Dog {
      doesKnowCommand(dogCommand: SIT)
    }

    fragment argOnOptional on Dog {
      isHouseTrained(atOtherHomes: true) @include(if: true)
    }
  """)

  invalidQuery("an argument name which the field does not define is rejected")("""
    query driver {
      dog { ...invalidArgName }
    }

    fragment invalidArgName on Dog {
      doesKnowCommand(command: CLEAN_UP_HOUSE)
    }
  """)

  invalidQuery("an argument name which the directive does not define is rejected")("""
    query driver {
      dog { ...invalidArgName }
    }

    fragment invalidArgName on Dog {
      isHouseTrained(atOtherHomes: true) @include(unless: false)
    }
  """)

  validSchema("a type whose fields declare several arguments")(
    ValidationSchema.base + ValidationSchema.arguments)

  validQuery("the order of the arguments of a field is free")("""
    query driver {
      a: arguments { ...multipleArgs }
      b: arguments { ...multipleArgsReverseOrder }
    }

    fragment multipleArgs on Arguments {
      multipleRequirements(x: 1, y: 2)
    }

    fragment multipleArgsReverseOrder on Arguments {
      multipleRequirements(y: 2, x: 1)
    }
  """)

  // 5.4.3 Required Arguments
  // https://spec.graphql.org/September2025/#sec-Required-Arguments

  validQuery("a required argument which the selection supplies")("""
    query driver {
      a: arguments { ...goodBooleanArg }
      b: arguments { ...goodNonNullArg }
    }

    fragment goodBooleanArg on Arguments {
      booleanArgField(booleanArg: true)
    }

    fragment goodNonNullArg on Arguments {
      nonNullBooleanArgField(nonNullBooleanArg: true)
    }
  """)

  validQuery("a nullable argument can be omitted")("""
    query driver {
      arguments { ...goodBooleanArgDefault }
    }

    fragment goodBooleanArgDefault on Arguments {
      booleanArgField
    }
  """)

  invalidQuery("a required argument must not be omitted")("""
    query driver {
      arguments { ...missingRequiredArg }
    }

    fragment missingRequiredArg on Arguments {
      nonNullBooleanArgField
    }
  """)

  invalidQuery("a required argument must not take the literal null")("""
    query driver {
      arguments { ...missingRequiredArg }
    }

    fragment missingRequiredArg on Arguments {
      nonNullBooleanArgField(nonNullBooleanArg: null)
    }
  """)
}
