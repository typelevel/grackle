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

import cats.effect.{IO, Ref}
import io.circe.literal._

/**
 * Conformance test cases for section 6, Execution.
 *
 * Each test case runs its document against a mapping in [[ExecutionMappings]] and asserts on
 * the response. Where the specification states the response, the assertion quotes it. Where the
 * specification states the outcome in prose, the assertion follows that prose.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Execution
 */
final class ExecutionSuite extends ConformanceSuite {

  // 6.2.3 Subscription
  // https://spec.graphql.org/September2025/#sec-Subscription

  test("a subscription publishes one response per event") {
    val responses =
      ExecutionMappings
        .Chat
        .compileAndRunSubscription("""
          subscription NewMessages {
            newMessage(roomId: 123) {
              sender
              text
            }
          }
        """)
        .compile
        .toList

    assertIO(
      responses,
      List(json"""
        {
          "data": {
            "newMessage": {
              "sender": "Hagrid",
              "text": "You're a wizard!"
            }
          }
        }
      """)
    )
  }

  // 6.3.2 Field Collection
  // https://spec.graphql.org/September2025/#sec-Field-Collection

  /**
   * The document of section 6.3.2, which the specification states twice to state two outcomes.
   */
  private val fieldCollectionDoc = """
    {
      a {
        subfield1
      }
      ...ExampleFragment
    }

    fragment ExampleFragment on Query {
      a {
        subfield2
      }
      b
    }
  """

  // The specification states the outcome in prose: field collection yields two entries, `a` and
  // `b`, and the field set for `a` holds both instances of the field.
  test("field collection yields one entry per response name") {
    val prog =
      for {
        resolutions <- Ref[IO].of(0)
        res <- new ExecutionMappings.Collection(resolutions).compileAndRun(fieldCollectionDoc)
      } yield res

    assertIO(
      prog,
      json"""
        {
          "data": {
            "a": {
              "subfield1": "one",
              "subfield2": "two"
            },
            "b": "three"
          }
        }
      """
    )
  }

  // The specification repeats the document above to state a second outcome: after the executor
  // resolves `a`, it merges the two selection sets, so `subfield1` and `subfield2` resolve in
  // the same phase against the same value. The response alone cannot show that outcome, so this
  // test case counts how many times the executor resolved `a`.
  test("the sub-selections of one response name merge into one phase") {
    val prog =
      for {
        resolutions <- Ref[IO].of(0)
        _ <- new ExecutionMappings.Collection(resolutions).compileAndRun(fieldCollectionDoc)
        count <- resolutions.get
      } yield count

    assertIO(prog, 1)
  }

  // 6.3.4 Normal and Serial Execution
  // https://spec.graphql.org/September2025/#sec-Normal-and-Serial-Execution

  test("the root fields of a query run in any order, and the response holds both") {
    val prog =
      for {
        state <- Ref[IO].of(List.empty[String])
        res <- new ExecutionMappings.Person(state).compileAndRun("""
          {
            birthday {
              month
            }
            address {
              street
            }
          }
        """)
      } yield res

    assertIO(
      prog,
      json"""
        {
          "data": {
            "birthday": {
              "month": "January"
            },
            "address": {
              "street": "Main Street"
            }
          }
        }
      """
    )
  }

  test("the root fields of a mutation run in serial, in document order") {
    val prog =
      for {
        state <- Ref[IO].of(List.empty[String])
        res <- new ExecutionMappings.Person(state).compileAndRun(
          """
            mutation ChangeBirthdayAndAddress($newBirthday: String!, $newAddress: String!) {
              changeBirthday(birthday: $newBirthday) {
                month
              }
              changeAddress(address: $newAddress) {
                street
              }
            }
          """,
          untypedVars = Some(json"""{"newBirthday": "January", "newAddress": "Main Street"}""")
        )
        order <- state.get
      } yield (res, order)

    assertIO(
      prog,
      (
        json"""
          {
            "data": {
              "changeBirthday": {
                "month": "January"
              },
              "changeAddress": {
                "street": "Main Street"
              }
            }
          }
        """,
        List("changeBirthday", "changeAddress"))
    )
  }

  // The specification marks this block as a selection set of a mutation, not as a document. This
  // test case wraps it in `mutation { ... }`, which is the smallest document which holds it. The
  // expected data is the response which the specification states.
  test("aliases let a mutation call one field more than once, in order") {
    val prog =
      for {
        log <- Ref[IO].of(List.empty[Int])
        res <- new ExecutionMappings.Numbers(log).compileAndRun("""
          mutation {
            first: changeTheNumber(newNumber: 1) {
              theNumber
            }
            second: changeTheNumber(newNumber: 3) {
              theNumber
            }
            third: changeTheNumber(newNumber: 2) {
              theNumber
            }
          }
        """)
        order <- log.get
      } yield (res, order)

    assertIO(
      prog,
      (
        json"""
          {
            "data": {
              "first": {
                "theNumber": 1
              },
              "second": {
                "theNumber": 3
              },
              "third": {
                "theNumber": 2
              }
            }
          }
        """,
        List(1, 3, 2))
    )
  }
}
