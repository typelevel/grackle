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
import cats.implicits._
import fs2.Stream

import grackle._
import grackle.Query.Binding
import grackle.QueryCompiler._
import grackle.Value.IntValue
import grackle.syntax._

/**
 * Mappings for the examples of section 6, Execution.
 *
 * The specification names the types and the fields of these examples in its prose. It supplies
 * no data, so each mapping here chooses values which the response assertions then quote.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Execution
 */
object ExecutionMappings {

  val CollectionSchema: Schema =
    schema"""
      type Query { a: A b: String }
      type A { subfield1: String subfield2: String }
    """

  val PersonSchema: Schema =
    schema"""
      type Query { birthday: Birthday address: Address }
      type Mutation {
        changeBirthday(birthday: String!): Birthday
        changeAddress(address: String!): Address
      }
      type Birthday { month: String }
      type Address { street: String }
    """

  val NumbersSchema: Schema =
    schema"""
      type Query { theNumber: Int! }
      type Mutation { changeTheNumber(newNumber: Int!): NumberHolder! }
      type NumberHolder { theNumber: Int! }
    """

  /**
   * The chat application of section 6.2.3, Subscription.
   *
   * The specification states the sender and the text of the published message.
   */
  object Chat extends ValueMapping[IO] {
    case class Message(sender: String, text: String)

    val messages: Map[Int, List[Message]] =
      Map(123 -> List(Message("Hagrid", "You're a wizard!")))

    val schema =
      schema"""
        type Query { placeholder: Boolean }
        type Subscription { newMessage(roomId: Int!): Message! }
        type Message { sender: String! text: String! }
      """

    val QueryType = schema.ref("Query")
    val SubscriptionType = schema.ref("Subscription")
    val MessageType = schema.ref("Message")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(ValueField("placeholder", _ => Some(true)))),
        ObjectMapping(
          SubscriptionType,
          List(
            RootStream.computeCursor("newMessage")((path, env) =>
              Stream
                .emits(env.get[Int]("roomId").toList.flatMap(messages.getOrElse(_, Nil)))
                .map(m => Result(valueCursor(path, env, m))))
          )
        ),
        ValueObjectMapping[Message](
          tpe = MessageType,
          fieldMappings = List(ValueField("sender", _.sender), ValueField("text", _.text)))
      )

    override val selectElaborator: SelectElaborator =
      SelectElaborator {
        case (SubscriptionType, "newMessage", List(Binding("roomId", IntValue(n)))) =>
          Elab.env("roomId" -> n)
      }
  }

  /**
   * The two fields of section 6.3.2, Field Collection.
   *
   * The specification collects two instances of the field `a` and one of the field `b`. The
   * response holds one entry for `a`, with the subfields of both instances.
   *
   * The field `a` counts its own resolutions in `resolutions`, so a test case can read how many
   * times the executor resolved it.
   */
  final class Collection(resolutions: Ref[IO, Int]) extends ValueMapping[IO] {
    case class A(subfield1: String, subfield2: String)

    val schema = CollectionSchema

    val QueryType = schema.ref("Query")
    val AType = schema.ref("A")

    val typeMappings =
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            RootEffect.computeCursor("a")((path, env) =>
              resolutions
                .update(_ + 1)
                .as(Result(valueCursor(path, env, Some(A("one", "two")))))),
            ValueField[Unit]("b", _ => Some("three"))
          )
        ),
        ValueObjectMapping[A](
          tpe = AType,
          fieldMappings = List(
            ValueField("subfield1", a => Some(a.subfield1)),
            ValueField("subfield2", a => Some(a.subfield2))))
      )
  }

  /**
   * The person of section 6.3.4, Normal and Serial Execution.
   *
   * `changeBirthday` and `changeAddress` both write to `state`, so a test case can read the
   * order in which the executor ran them.
   */
  final class Person(state: Ref[IO, List[String]]) extends ValueMapping[IO] {
    case class Birthday(month: String)
    case class Address(street: String)

    val schema = PersonSchema

    val QueryType = schema.ref("Query")
    val MutationType = schema.ref("Mutation")
    val BirthdayType = schema.ref("Birthday")
    val AddressType = schema.ref("Address")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(
            ValueField("birthday", _ => Some(Birthday("January"))),
            ValueField("address", _ => Some(Address("Main Street")))
          )),
        ObjectMapping(
          MutationType,
          List(
            RootEffect.computeCursor("changeBirthday")((path, env) =>
              record(env, "changeBirthday")
                .map(v => Result(valueCursor(path, env, Some(Birthday(v)))))),
            RootEffect.computeCursor("changeAddress")((path, env) =>
              record(env, "changeAddress").map(v =>
                Result(valueCursor(path, env, Some(Address(v))))))
          )
        ),
        ValueObjectMapping[Birthday](
          tpe = BirthdayType,
          fieldMappings = List(ValueField("month", b => Some(b.month)))),
        ValueObjectMapping[Address](
          tpe = AddressType,
          fieldMappings = List(ValueField("street", a => Some(a.street))))
      )

    override val selectElaborator: SelectElaborator =
      SelectElaborator {
        case (MutationType, "changeBirthday" | "changeAddress", List(Binding(_, arg))) =>
          Elab.env("arg" -> arg)
      }

    private def record(env: Env, fieldName: String): IO[String] = {
      val value = env.get[Value]("arg").collect { case Value.StringValue(s) => s }.orEmpty
      state.update(_ :+ fieldName).as(value)
    }
  }

  /**
   * The number holder of section 6.3.4, Normal and Serial Execution.
   *
   * `changeTheNumber` appends its argument to `log`, so a test case can read the order in which
   * the executor ran the three aliases of the mutation.
   */
  final class Numbers(log: Ref[IO, List[Int]]) extends ValueMapping[IO] {
    case class NumberHolder(theNumber: Int)

    val schema = NumbersSchema

    val QueryType = schema.ref("Query")
    val MutationType = schema.ref("Mutation")
    val NumberHolderType = schema.ref("NumberHolder")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(ValueField("theNumber", _ => 0))),
        ObjectMapping(
          MutationType,
          List(
            RootEffect.computeCursor("changeTheNumber")((path, env) => {
              val n = env.get[Int]("newNumber").getOrElse(0)
              log.update(_ :+ n).as(Result(valueCursor(path, env, NumberHolder(n))))
            })
          )
        ),
        ValueObjectMapping[NumberHolder](
          tpe = NumberHolderType,
          fieldMappings = List(ValueField("theNumber", _.theNumber)))
      )

    override val selectElaborator: SelectElaborator =
      SelectElaborator {
        case (MutationType, "changeTheNumber", List(Binding("newNumber", IntValue(n)))) =>
          Elab.env("newNumber" -> n)
      }
  }
}
