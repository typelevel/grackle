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

import cats.effect.IO
import io.circe.Json
import munit.{CatsEffectSuite, Location, TestOptions}

import grackle._
import grackle.QueryCompiler.SelectElaborator

/**
 * Base class for the GraphQL conformance suites.
 *
 * Each suite covers one subject of the September 2025 specification. Each test case corresponds
 * to one example or one counter-example in the specification text.
 *
 * A Scala triple-quoted string cannot hold a GraphQL block string delimiter. Write `'''` in a
 * document instead. Every helper below replaces that marker with three double quotes.
 *
 * @see
 *   https://spec.graphql.org/September2025/
 */
abstract protected[conformance] class ConformanceSuite extends CatsEffectSuite {
  import ConformanceSuite._

  /**
   * The schema which the query test cases of this suite run against.
   *
   * Override this in a suite which tests queries. A suite which tests documents or schemas only
   * can leave the default in place.
   */
  lazy val defaultSchema: Schema = mkSchema("type Query { placeholder: Boolean }")

  // -- Documents -------------------------------------------------------------------------------

  /**
   * Registers a test case which requires that `doc` parses.
   *
   * Use this for an example which no schema in the specification covers.
   */
  def parses(name: TestOptions)(doc: String)(implicit loc: Location): Unit =
    test(name) {
      val res = graphQLParser.parseText(gql(doc))
      assert(res.hasValue, problems("the document did not parse", res))
    }

  // -- Schemas ---------------------------------------------------------------------------------

  /**
   * Registers a test case which requires that `text` is a valid schema.
   */
  def validSchema(name: TestOptions)(text: String)(implicit loc: Location): Unit =
    test(name) {
      val res = Schema(gql(text))
      assert(res.hasValue, problems("the schema was rejected", res))
    }

  /**
   * Registers a test case which requires that `text` is not a valid schema.
   */
  def invalidSchema(name: TestOptions)(text: String)(implicit loc: Location): Unit =
    test(name) {
      val res = Schema(gql(text))
      assert(!res.hasValue, "the schema was accepted, but the specification forbids it")
    }

  // -- Queries ---------------------------------------------------------------------------------

  /**
   * Registers a test case which requires that every operation in `query` compiles against
   * `schema`.
   *
   * `schema` defaults to [[defaultSchema]]. Supply `vars` when an operation declares a
   * non-nullable variable, because variable coercion runs before the query is complete.
   */
  def validQuery(
      name: TestOptions,
      schema: => Schema = defaultSchema,
      vars: Json = Json.obj()
  )(query: String)(implicit loc: Location): Unit =
    test(name) {
      val res = compileDocument(schema, query, vars)
      assert(res.hasValue, problems("the document was rejected", res))
    }

  /**
   * Registers a test case which requires that `query` does not compile against `schema`.
   *
   * A document with more than one operation is rejected when any one of its operations is
   * rejected. A counter-example which the specification writes as several operations or several
   * fragments therefore needs one test case per operation or per fragment. One test case for
   * the whole block passes while one sub-case fails, which hides the state of every other
   * sub-case.
   */
  def invalidQuery(
      name: TestOptions,
      schema: => Schema = defaultSchema,
      vars: Json = Json.obj()
  )(query: String)(implicit loc: Location): Unit =
    test(name) {
      val res = compileDocument(schema, query, vars)
      assert(!res.hasValue, "the document compiled, but the specification forbids it")
    }

  // -- Responses -------------------------------------------------------------------------------

  /**
   * Registers a test case which requires that `query` yields `expected` when it runs against
   * `mapping`.
   *
   * Use this for an example whose section of the specification also states the response.
   */
  def yields(name: TestOptions, mapping: => Mapping[IO], vars: Json = Json.obj())(
      query: String)(expected: Json)(implicit loc: Location): Unit =
    test(name) {
      assertIO(mapping.compileAndRun(gql(query), untypedVars = Some(vars)), expected)
    }

  /**
   * Registers a test case which requires that the `data` entry of the response holds the
   * response keys `expected`, in that order.
   *
   * Two JSON objects which hold the same entries in a different order are equal, so [[yields]]
   * cannot observe the field order. Section 3.6, Field Ordering, states an order, so the test
   * cases for that subject compare the keys as a list.
   */
  def yieldsFieldOrder(name: TestOptions, mapping: => Mapping[IO], vars: Json = Json.obj())(
      query: String)(expected: List[String])(implicit loc: Location): Unit =
    test(name) {
      val keys =
        mapping
          .compileAndRun(gql(query), untypedVars = Some(vars))
          .map(_.hcursor.downField("data").keys.map(_.toList))
      assertIO(keys, Some(expected))
    }

  /**
   * Compiles every operation of `text` against `schema`.
   */
  private def compileDocument(schema: Schema, text0: String, vars: Json): Result[Operation] = {
    val text = gql(text0)
    val compiler = new QueryCompiler(queryParser, schema, List(SelectElaborator.identity))
    val name = queryParser.parseText(text).toOption.flatMap(_._1.flatMap(_.name).headOption)
    compiler.compile(text, name = name, untypedVars = Some(vars))
  }
}

object ConformanceSuite {
  val graphQLParser: GraphQLParser = GraphQLParser(GraphQLParser.defaultConfig)
  val queryParser: QueryParser = QueryParser(graphQLParser)

  /**
   * Builds a schema from `text`, or throws when `text` is not a valid schema.
   */
  def mkSchema(text: String): Schema =
    Schema(gql(text)) match {
      case Result.Success(s) => s
      case Result.Warning(_, s) => s
      case other => throw new IllegalArgumentException(problems("invalid test schema", other))
    }

  /**
   * Replaces each `'''` marker in `text` with a GraphQL block string delimiter.
   *
   * A Scala triple-quoted string cannot hold a GraphQL block string delimiter, so a test case
   * writes `'''` where the specification writes three double quotes.
   */
  def gql(text: String): String =
    text.replace("'''", "\"\"\"")

  private[conformance] def problems(prefix: String, res: Result[Any]): String =
    res.toProblems.toList match {
      case Nil => prefix
      case ps => ps.mkString(prefix + ": ", "; ", "")
    }
}
