// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package introspection

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._

final class IntrospectionSuite extends CatsSuite {
  test("simple type query") {
    val text = """
      {
        __type(name: "User") {
          name
          fields {
            name
            type {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type": {
            "name": "User",
            "fields": [
              {
                "name": "id",
                "type": { "name": "String" }
              },
              {
                "name": "name",
                "type": { "name": "String" }
              },
              {
                "name": "birthday",
                "type": { "name": "Date" }
              }
            ]
          }
        }
      }
    """

    val compiled = IntrospectionQueryCompiler.compile(text)
    val res = IntrospectionQueryInterpreter(SimpleSchema).run(compiled.right.get, SchemaSchema.queryType)
    //println(res)
    assert(res == expected)
  }

  test("introspection query") {
    val text = """
      |query IntrospectionQuery {
      |  __schema {
      |    queryType { name }
      |    mutationType { name }
      |    subscriptionType { name }
      |    types {
      |      ...FullType
      |    }
      |    directives {
      |      name
      |      description
      |      locations
      |      args {
      |        ...InputValue
      |      }
      |    }
      |  }
      |}
      |
      |fragment FullType on __Type {
      |  kind
      |  name
      |  description
      |  fields(includeDeprecated: true) {
      |    name
      |    description
      |    args {
      |      ...InputValue
      |    }
      |    type {
      |      ...TypeRef
      |    }
      |    isDeprecated
      |    deprecationReason
      |  }
      |  inputFields {
      |    ...InputValue
      |  }
      |  interfaces {
      |    ...TypeRef
      |  }
      |  enumValues(includeDeprecated: true) {
      |    name
      |    description
      |    isDeprecated
      |    deprecationReason
      |  }
      |  possibleTypes {
      |    ...TypeRef
      |  }
      |}
      |
      |fragment InputValue on __InputValue {
      |  name
      |  description
      |  type { ...TypeRef }
      |  defaultValue
      |}
      |
      |fragment TypeRef on __Type {
      |  kind
      |  name
      |  ofType {
      |    kind
      |    name
      |    ofType {
      |      kind
      |      name
      |      ofType {
      |        kind
      |        name
      |        ofType {
      |          kind
      |          name
      |          ofType {
      |            kind
      |            name
      |            ofType {
      |              kind
      |              name
      |              ofType {
      |                kind
      |                name
      |              }
      |            }
      |          }
      |        }
      |      }
      |    }
      |  }
      |}
    """.stripMargin.trim

    val res = QueryParser.parseText(text)
    assert(res.isRight)
  }
}

object SimpleSchema extends Schema {
  import ScalarType._

  val types = List(
    ObjectType(
      name = "User",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("birthday", None, Nil, TypeRef("Date"), false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Date",
      description = None,
      fields = List(
        Field("day", None, Nil, IntType, false, None),
        Field("month", None, Nil, IntType, false, None),
        Field("year", None, Nil, IntType, false, None),
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}
