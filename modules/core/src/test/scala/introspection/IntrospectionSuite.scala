// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package introspection

import cats.effect.IO
import io.circe.{ ACursor, Json }
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import QueryCompiler.IntrospectionLevel
import IntrospectionLevel._

final class IntrospectionSuite extends CatsEffectSuite {
  def standardTypeName(name: String): Boolean = name match {
    case "Int" | "Float" | "Boolean" | "String" | "ID" => true
    case _ => name.startsWith("__")
  }

  def standardDirectiveName(name: String): Boolean = name match {
    case "skip" | "include" | "deprecated" => true
    case _ => name.startsWith("__")
  }

  implicit class ACursorOps(self: ACursor) {
    def filterArray(f: Json => Boolean): ACursor = {
      def go(ac: ACursor, i: Int = 0): ACursor = {
        val acʹ = ac.downN(i)
        acʹ.focus match {
          case None    => ac
          case Some(j) => if (f(j)) go(ac, i + 1) else go(acʹ.delete, i)
        }
      }
      go(self)
    }
  }

  def stripStandardElements(result: Json): Option[Json] =
    result
      .hcursor
      .downField("data")
      .downField("__schema")
      .downField("types")
      .filterArray(_.hcursor.downField("name").as[String].exists(!standardTypeName(_)))
      .up
      .downField("directives")
      .filterArray(_.hcursor.downField("name").as[String].exists(!standardDirectiveName(_)))
      .top // .root doesn't work in 0.13

  test("simple type query") {
    val query = """
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

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("kind/name/description/ofType query") {
    val query = """
      {
        __type(name: "KindTest") {
          name
          description
          fields {
            name
            description
            type {
              kind
              name
              description
              ofType {
                kind
                name
                description
                ofType {
                  kind
                  name
                  description
                  ofType {
                    kind
                    name
                    description
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "name" : "KindTest",
            "description" : null,
            "fields" : [
              {
                "name" : "scalar",
                "description" : "Scalar field",
                "type" : {
                  "kind" : "SCALAR",
                  "name" : "Int",
                  "description" : "The Int scalar type represents a signed 32‐bit numeric non‐fractional value.\nResponse formats that support a 32‐bit integer or a number type should use that\ntype to represent this scalar.",
                  "ofType" : null
                }
              },
              {
                "name" : "object",
                "description" : "Object field",
                "type" : {
                  "kind" : "OBJECT",
                  "name" : "User",
                  "description" : "User object type",
                  "ofType" : null
                }
              },
              {
                "name" : "interface",
                "description" : "Interface field",
                "type" : {
                  "kind" : "INTERFACE",
                  "name" : "Profile",
                  "description" : "Profile interface type",
                  "ofType" : null
                }
              },
              {
                "name" : "union",
                "description" : "Union field",
                "type" : {
                  "kind" : "UNION",
                  "name" : "Choice",
                  "description" : "Choice union type",
                  "ofType" : null
                }
              },
              {
                "name" : "enum",
                "description" : "Enum field",
                "type" : {
                  "kind" : "ENUM",
                  "name" : "Flags",
                  "description" : "Flags enum type",
                  "ofType" : null
                }
              },
              {
                "name" : "list",
                "description" : "List field",
                "type" : {
                  "kind" : "LIST",
                  "name" : null,
                  "description" : null,
                  "ofType" : {
                    "kind" : "SCALAR",
                    "name" : "Int",
                    "description" : "The Int scalar type represents a signed 32‐bit numeric non‐fractional value.\nResponse formats that support a 32‐bit integer or a number type should use that\ntype to represent this scalar.",
                    "ofType" : null
                  }
                }
              },
              {
                "name" : "nonnull",
                "description" : "Nonnull field",
                "type" : {
                  "kind" : "NON_NULL",
                  "name" : null,
                  "description" : null,
                  "ofType" : {
                    "kind" : "SCALAR",
                    "name" : "Int",
                    "description" : "The Int scalar type represents a signed 32‐bit numeric non‐fractional value.\nResponse formats that support a 32‐bit integer or a number type should use that\ntype to represent this scalar.",
                    "ofType" : null
                  }
                }
              },
              {
                "name" : "nonnulllistnonnull",
                "description" : "Nonnull list of nonnull field",
                "type" : {
                  "kind" : "NON_NULL",
                  "name" : null,
                  "description" : null,
                  "ofType" : {
                    "kind" : "LIST",
                    "name" : null,
                    "description" : null,
                    "ofType" : {
                      "kind" : "NON_NULL",
                      "name" : null,
                      "description" : null,
                      "ofType" : {
                        "kind" : "SCALAR",
                        "name" : "Int",
                        "description" : "The Int scalar type represents a signed 32‐bit numeric non‐fractional value.\nResponse formats that support a 32‐bit integer or a number type should use that\ntype to represent this scalar."
                      }
                    }
                  }
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fields query") {
    val query = """
      {
        __type(name: "KindTest") {
          name
          fields {
            name
            type {
              name
              fields {
                name
                type {
                  name
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "name" : "KindTest",
            "fields" : [
              {
                "name" : "scalar",
                "type" : {
                  "name" : "Int",
                  "fields" : null
                }
              },
              {
                "name" : "object",
                "type" : {
                  "name" : "User",
                  "fields" : [
                    {
                      "name" : "id",
                      "type" : {
                        "name" : "String"
                      }
                    },
                    {
                      "name" : "name",
                      "type" : {
                        "name" : "String"
                      }
                    },
                    {
                      "name" : "birthday",
                      "type" : {
                        "name" : "Date"
                      }
                    }
                  ]
                }
              },
              {
                "name" : "interface",
                "type" : {
                  "name" : "Profile",
                  "fields" : [
                    {
                      "name" : "id",
                      "type" : {
                        "name" : "String"
                      }
                    }
                  ]
                }
              },
              {
                "name" : "union",
                "type" : {
                  "name" : "Choice",
                  "fields" : null
                }
              },
              {
                "name" : "enum",
                "type" : {
                  "name" : "Flags",
                  "fields" : null
                }
              },
              {
                "name" : "list",
                "type" : {
                  "name" : null,
                  "fields" : null
                }
              },
              {
                "name" : "nonnull",
                "type" : {
                  "name" : null,
                  "fields" : null
                }
              },
              {
                "name" : "nonnulllistnonnull",
                "type" : {
                  "name" : null,
                  "fields" : null
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interfaces query") {
    val query = """
      {
        __type(name: "KindTest") {
          name
          fields {
            name
            type {
              interfaces {
                name
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "name" : "KindTest",
            "fields" : [
              {
                "name" : "scalar",
                "type" : {
                  "interfaces" : null
                }
              },
              {
                "name" : "object",
                "type" : {
                  "interfaces" : [
                    {
                      "name" : "Profile"
                    }
                  ]
                }
              },
              {
                "name" : "interface",
                "type" : {
                  "interfaces" : null
                }
              },
              {
                "name" : "union",
                "type" : {
                  "interfaces" : null
                }
              },
              {
                "name" : "enum",
                "type" : {
                  "interfaces" : null
                }
              },
              {
                "name" : "list",
                "type" : {
                  "interfaces" : null
                }
              },
              {
                "name" : "nonnull",
                "type" : {
                  "interfaces" : null
                }
              },
              {
                "name" : "nonnulllistnonnull",
                "type" : {
                  "interfaces" : null
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("possibleTypes query") {
    val query = """
      {
        __type(name: "KindTest") {
          name
          fields {
            name
            type {
              possibleTypes {
                name
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "name" : "KindTest",
            "fields" : [
              {
                "name" : "scalar",
                "type" : {
                  "possibleTypes" : null
                }
              },
              {
                "name" : "object",
                "type" : {
                  "possibleTypes" : null
                }
              },
              {
                "name" : "interface",
                "type" : {
                  "possibleTypes" : [
                    {
                      "name" : "User"
                    }
                  ]
                }
              },
              {
                "name" : "union",
                "type" : {
                  "possibleTypes" : [
                    {
                      "name" : "User"
                    },
                    {
                      "name" : "Date"
                    }
                  ]
                }
              },
              {
                "name" : "enum",
                "type" : {
                  "possibleTypes" : null
                }
              },
              {
                "name" : "list",
                "type" : {
                  "possibleTypes" : null
                }
              },
              {
                "name" : "nonnull",
                "type" : {
                  "possibleTypes" : null
                }
              },
              {
                "name" : "nonnulllistnonnull",
                "type" : {
                  "possibleTypes" : null
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("enumValues query") {
    val query = """
      {
        __type(name: "KindTest") {
          name
          fields {
            name
            type {
              enumValues {
                name
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "name" : "KindTest",
            "fields" : [
              {
                "name" : "scalar",
                "type" : {
                  "enumValues" : null
                }
              },
              {
                "name" : "object",
                "type" : {
                  "enumValues" : null
                }
              },
              {
                "name" : "interface",
                "type" : {
                  "enumValues" : null
                }
              },
              {
                "name" : "union",
                "type" : {
                  "enumValues" : null
                }
              },
              {
                "name" : "enum",
                "type" : {
                  "enumValues" : [
                    {
                      "name" : "A"
                    },
                    {
                      "name" : "C"
                    }
                  ]
                }
              },
              {
                "name" : "list",
                "type" : {
                  "enumValues" : null
                }
              },
              {
                "name" : "nonnull",
                "type" : {
                  "enumValues" : null
                }
              },
              {
                "name" : "nonnulllistnonnull",
                "type" : {
                  "enumValues" : null
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("deprecation excluded") {
    val query = """
      {
        __type(name: "DeprecationTest") {
          fields {
            name
            type {
              fields(includeDeprecated: false) {
                name
                isDeprecated
                deprecationReason
              }
              enumValues(includeDeprecated: false) {
                name
                isDeprecated
                deprecationReason
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "fields" : [
              {
                "name" : "user",
                "type" : {
                  "fields" : [
                    {
                      "name" : "id",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    },
                    {
                      "name" : "name",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    },
                    {
                      "name" : "birthday",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    }
                  ],
                  "enumValues" : null
                }
              },
              {
                "name" : "flags",
                "type" : {
                  "fields" : null,
                  "enumValues" : [
                    {
                      "name" : "A",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    },
                    {
                      "name" : "C",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    }
                  ]
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("deprecation included") {
    val query = """
      {
        __type(name: "DeprecationTest") {
          fields {
            name
            type {
              fields(includeDeprecated: true) {
                name
                isDeprecated
                deprecationReason
              }
              enumValues(includeDeprecated: true) {
                name
                isDeprecated
                deprecationReason
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__type" : {
            "fields" : [
              {
                "name" : "user",
                "type" : {
                  "fields" : [
                    {
                      "name" : "id",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    },
                    {
                      "name" : "name",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    },
                    {
                      "name" : "age",
                      "isDeprecated" : true,
                      "deprecationReason" : "Use birthday instead"
                    },
                    {
                      "name" : "birthday",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    }
                  ],
                  "enumValues" : null
                }
              },
              {
                "name" : "flags",
                "type" : {
                  "fields" : null,
                  "enumValues" : [
                    {
                      "name" : "A",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    },
                    {
                      "name" : "B",
                      "isDeprecated" : true,
                      "deprecationReason" : "Deprecation test"
                    },
                    {
                      "name" : "C",
                      "isDeprecated" : false,
                      "deprecationReason" : null
                    }
                  ]
                }
              }
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("simple schema query") {
    val query = """
      {
        __schema {
          queryType { name }
          mutationType { name }
          subscriptionType { name }
          types { name }
          directives { name }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "__schema" : {
            "queryType" : {
              "name" : "Query"
            },
            "mutationType" : null,
            "subscriptionType" : null,
            "types" : [
              {
                "name" : "Query"
              },
              {
                "name" : "User"
              },
              {
                "name" : "Profile"
              },
              {
                "name" : "Date"
              },
              {
                "name" : "Choice"
              },
              {
                "name" : "Flags"
              },
              {
                "name" : "KindTest"
              },
              {
                "name" : "DeprecationTest"
              }
            ],
            "directives" : [
            ]
          }
        }
      }
    """

    val res = TestMapping.compileAndRun(query).map(stripStandardElements)

    assertIO(res, Some(expected))
  }

  test("standard introspection query") {
    val query = """
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
      |      isRepeatable
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

    val expected = json"""
      {
        "data" : {
          "__schema" : {
            "queryType" : {
              "name" : "Query"
            },
            "mutationType" : {
              "name" : "Mutation"
            },
            "subscriptionType" : {
              "name" : "Subscription"
            },
            "types" : [
              {
                "kind" : "OBJECT",
                "name" : "Query",
                "description" : null,
                "fields" : [
                  {
                    "name" : "users",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "NON_NULL",
                      "name" : null,
                      "ofType" : {
                        "kind" : "LIST",
                        "name" : null,
                        "ofType" : {
                          "kind" : "NON_NULL",
                          "name" : null,
                          "ofType" : {
                            "kind" : "OBJECT",
                            "name" : "User",
                            "ofType" : null
                          }
                        }
                      }
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  },
                  {
                    "name" : "profiles",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "NON_NULL",
                      "name" : null,
                      "ofType" : {
                        "kind" : "LIST",
                        "name" : null,
                        "ofType" : {
                          "kind" : "NON_NULL",
                          "name" : null,
                          "ofType" : {
                            "kind" : "INTERFACE",
                            "name" : "Profile",
                            "ofType" : null
                          }
                        }
                      }
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  }
                ],
                "inputFields" : null,
                "interfaces" : [
                ],
                "enumValues" : null,
                "possibleTypes" : null
              },
              {
                "kind" : "OBJECT",
                "name" : "Subscription",
                "description" : null,
                "fields" : [
                  {
                    "name" : "dummy",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "NON_NULL",
                      "name" : null,
                      "ofType" : {
                        "kind" : "SCALAR",
                        "name" : "Int",
                        "ofType" : null
                      }
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  }
                ],
                "inputFields" : null,
                "interfaces" : [
                ],
                "enumValues" : null,
                "possibleTypes" : null
              },
              {
                "kind" : "OBJECT",
                "name" : "Mutation",
                "description" : null,
                "fields" : [
                  {
                    "name" : "dummy",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "NON_NULL",
                      "name" : null,
                      "ofType" : {
                        "kind" : "SCALAR",
                        "name" : "Int",
                        "ofType" : null
                      }
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  }
                ],
                "inputFields" : null,
                "interfaces" : [
                ],
                "enumValues" : null,
                "possibleTypes" : null
              },
              {
                "kind" : "INTERFACE",
                "name" : "Profile",
                "description" : "Profile interface type",
                "fields" : [
                  {
                    "name" : "id",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "SCALAR",
                      "name" : "String",
                      "ofType" : null
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  }
                ],
                "inputFields" : null,
                "interfaces" : null,
                "enumValues" : null,
                "possibleTypes" : [
                  {
                    "kind" : "OBJECT",
                    "name" : "User",
                    "ofType" : null
                  }
                ]
              },
              {
                "kind" : "OBJECT",
                "name" : "User",
                "description" : "User object type",
                "fields" : [
                  {
                    "name" : "id",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "SCALAR",
                      "name" : "String",
                      "ofType" : null
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  },
                  {
                    "name" : "name",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "SCALAR",
                      "name" : "String",
                      "ofType" : null
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  },
                  {
                    "name" : "age",
                    "description" : null,
                    "args" : [
                    ],
                    "type" : {
                      "kind" : "SCALAR",
                      "name" : "Int",
                      "ofType" : null
                    },
                    "isDeprecated" : false,
                    "deprecationReason" : null
                  }
                ],
                "inputFields" : null,
                "interfaces" : [
                  {
                    "kind" : "INTERFACE",
                    "name" : "Profile",
                    "ofType" : null
                  }
                ],
                "enumValues" : null,
                "possibleTypes" : null
              }
            ],
            "directives" : [
            ]
          }
        }
      }
    """

    val res = SmallMapping.compileAndRun(query).map(stripStandardElements)

    assertIO(res, Some(expected))
  }

  test("typename query") {
    val query = """
      {
        users {
          __typename
          renamed: __typename
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "users" : [
            {
              "__typename" : "User",
              "renamed" : "User",
              "name" : "Luke Skywalker"
            }
          ]
        }
      }
    """

    val res = SmallMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("typename query with narrowing") {
    val query = """
      {
        profiles {
          __typename
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "profiles" : [
            {
              "__typename" : "User",
              "id" : "1000"
            }
          ]
        }
      }
    """

    val res = SmallMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("mixed query") {
    val query = """
      {
        users {
          name
        }
        __type(name: "User") {
          name
          fields {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "users" : [
            {
              "name" : "Luke Skywalker"
            }
          ],
          "__type" : {
            "name": "User",
            "fields" : [
              {
                "name" : "id"
              },
              {
                "name" : "name"
              },
              {
                "name" : "age"
              }
            ]
          }
        }
      }
    """

    val res = SmallMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("introspection disabled") {
    val query = """
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
        "errors" : [
          {
            "message" : "No field '__type' for type Query"
          }
        ]
      }
    """

    val res = TestMapping.compileAndRun(query, introspectionLevel = Disabled)

    assertIO(res, expected)
  }

  test("introspection disabled typename allowed") {
    val query = """
      {
        users {
          __typename
          renamed: __typename
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "users" : [
            {
              "__typename" : "User",
              "renamed" : "User",
              "name" : "Luke Skywalker"
            }
          ]
        }
      }
    """

    val res = SmallMapping.compileAndRun(query, introspectionLevel = TypenameOnly)

    assertIO(res, expected)
  }
}

object TestMapping extends compiler.TestMapping {
  val schema =
    schema"""
      type Query {
        users: [User!]!
      }

      "User object type"
      type User implements Profile {
        id: String
        name: String
        age: Int @deprecated(reason: "Use birthday instead")
        birthday: Date
      }

      "Profile interface type"
      interface Profile {
        id: String
      }

      "Date object type"
      type Date {
        day: Int
        month: Int
        year: Int
      }

      "Choice union type"
      union Choice = User | Date

      "Flags enum type"
      enum Flags {
        A
        B @deprecated(reason: "Deprecation test")
        C
      }

      type KindTest {
        "Scalar field"
        scalar: Int
        "Object field"
        object: User
        "Interface field"
        interface: Profile
        "Union field"
        union: Choice
        "Enum field"
        enum: Flags
        "List field"
        list: [Int]
        "Nonnull field"
        nonnull: Int!
        "Nonnull list of nonnull field"
        nonnulllistnonnull: [Int!]!
      }

      type DeprecationTest {
        user: User
        flags: Flags
      }
    """
}

object SmallData {
  trait Profile {
    def id: Option[String]
  }
  case class User(id: Option[String], name: Option[String], age: Option[Int]) extends Profile
  val users = List(
    User(Some("1000"), Some("Luke Skywalker"), Some(30))
  )
}

object SmallMapping extends ValueMapping[IO] {
  import SmallData._

  val schema =
    schema"""
      type Query {
        users: [User!]!
        profiles: [Profile!]!
      }
      type Subscription {
        dummy: Int!
      }
      type Mutation {
        dummy: Int!
      }
      "Profile interface type"
      interface Profile {
        id: String
      }
      "User object type"
      type User implements Profile {
        id: String
        name: String
        age: Int
      }
    """

  val QueryType = schema.queryType
  val UserType = schema.ref("User")
  val ProfileType = schema.ref("Profile")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("users", _ => users),
            ValueField("profiles", _ => users)
          )
      ),
      ValueObjectMapping[User](
        tpe = UserType,
        fieldMappings =
          List(
            ValueField("name", _.name),
            ValueField("age", _.age)
          )
      ),
      ValueObjectMapping[Profile](
        tpe = ProfileType,
        fieldMappings =
          List(
            ValueField("id", _.id)
          )
      )
  )
}
