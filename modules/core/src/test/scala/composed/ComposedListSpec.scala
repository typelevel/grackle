// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Predicate._, Value._
import QueryCompiler._, ComponentElaborator.Mapping
import QueryInterpreter.mkErrorResult

object ComposedListData {
  val collectionSchema =
    Schema(
      """
        type Query {
          collection: Collection!
          collections: [Collection!]!
          collectionByName(name: String!): Collection
        }
        type Collection {
          name: String!
          itemIds: [String!]!
        }
      """
    ).right.get

  val itemSchema =
    Schema(
      """
        type Query {
          itemById(id: ID!): Item
        }
        type Item {
          id: String!
          name: String!
        }
      """
    ).right.get

  val composedSchema =
    Schema(
      """
        type Query {
          collection: Collection!
          collections: [Collection!]!
          collectionByName(name: String!): Collection
          itemById(id: ID!): Item
        }
        type Collection {
          name: String!
          itemIds: [String!]!
          items: [Item!]!
        }
        type Item {
          id: String!
          name: String!
        }
      """
    ).right.get

  case class Collection(name: String, itemIds: List[String])
  val collections =
    List(
      Collection("AB", List("A", "B")),
      Collection("BC", List("B", "C"))
    )

  case class Item(id: String, name: String)
  val items = List(Item("A", "foo"), Item("B", "bar"), Item("C", "baz"))
}

import ComposedListData._

object CollectionQueryCompiler extends QueryCompiler(collectionSchema) {
  val QueryType = collectionSchema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("collectionByName", List(Binding("name", StringValue(name))), child) =>
        Select("collectionByName", Nil, Unique(Eql(FieldPath(List("name")), Const(name)), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object CollectionQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "collection" => (collectionSchema.ref("Collection"), collections.head)
    case "collections" => (ListType(collectionSchema.ref("Collection")), collections)
    case "collectionByName" => (ListType(collectionSchema.ref("Collection")), collections)
  },
  {
    case (c: Collection, "name") => c.name
    case (c: Collection, "itemIds") => c.itemIds
  }
)

object ItemQueryCompiler extends QueryCompiler(itemSchema) {
  val QueryType = itemSchema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("itemById", List(Binding("id", IDValue(id))), child) =>
        Select("itemById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object ItemQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "itemById" => (ListType(itemSchema.ref("Item")), items)
  },
  {
    case (i: Item, "id")   => i.id
    case (i: Item, "name") => i.name
  }
)

object ComposedListQueryCompiler extends QueryCompiler(composedSchema) {
  val QueryType = composedSchema.ref("Query")
  val CollectionType = composedSchema.ref("Collection")

  val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("itemById", List(Binding("id", IDValue(id))), child) =>
        Select("itemById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
      case Select("collectionByName", List(Binding("name", StringValue(name))), child) =>
        Select("collectionByName", Nil, Unique(Eql(FieldPath(List("name")), Const(name)), child)).rightIor
    }
  ))

  val collectionItemJoin = (c: Cursor, q: Query) =>
    (c.focus, q) match {
      case (c: Collection, Select("items", _, child)) =>
        GroupList(c.itemIds.map(id => Select("itemById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)))).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in collectionItemJoin")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "collection", "CollectionComponent"),
    Mapping(QueryType, "collections", "CollectionComponent"),
    Mapping(QueryType, "collectionByName", "CollectionComponent"),
    Mapping(QueryType, "itemById", "ItemComponent"),
    Mapping(CollectionType, "items", "ItemComponent", collectionItemJoin)
  )

  val phases = List(componentElaborator, selectElaborator)
}

object ComposedListQueryInterpreter extends
  ComposedQueryInterpreter[Id](Map(
    "CollectionComponent" -> CollectionQueryInterpreter,
    "ItemComponent"       -> ItemQueryInterpreter
  ))

final class ComposedListSpec extends CatsSuite {
  test("simple outer query") {
    val query = """
      query {
        collection {
          itemIds
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collection" : {
            "itemIds" : [
              "A",
              "B"
            ]
          }
        }
      }
    """

    val compiledQuery = CollectionQueryCompiler.compile(query).right.get
    val res = CollectionQueryInterpreter.run(compiledQuery, collectionSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("simple inner query") {
    val query = """
      query {
        itemById(id: "A") {
          id
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "itemById" : {
            "id" : "A",
            "name" : "foo"
          }
        }
      }
    """

    val compiledQuery = ItemQueryCompiler.compile(query).right.get
    val res = ItemQueryInterpreter.run(compiledQuery, itemSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("composed query (1)") {
    val query = """
      query {
        collection {
          itemIds
          items {
            id
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collection" : {
            "itemIds" : [
              "A",
              "B"
            ],
            "items" : [
              {
                "id" : "A",
                "name" : "foo"
              },
              {
                "id" : "B",
                "name" : "bar"
              }
            ]
          }
        }
      }
    """

    val compiledQuery = ComposedListQueryCompiler.compile(query).right.get
    val res = ComposedListQueryInterpreter.run(compiledQuery, composedSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("composed query (2)") {
    val query = """
      query {
        collectionByName(name: "BC") {
          items {
            id
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collectionByName" : {
            "items" : [
              {
                "id" : "B",
                "name" : "bar"
              },
              {
                "id" : "C",
                "name" : "baz"
              }
            ]
          }
        }
      }
    """

    val compiledQuery = ComposedListQueryCompiler.compile(query).right.get
    val res = ComposedListQueryInterpreter.run(compiledQuery, composedSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("composed query (3)") {
    val query = """
      query {
        collections {
          name
          items {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collections" : [
            {
              "name" : "AB",
              "items" : [
                {
                  "id" : "A"
                },
                {
                  "id" : "B"
                }
              ]
            },
            {
              "name" : "BC",
              "items" : [
                {
                  "id" : "B"
                },
                {
                  "id" : "C"
                }
              ]
            }
          ]
        }
      }
    """

    val compiledQuery = ComposedListQueryCompiler.compile(query).right.get
    val res = ComposedListQueryInterpreter.run(compiledQuery, composedSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("composed query (4)") {
    val query = """
      query {
        collections {
          items {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collections" : [
            {
              "items" : [
                {
                  "id" : "A"
                },
                {
                  "id" : "B"
                }
              ]
            },
            {
              "items" : [
                {
                  "id" : "B"
                },
                {
                  "id" : "C"
                }
              ]
            }
          ]
        }
      }
    """

    val compiledQuery = ComposedListQueryCompiler.compile(query).right.get
    val res = ComposedListQueryInterpreter.run(compiledQuery, composedSchema.queryType)
    //println(res)

    assert(res == expected)
  }
}
