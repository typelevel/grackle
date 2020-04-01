// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import edu.gemini.grackle._

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
        }
        type Collection {
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
          itemById(id: ID!): Item
        }
        type Collection {
          itemIds: [String!]!
          items: [Item!]!
        }
        type Item {
          id: String!
          name: String!
        }
      """
    ).right.get

  case class Collection(itemIds: List[String])
  val collection = Collection(List("A", "B", "C"))

  case class Item(id: String, name: String)
  val items = List(Item("A", "foo"), Item("B", "bar"), Item("C", "baz"))
}

import ComposedListData._

object CollectionQueryCompiler extends QueryCompiler(collectionSchema) {
  val phases = Nil
}

object CollectionQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "collection" => (collectionSchema.ref("Collection"), collection)
  },
  {
    case (c: Collection, "itemIds") => c.itemIds
  }
)

object ItemQueryCompiler extends QueryCompiler(itemSchema) {
  val QueryType = itemSchema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("itemById", List(Binding("id", IDValue(id))), child) =>
        Select("itemById", Nil, Unique(FieldEquals("id", id), child)).rightIor
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
        Select("itemById", Nil, Unique(FieldEquals("id", id), child)).rightIor
    }
  ))

  val collectionItemJoin = (c: Cursor, q: Query) =>
    (c.focus, q) match {
      case (c: Collection, Select("items", _, child)) =>
        GroupList(c.itemIds.map(id => Select("itemById", Nil, Unique(FieldEquals("id", id), child)))).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in collectionItemJoin")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "collection", "CollectionComponent"),
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
              "B",
              "C"
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

  test("composed query") {
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
              "B",
              "C"
            ],
            "items" : [
              {
                "id" : "A",
                "name" : "foo"
              },
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
}
