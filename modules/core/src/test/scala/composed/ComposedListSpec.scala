// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.mkErrorResult

object CollectionData {
  case class Collection(name: String, itemIds: List[String])
  val collections =
    List(
      Collection("AB", List("A", "B")),
      Collection("BC", List("B", "C"))
    )
}

object CollectionMapping extends ValueMapping[Id] {
  import CollectionData._

  val schema =
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

  val QueryType = schema.ref("Query")
  val CollectionType = schema.ref("Collection")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("collection", collections.head),
            ValueRoot("collections", collections),
            ValueRoot("collectionByName", collections)
          )
      ),
      ValueObjectMapping[Collection](
        tpe = CollectionType,
        fieldMappings =
          List(
            ValueField("name", _.name),
            ValueField("itemIds", _.itemIds)
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("collectionByName", List(Binding("name", StringValue(name))), child) =>
        Select("collectionByName", Nil, Unique(Eql(FieldPath(List("name")), Const(name)), child)).rightIor
    }
  ))
}

object ItemData {
  case class Item(id: String, name: String)
  val items = List(Item("A", "foo"), Item("B", "bar"), Item("C", "baz"))
}

object ItemMapping extends ValueMapping[Id] {
  import ItemData._

  val schema =
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

  val QueryType = schema.ref("Query")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("itemById", items)
          )
      ),
      ValueObjectMapping[Item](
        tpe = ItemType,
        fieldMappings =
          List(
            ValueField("id", _.id),
            ValueField("name", _.name)
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("itemById", List(Binding("id", IDValue(id))), child) =>
        Select("itemById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))
}

object ComposedListMapping extends Mapping[Id] {
  val schema =
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

  val QueryType = schema.ref("Query")
  val CollectionType = schema.ref("Collection")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("collection", CollectionMapping),
            Delegate("collections", CollectionMapping),
            Delegate("collectionByName", CollectionMapping),
            Delegate("itemById", ItemMapping)
          )
      ),
      ObjectMapping(
        tpe = CollectionType,
        fieldMappings =
          List(
            Delegate("items", ItemMapping, collectionItemJoin)
          )
      )
  )

  override val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("itemById", List(Binding("id", IDValue(id))), child) =>
        Select("itemById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
      case Select("collectionByName", List(Binding("name", StringValue(name))), child) =>
        Select("collectionByName", Nil, Unique(Eql(FieldPath(List("name")), Const(name)), child)).rightIor
    }
  ))

  def collectionItemJoin(c: Cursor, q: Query): Result[Query] =
    (c.focus, q) match {
      case (c: CollectionData.Collection, Select("items", _, child)) =>
        GroupList(c.itemIds.map(id => Select("itemById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)))).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in collectionItemJoin")
    }
}

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

    val res = CollectionMapping.compileAndRun(query)
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

    val res = ItemMapping.compileAndRun(query)
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

    val res = ComposedListMapping.compileAndRun(query)
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

    val res = ComposedListMapping.compileAndRun(query)
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

    val res = ComposedListMapping.compileAndRun(query)
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

    val res = ComposedListMapping.compileAndRun(query)
    //println(res)

    assert(res == expected)
  }
}
