// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.implicits._
import io.circe.Json
import edu.gemini.grackle._
import syntax._
import Query._
import Predicate._
import Value._
import QueryCompiler._
import io.circe.syntax.EncoderOps

trait SqlCursorJsonMapping[F[_]] extends SqlTestMapping[F] {

  object brands extends TableDef("brands") {
    val id = col("id", int4)
    val category = col("categories", int4)
  }

  trait Category {
    val name: String
  }
  object Category {
    def decodeCategoryInt(i: Int): List[Category] =
      i match {
        case 8 => List(Film)
        case 16 => List(Drama)
      }
    implicit val categoryEncoder: io.circe.Encoder[Category] =
      io.circe.Encoder.instance {
        case Film => Json.obj(("name", Json.fromString("Film")))
        case Drama => Json.obj(("name", Json.fromString("Drama")))
        case _ => Json.Null
      }
    object Film extends Category {
      override val name: String = "Film"
    }
    object Drama extends Category {
      override val name: String = "Drama"
    }
  }

  val schema =
    schema"""
      type Query {
        brands(id: Int!): Brand
      }
      type Brand {
        id: Int!
        categories: [Category!]
      }
      type Category {
        name: String
      }
    """

  val QueryType = schema.ref("Query")
  val BrandType = schema.ref("Brand")
  val CategoryType = schema.ref("Category")

  def decodeCategories(c: Cursor): Result[Json] = {
    for {
      enc <- c.fieldAs[Int]("encodedCategories")
      res = Category.decodeCategoryInt(enc)
    } yield res.asJson
  }

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("brands")
          )
      ),
      ObjectMapping(
        tpe = BrandType,
        fieldMappings =
          List(
            SqlField("id", brands.id, key = true),
            SqlField("encodedCategories", brands.category, hidden = true),
            CursorFieldJson("categories", decodeCategories, List("encodedCategories"))
          )
      ),
      PrimitiveMapping(CategoryType)
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("brands", List(Binding("id", IntValue(id))), child) =>
        Select("brands", Nil, Unique(Filter(Eql(BrandType / "id", Const(id)), child))).rightIor
    }
  ))
}
