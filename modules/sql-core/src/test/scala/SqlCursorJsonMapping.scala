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

package grackle.sql.test

import cats.implicits._
import io.circe.Json
import io.circe.syntax.EncoderOps

import grackle._
import syntax._
import Query._
import Predicate._
import Value._
import QueryCompiler._

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
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "brands", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(BrandType / "id", Const(id)), child)))
  }
}
