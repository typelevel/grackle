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

import grackle.Query.{Binding, Limit, OrderBy, OrderSelection, OrderSelections}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.{BooleanValue, IntValue}
import grackle.syntax._

trait SqlNullOrderingMapping[F[_]] extends SqlTestMapping[F] {

  object nullOrdering extends TableDef("null_ordering") {
    val id = col("id", int4)
    val v = col("v", nullable(int4))
  }

  val schema =
    schema"""
      type Query {
        items(ascending: Boolean!, nullsLast: Boolean!, limit: Int!): [Item!]!
      }
      type Item {
        id: Int!
        v: Int
      }
    """

  val QueryType = schema.ref("Query")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("items")
        )
      ),
      ObjectMapping(
        tpe = ItemType,
        fieldMappings = List(
          SqlField("id", nullOrdering.id, key = true),
          SqlField("v", nullOrdering.v)
        )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (
          QueryType,
          "items",
          List(
            Binding("ascending", BooleanValue(asc)),
            Binding("nullsLast", BooleanValue(nl)),
            Binding("limit", IntValue(lim)))) =>
      Elab.transformChild(child =>
        Limit(
          lim,
          OrderBy(
            OrderSelections(
              List(
                OrderSelection[Option[Int]](ItemType / "v", ascending = asc, nullsLast = nl),
                // Secondary key so the order within the NULL block is deterministic.
                OrderSelection[Int](ItemType / "id")
              )),
            child
          )
        ).success)
  }
}
