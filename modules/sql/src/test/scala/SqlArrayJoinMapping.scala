// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import edu.gemini.grackle.syntax._

trait SqlArrayJoinMapping[F[_]] extends SqlTestMapping[F] {

  object root extends TableDef("array_join_root") {
    val id = col("id", varchar)
  }

  object listA extends TableDef("array_join_list_a") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
    val aElem = col("a_elem", nullable(list(varchar)))
  }

  object listB extends TableDef("array_join_list_b") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
    val bElem = col("b_elem", nullable(int4))
  }

  val schema =
    schema"""
      type Query {
        root: [Root!]!
      }
      type Root {
        id: String!
        listA: [ElemA!]!
        listB: [ElemB!]!
      }
      type ElemA {
        id: String!
        elemA: [String!]
      }
      type ElemB {
        id: String!
        elemB: Int
      }
    """

  val QueryType = schema.ref("Query")
  val RootType = schema.ref("Root")
  val ElemAType = schema.ref("ElemA")
  val ElemBType = schema.ref("ElemB")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("root")
          )
      ),
      ObjectMapping(
        tpe = RootType,
        fieldMappings =
          List(
            SqlField("id", root.id, key = true),
            SqlObject("listA", Join(root.id, listA.rootId)),
            SqlObject("listB", Join(root.id, listB.rootId))
          )
      ),
      ObjectMapping(
        tpe = ElemAType,
        fieldMappings =
          List(
            SqlField("id", listA.id, key = true),
            SqlField("rootId", listA.rootId, hidden = true),
            SqlField("elemA", listA.aElem)
          )
      ),
      ObjectMapping(
        tpe = ElemBType,
        fieldMappings =
          List(
            SqlField("id", listB.id, key = true),
            SqlField("rootId", listB.rootId, hidden = true),
            SqlField("elemB", listB.bElem)
          )
      )
    )
}
