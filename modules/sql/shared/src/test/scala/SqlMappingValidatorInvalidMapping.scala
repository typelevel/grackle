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

package grackle.sql.test

import grackle._
import grackle.syntax._

trait SqlMappingValidatorInvalidMapping[F[_]] extends SqlTestMapping[F] {
  object scalars extends TableDef("scalars") {
    val boolCol = col("boolCol", bool)
    val intCol = col("intCol", int4)
    val nullableBoolCol = col("nullableBoolCol", nullable(bool))
    val nullableIntCol = col("nullableIntCol", nullable(int4))

    val stringsCol = col("stringsCol", list(text))
    val nullableStringsCol = col("nullableStringsCol", nullable(list(text)))
  }

  object obj1 extends TableDef("obj1") {
    val idCol = col("idCol", int4)
    val typeCol = col("typeCol", int4)
    val intCol = col("intCol", int4)
  }

  object obj2 extends TableDef("obj2") {
    val idCol = col("idCol", int4)
    val boolCol = col("boolCol", bool)
    val stringCol = col("stringCol", text)
  }

  object join extends TableDef("join") {
    val parentIdCol = col("parentIdCol", int4)
    val childIdCol = col("childIdCol", int4)
  }

  object subObj1 extends TableDef("subObj1") {
    val idCol = col("idCol", int4)
  }

  object subObj2 extends TableDef("subObj2") {
    val idCol = col("idCol", int4)
    val parentIdCol = col("parentIdCol", int4)
    val parentBoolCol = col("parentBoolCol", int4)
  }

  object subObj3 extends TableDef("subObj3") {
    val idCol = col("idCol", int4)
  }

  val schema =
    schema"""
      type Query {
        scalars: Scalars
        objs: [Intrf]
        union: Union
      }

      type Scalars {
        boolField1: Boolean!
        boolField2: Boolean!
        nullableBoolField1: Boolean
        nullableBoolField2: Boolean

        stringsField: [String]!
        nullableStringsField: [String]

        jsonbField: Record
        nullableJsonbField: Record!
      }

      type Record {
        foo: String
      }

      interface Intrf {
        id: Int!
      }

      type Obj1 implements Intrf {
        id: Int!
        intField: Int!
        embedded: Obj3!
        sub1: SubObj1
      }

      type Obj2 implements Intrf {
        id: Int!
        boolField: Boolean!
        sub2: SubObj2
      }

      type Obj3 {
        stringField: String!
        sub3: SubObj3
      }

      type SubObj1 {
        id: Int!
      }

      type SubObj2 {
        id: Int!
      }

      type SubObj3 {
        id: Int!
      }

      union Union = Obj1 | Obj2
    """
  val QueryType = schema.ref("Query")
  val ScalarsType = schema.ref("Scalars")
  val IntrfType = schema.ref("Intrf")
  val Obj1Type = schema.ref("Obj1")
  val Obj2Type = schema.ref("Obj2")
  val Obj3Type = schema.ref("Obj3")
  val UnionType = schema.ref("Union")
  val SubObj1Type = schema.ref("SubObj1")
  val SubObj2Type = schema.ref("SubObj2")
  val SubObj3Type = schema.ref("SubObj3")

  override val typeMappings =
    TypeMappings.unsafe(
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings =
            List(
              SqlObject("scalars"),
              SqlObject("objs"),
              SqlObject("union")
            )
        ),
        ObjectMapping(
          tpe = ScalarsType,
          fieldMappings =
            List(
              SqlField("boolField1", scalars.nullableBoolCol),
              SqlField("boolField2", scalars.intCol),
              SqlField("nullableBoolField1", scalars.boolCol),
              SqlField("nullableBoolField2", scalars.nullableIntCol),

              SqlField("stringsField", scalars.nullableStringsCol),
              SqlField("nullableStringsField", scalars.stringsCol),

              SqlJson("jsonbField", scalars.boolCol),
              SqlJson("nullableJsonbField", scalars.nullableBoolCol)
            )
        ),
        SqlInterfaceMapping(
          tpe = IntrfType,
          discriminator = objectTypeDiscriminator,
          fieldMappings =
            List(
              SqlField("id", obj1.idCol),
              SqlField("typeField", obj1.typeCol, hidden = true)
            )
        ),
        ObjectMapping(
          tpe = Obj1Type,
          fieldMappings =
            List(
              SqlField("id", obj1.idCol),
              SqlField("intField", obj1.intCol),
              SqlObject("embedded"),
              SqlObject("sub1", Join(obj1.idCol, join.parentIdCol), Join(join.childIdCol, obj1.idCol))
            )
        ),
        ObjectMapping(
          tpe = Obj2Type,
          fieldMappings =
            List(
              SqlField("id", obj2.idCol),
              SqlField("assoc", obj1.idCol, associative = true, hidden = true),
              SqlField("boolField", obj2.boolCol),
              SqlObject("sub2", Join(List((obj2.idCol, subObj2.idCol), (obj2.boolCol, subObj3.idCol))))
            )
        ),
        ObjectMapping(
          tpe = Obj3Type,
          fieldMappings =
            List(
              SqlField("id", obj2.idCol, key = true, hidden = true),
              SqlField("stringField", obj2.stringCol),
              SqlObject("sub3", Join(Nil))
            )
        ),
        SqlUnionMapping(
          tpe = UnionType,
          discriminator = objectTypeDiscriminator,
          fieldMappings =
            List(
              SqlField("id", obj1.idCol),
              SqlField("typeField", obj1.typeCol, hidden = true),
              SqlObject("bogus", Nil)
            )
        ),
        ObjectMapping(
          tpe = SubObj1Type,
          fieldMappings =
            List(
              SqlField("id", subObj1.idCol, key = true)
            )
        ),
        ObjectMapping(
          tpe = SubObj2Type,
          fieldMappings =
            List(
              SqlField("id", subObj2.idCol, key = true)
            )
        ),
        ObjectMapping(
          tpe = SubObj3Type,
          fieldMappings =
            List(
              SqlField("id", subObj3.idCol, key = true)
            )
        )
      )
    )

  lazy val objectTypeDiscriminator = new SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] =
      Result.failure("discriminator not implemented")

    def narrowPredicate(subtpe: Type): Option[Predicate] = None
  }
}
