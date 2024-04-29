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

import cats.effect.IO
import cats.implicits._
import grackle._
import grackle.sql._
import munit.CatsEffectSuite
import org.tpolecat.typename.typeName

trait SqlMappingValidatorInvalidSuite extends CatsEffectSuite {
  def mapping: SqlMappingLike[IO]

  lazy val M = mapping

  def check(es: List[ValidationFailure])(expected: PartialFunction[List[ValidationFailure], Unit]): Unit =
    es match {
      case `expected`(_) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  object INFM {
    def unapply(vf: ValidationFailure): Option[(String, String, String, Boolean, String, String, Boolean)] =
      vf match {
        case M.InconsistentlyNullableFieldMapping(om, fm, field, columnRef, colIsNullable) =>
          Some((om.tpe.name, fm.fieldName, SchemaRenderer.renderType(field.tpe), field.tpe.isNullable, columnRef.table, columnRef.column, colIsNullable))
        case _ => None
      }
  }

  object IFLM {
    def unapply(vf: ValidationFailure): Option[(String, String, String, String, String, String)] =
      vf match {
        case M.InconsistentFieldLeafMapping(om, fm, field, columnRef, _) =>
          Some((om.tpe.name, fm.fieldName, SchemaRenderer.renderType(field.tpe), columnRef.table, columnRef.column, columnRef.scalaTypeName))
        case _ => None
      }
  }

  object IFTM {
    def unapply(vf: ValidationFailure): Option[(String, String, String, String, String, String)] =
      vf match {
        case M.InconsistentFieldTypeMapping(om, fm, field, columnRef, _) =>
          Some((om.tpe.name, fm.fieldName, SchemaRenderer.renderType(field.tpe), columnRef.table, columnRef.column, columnRef.scalaTypeName))
        case _ => None
      }
  }

  object NK {
    def unapply(vf: ValidationFailure): Option[String] =
      vf match {
        case M.NoKeyInObjectTypeMapping(om) =>
          Some(om.tpe.name)
        case _ => None
      }
  }

  object STM {
    def unapply(vf: ValidationFailure): Option[(String, List[String])] =
      vf match {
        case M.SplitObjectTypeMapping(om, tables) =>
          Some((om.tpe.name, tables))
        case _ => None
      }
  }

  object SEM {
    def unapply(vf: ValidationFailure): Option[(String, String, String, List[String], List[String])] =
      vf match {
        case M.SplitEmbeddedObjectTypeMapping(om, fm, com, parentTables, childTables) =>
          Some((om.tpe.name, fm.fieldName, com.tpe.name, parentTables, childTables))
        case _ => None
      }
  }

  object SIM {
    def unapply(vf: ValidationFailure): Option[(String, List[String], List[String])] =
      vf match {
        case M.SplitInterfaceTypeMapping(om, intrfs, tables) =>
          Some((om.tpe.name, intrfs.map(_.tpe.name), tables))
        case _ => None
      }
  }

  object SUM {
    def unapply(vf: ValidationFailure): Option[(String, List[String], List[String])] =
      vf match {
        case M.SplitUnionTypeMapping(om, members, tables) =>
          Some((om.tpe.name, members.map(_.tpe.name), tables))
        case _ => None
      }
  }

  object ND {
    def unapply(vf: ValidationFailure): Option[String] =
      vf match {
        case M.NoDiscriminatorInObjectTypeMapping(om) =>
          Some(om.tpe.name)
        case _ => None
      }
  }

  object ISMU {
    def unapply(vf: ValidationFailure): Option[(String, String)] =
      vf match {
        case M.IllegalSubobjectInUnionTypeMapping(om, fm) =>
          Some((om.tpe.name, fm.fieldName))
        case _ => None
      }
  }

  object ASNK {
    def unapply(vf: ValidationFailure): Option[(String, String)] =
      vf match {
        case M.AssocFieldNotKey(om, fm) =>
          Some((om.tpe.name, fm.fieldName))
        case _ => None
      }
  }

  object NHUFM {
    def unapply(vf: ValidationFailure): Option[(String, String)] =
      vf match {
        case M.NonHiddenUnionFieldMapping(om, fm) =>
          Some((om.tpe.name, fm.fieldName))
        case _ => None
      }
  }

  object NJC {
    def unapply(vf: ValidationFailure): Option[(String, String)] =
      vf match {
        case M.NoJoinConditions(om, fm) =>
          Some((om.tpe.name, fm.fieldName))
        case _ => None
      }
  }

  object IJC {
    def unapply(vf: ValidationFailure): Option[(String, String, List[String], List[String])] =
      vf match {
        case M.InconsistentJoinConditions(om, fm, parents, children) =>
          Some((om.tpe.name, fm.fieldName, parents, children))
        case _ => None
      }
  }

  object MJ {
    def unapply(vf: ValidationFailure): Option[(String, String, String, String, List[(String, String)])] =
      vf match {
        case M.MisalignedJoins(om, fm, parent, child, path) =>
          Some((om.tpe.name, fm.fieldName, parent, child, path))
        case _ => None
      }
  }

  object TypeNames {
    val BooleanTypeName = typeName[Boolean]
    val IntTypeName = typeName[Int]
    val ListStringTypeName = typeName[List[java.lang.String]]
  }

  import TypeNames._

  test("invalid mapping is invalid") {
    val es = M.validate()

    check(es.take(8)) {
      case
        List(
          INFM("Scalars", "boolField1", "Boolean!", false, "scalars", "nullableBoolCol", true),
          IFLM("Scalars", "boolField2", "Boolean!", "scalars", "intCol", IntTypeName),
          INFM("Scalars", "nullableBoolField1", "Boolean", true, "scalars", "boolCol", false),
          IFLM("Scalars", "nullableBoolField2", "Boolean", "scalars", "nullableIntCol", IntTypeName),
          INFM("Scalars", "stringsField", "[String]!", false, "scalars", "nullableStringsCol", true),
          INFM("Scalars", "nullableStringsField", "[String]", true, "scalars", "stringsCol", false),
          IFTM("Scalars", "jsonbField", "Record", "scalars", "boolCol", BooleanTypeName),
          IFTM("Scalars", "nullableJsonbField", "Record!", "scalars", "nullableBoolCol", BooleanTypeName),
        ) =>
    }

    check(es.drop(8)) {
      case
        List(
          NK("Scalars"),
          ND("Intrf"),
          NK("Intrf"),
          SEM("Obj1", "embedded", "Obj3", List("obj1"), List("obj2")),
          MJ("Obj1", "sub1", "obj1", "subObj1", List(("obj1", "join"), ("join", "obj1"))),
          NK("Obj1"),
          NJC("Obj3", "sub3"),
          IJC("Obj2", "sub2", List("obj2"), List("subObj2", "subObj3")),
          SIM("Obj2", List("Obj2", "Intrf"), List("obj2", "obj1")),
          STM("Obj2", List("obj2", "obj1")),
          ASNK("Obj2", "assoc"),
          NK("Obj2"),
          ISMU("Union", "bogus"),
          NHUFM("Union", "id"),
          SUM("Union", List("Obj1", "Obj2"), List("obj1", "obj2")),
          ND("Union"),
          NK("Union")
        ) =>
    }
  }
}
