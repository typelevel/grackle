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

import java.util.UUID

import cats.Eq
import io.circe.{Decoder, Encoder}

import grackle._
import grackle.syntax._

trait SqlMappingValidatorValidMapping[F[_]] extends SqlTestMapping[F] {
  def genre: TestCodec[Genre]
  def feature: TestCodec[Feature]

  object scalars extends TableDef("scalars") {
    val idCol = col("idCol", int4)

    val boolCol = col("boolCol", bool)
    val nullableBoolCol = col("nullableBoolCol", nullable(bool))

    val textCol = col("textCol", text)
    val nullableTextCol = col("nullableTextCol", nullable(text))
    val varcharCol = col("varcharCol", varchar)
    val nullableVarcharCol = col("nullableVarcharCol", nullable(varchar))
    val bpcharCol = col("bpcharCol", bpchar(32))
    val nullableBpcharCol = col("nullableBpcharCol", nullable(bpchar(32)))

    val int2Col = col("int2Col", int2)
    val nullableInt2Col = col("nullableInt2Col", nullable(int2))
    val int4Col = col("int4Col", int4)
    val nullableInt4Col = col("nullableInt4Col", nullable(int4))
    val int8Col = col("int8Col", int8)
    val nullableInt8Col = col("nullableInt8Col", nullable(int8))

    val float4Col = col("float4Col", float4)
    val nullableFloat4Col = col("nullableFloat4Col", nullable(float4))
    val float8Col = col("float8Col", float8)
    val nullableFloat8Col = col("nullableFloat8Col", nullable(float8))

    val numericCol = col("numericCol", numeric(10, 2))
    val nullableNumericCol = col("nullableNumericCol", nullable(numeric(10, 2)))

    val uuidcol = col("uuidcol", uuid)
    val nullableUuidCol = col("nullableUuidCol", nullable(uuid))

    val genreCol = col("genreCol", genre)
    val nullableGenreCol = col("nullableGenreCol", nullable(genre))

    val featureCol = col("featureCol", feature)
    val nullableFeatureCol = col("nullableFeatureCol", nullable(feature))

    val featuresCol = col("featuresCol", list(feature))
    val nullableFeatures1Col = col("nullableFeatures1Col", nullable(list(nullable(feature))))
    val nullableFeatures2Col = col("nullableFeatures2Col", list(nullable(feature)))
    val nullableFeatures3Col = col("nullableFeatures3Col", nullable(list(feature)))

    val jsonbCol = col("jsonbCol", jsonb)
    val nullableJsonbCol = col("nullableJsonbCol", nullable(jsonb))
  }

  object objs extends TableDef("objs") {
    val idCol = col("idCol", int4)
    val typeCol = col("typeCol", int4)
    val intCol = col("intCol", int4)
    val boolCol = col("boolCol", bool)
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
    val parentBolCol = col("parentBoolCol", int4)
  }

  val schema =
    schema"""
      type Query {
        scalars: Scalars
        objs: [Intrf]
        union: Union
      }

      type Scalars {
        boolField: Boolean!
        nullableBoolField: Boolean

        textField: String!
        nullableTextField: String
        varcharField: String!
        nullableVarcharField: String
        bpcharField: String!
        nullableBpcharField: String

        idField: ID!
        nullableIdField: ID

        int2Field: Int!
        nullableInt2Field: Int
        int4Field: Int!
        nullableInt4Field: Int
        int8Field: Int!
        nullableInt8Field: Int

        float4Field: Float!
        nullableFloat4Field: Float
        float8Field: Float!
        nullableFloat8Field: Float

        numericField: Float!
        nullablenumericField: Float

        uuidField: UUID!
        nullableUuidField: UUID

        genreField: Genre!
        nullableGenreField: Genre

        featureField: Feature!
        nullableFeatureField: Feature

        featuresField: [Feature!]!
        nullableFeatures1Field: [Feature]
        nullableFeatures2Field: [Feature]!
        nullableFeatures3Field: [Feature!]

        jsonbField: Record!
        nullableJsonbField: Record
      }

      scalar UUID

      enum Genre {
        DRAMA
        ACTION
        COMEDY
      }
      enum Feature {
        HD
        HLS
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
        sub1: SubObj1
      }

      type Obj2 implements Intrf {
        id: Int!
        boolField: Boolean!
        sub2: SubObj2
      }

      type SubObj1 {
        id: Int!
      }

      type SubObj2 {
        id: Int!
      }


      union Union = Obj1 | Obj2
    """

  val QueryType = schema.ref("Query")
  val ScalarsType = schema.ref("Scalars")
  val UUIDType = schema.ref("UUID")
  val GenreType = schema.ref("Genre")
  val FeatureType = schema.ref("Feature")
  val IntrfType = schema.ref("Intrf")
  val Obj1Type = schema.ref("Obj1")
  val Obj2Type = schema.ref("Obj2")
  val UnionType = schema.ref("Union")
  val SubObj1Type = schema.ref("SubObj1")
  val SubObj2Type = schema.ref("SubObj2")

  override val typeMappings =
    TypeMappings.unchecked(
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
              SqlField("id", scalars.idCol, key = true, hidden = true),

              SqlField("boolField", scalars.boolCol),
              SqlField("nullableBoolField", scalars.nullableBoolCol),

              SqlField("textField", scalars.textCol),
              SqlField("nullableTextField", scalars.nullableTextCol),
              SqlField("varcharField", scalars.varcharCol),
              SqlField("nullableVarcharField", scalars.nullableVarcharCol),
              SqlField("bpcharField", scalars.bpcharCol),
              SqlField("nullableBpcharField", scalars.nullableBpcharCol),

              SqlField("idField", scalars.textCol),
              SqlField("nullableIdField", scalars.nullableTextCol),

              SqlField("int2Field", scalars.int2Col),
              SqlField("nullableInt2Field", scalars.nullableInt2Col),
              SqlField("int4Field", scalars.int4Col),
              SqlField("nullableInt4Field", scalars.nullableInt4Col),
              SqlField("int8Field", scalars.int8Col),
              SqlField("nullableInt8Field", scalars.nullableInt8Col),

              SqlField("float4Field", scalars.float4Col),
              SqlField("nullableFloat4Field", scalars.nullableFloat4Col),
              SqlField("float8Field", scalars.float8Col),
              SqlField("nullableFloat8Field", scalars.nullableFloat8Col),

              SqlField("numericField", scalars.numericCol),
              SqlField("nullablenumericField", scalars.nullableNumericCol),

              SqlField("uuidField", scalars.uuidcol),
              SqlField("nullableUuidField", scalars.nullableUuidCol),

              SqlField("genreField", scalars.genreCol),
              SqlField("nullableGenreField", scalars.nullableGenreCol),

              SqlField("featureField", scalars.featureCol),
              SqlField("nullableFeatureField", scalars.nullableFeatureCol),

              SqlField("featuresField", scalars.featuresCol),
              SqlField("nullableFeatures1Field", scalars.nullableFeatures1Col),
              SqlField("nullableFeatures2Field", scalars.nullableFeatures2Col),
              SqlField("nullableFeatures3Field", scalars.nullableFeatures3Col),

              SqlJson("jsonbField", scalars.jsonbCol),
              SqlJson("nullableJsonbField", scalars.nullableJsonbCol)
            )
        ),
        SqlInterfaceMapping(
          tpe = IntrfType,
          discriminator = objectTypeDiscriminator,
          fieldMappings =
            List(
              SqlField("id", objs.idCol, key = true),
              SqlField("typeField", objs.typeCol, discriminator = true, hidden = true)
            )
        ),
        ObjectMapping(
          tpe = Obj1Type,
          fieldMappings =
            List(
              SqlField("intField", objs.intCol),
              SqlObject("sub1", Join(objs.idCol, join.parentIdCol), Join(join.childIdCol, subObj1.idCol))
            )
        ),
        ObjectMapping(
          tpe = Obj2Type,
          fieldMappings =
            List(
              SqlField("boolField", objs.boolCol),
              SqlObject("sub2", Join(List((objs.idCol, subObj2.idCol), (objs.boolCol, subObj2.parentBolCol))))
            )
        ),
        SqlUnionMapping(
          tpe = UnionType,
          discriminator = objectTypeDiscriminator,
          fieldMappings =
            List(
              SqlField("id", objs.idCol, key = true, hidden = true),
              SqlField("typeField", objs.typeCol, discriminator = true, hidden = true)
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
        LeafMapping[UUID](UUIDType),
        LeafMapping[Genre](GenreType),
        LeafMapping[Feature](FeatureType)
      )
    )

  lazy val objectTypeDiscriminator = new SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] =
      Result.internalError("discriminator not implemented")

    def narrowPredicate(subtpe: Type): Result[Predicate] =
      Result.internalError("discriminator not implemented")
  }

  sealed trait Genre extends Product with Serializable
  object Genre {
    case object Drama extends Genre
    case object Action extends Genre
    case object Comedy extends Genre

    implicit val genreEq: Eq[Genre] = Eq.fromUniversalEquals[Genre]

    def fromString(s: String): Option[Genre] =
      s.trim.toUpperCase match {
        case "DRAMA"  => Some(Drama)
        case "ACTION" => Some(Action)
        case "COMEDY" => Some(Comedy)
        case _ => None
      }

    implicit val genreEncoder: io.circe.Encoder[Genre] =
      Encoder[String].contramap(_ match {
        case Drama => "DRAMA"
        case Action => "ACTION"
        case Comedy => "COMEDY"
      })

    def fromInt(i: Int): Genre =
      (i: @unchecked) match {
        case 1 => Drama
        case 2 => Action
        case 3 => Comedy
      }

    def toInt(f: Genre): Int =
      f match {
        case Drama  => 1
        case Action => 2
        case Comedy => 3
      }
  }

  sealed trait Feature
  object Feature {
    case object HD extends Feature
    case object HLS extends Feature

    def fromString(s: String): Feature = (s.trim.toUpperCase: @unchecked) match {
      case "HD" => HD
      case "HLS" => HLS
    }

    implicit def featureEncoder: io.circe.Encoder[Feature] =
      Encoder[String].contramap(_.toString)

    implicit def featureDecoder: io.circe.Decoder[Feature] =
      Decoder[String].map(fromString)
  }
}
