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

import cats.implicits.*
import grackle.*
import grackle.syntax.*
import Query.*
import Predicate.*
import Value.*
import QueryCompiler.*
import cats.Eq
import io.circe.Encoder

trait SqlIssue606Mapping[F[_]] extends SqlTestMapping[F] {  self =>

  def stepType: Codec
  
  object instruction extends TableDef("instruction") {
    val id = col("id", int4)
  }

  object callsequence extends TableDef("callsequence") {
    val stepKey = col("stepkey", int4)
    val caller = col("caller", int4)
    val callee = col("callee", nullable(int4)) // this, if not null, is instruction.id. A single instruction can be called multiple times from another one
    val stepType = col("textual", self.stepType)
    val description = col("description", nullable(int4)) // if this is set, it's a text only instruction and everything below here will be null
  }

  object richtext extends TableDef("richtext") {
    val id = col("id", int4)
    val html = col("html", nullable(text))
    val plain = col("plain", nullable(text))
  }
  
  val schema =
    schema"""
      type Query {
        instruction(id: Int): Instruction
      }
      type Instruction {
        sequence: [SequenceStep!]!
      }
      interface SequenceStep {
        id: Int!
        stepType: StepType!
      }
      type InstructionCall implements SequenceStep {
        id: Int!
        stepType: StepType!
      }
      type TextualStep implements SequenceStep {
        id: Int!
        stepType: StepType!
        description: RichText!
      }
      type RichText {
        id: Int!
        plain: String
        html: String
      }
      enum StepType {
        CALL
        TEXTUAL
      }
    """

  val QueryType           = schema.ref("Query")
  val InstructionCallType    = schema.ref("InstructionCall")
  val InstructionType        = schema.ref("Instruction")
  val TextOnlyStepType = schema.ref("TextualStep")
  val StepType            = schema.ref("SequenceStep")
  val StepTypeType        = schema.ref("StepType")
  val RichTextType        = schema.ref("RichText")

  lazy val stepTypeDiscriminator = new SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] = {
      for {
        et <- c.fieldAs[StepCallType]("stepType")
      } yield et match {
        case StepCallType.InstructionCall => InstructionCallType
        case StepCallType.TextOnlyStep => TextOnlyStepType
      }
    }

    def narrowPredicate(subtpe: Type): Option[Predicate] = {
      def mkPredicate(tpe: StepCallType): Option[Predicate] =
        Some(Eql(StepType / "stepType", Const(tpe)))

      subtpe match {
        case InstructionCallType => mkPredicate(StepCallType.InstructionCall)
        case TextOnlyStepType => mkPredicate(StepCallType.TextOnlyStep)
        case _ => None
      }
    }
  }

  sealed trait StepCallType extends Product with Serializable
  object StepCallType {
    case object InstructionCall extends StepCallType
    case object TextOnlyStep extends StepCallType

    implicit val entityTypeEq: Eq[StepCallType] = Eq.fromUniversalEquals[StepCallType]

    def fromString(s: String): Option[StepCallType] =
      s.trim.toUpperCase match {
        case "INSTRUCTION_CALL"  => Some(InstructionCall)
        case "TEXT_ONLY_STEP" => Some(TextOnlyStep)
        case _ => None
      }

    implicit val stepCallTypeEncoder: io.circe.Encoder[StepCallType] =
      Encoder[String].contramap {
        case InstructionCall => "INSTRUCTION_CALL"
        case TextOnlyStep => "TEXT_ONLY_STEP"
      }

    def fromBoolean(b: Boolean): StepCallType =
      if (b)
        TextOnlyStep
      else
        InstructionCall

    def toBoolean(e: StepCallType): Boolean =
      e == TextOnlyStep
  }
  
  val typeMappings: List[TypeMapping] =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("instruction"),
        )
      ),
      // #type_mappings
      ObjectMapping(
        tpe = InstructionCallType,
        fieldMappings = List(
        )
      ),
      ObjectMapping(
        tpe = InstructionType,
        fieldMappings = List(
          SqlField("id", instruction.id, key = true),
          SqlObject("sequence", Join(instruction.id, callsequence.caller)),
        )
      ),
      ObjectMapping(
        tpe = TextOnlyStepType,
        fieldMappings = List(
          SqlObject("description", Join(callsequence.description, richtext.id))
        )
      ),
      SqlInterfaceMapping(
        tpe = StepType,
        discriminator = stepTypeDiscriminator,
        fieldMappings =
          List(
            SqlField("id", callsequence.stepKey, key = true),
            SqlField("stepType", callsequence.stepType, discriminator = true),
          )
      ),
      ObjectMapping(
        tpe = RichTextType,
        fieldMappings = List(
          SqlField("id", richtext.id, key = true),
          SqlField("plain", richtext.plain),
          SqlField("html", richtext.html),
        )
      ),
      LeafMapping[StepCallType](StepTypeType),
      // #type_mappings
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "instruction", List(Binding("key", IntValue(key)))) =>
      Elab.transformChild { child =>
        Unique(Filter(Eql(InstructionType / "key", Const(key)), child))
      }
  }
}
