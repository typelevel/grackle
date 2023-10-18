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

package grackle

import cats.implicits._

object QueryMinimizer {
  import Ast._

  def minimizeText(text: String): Either[String, String] = {
    for {
      doc <- GraphQLParser.Document.parseAll(text).leftMap(_.expected.toList.mkString(","))
    } yield minimizeDocument(doc)
  }

  def minimizeDocument(doc: Document): String = {
    import OperationDefinition._
    import OperationType._
    import Selection._
    import Value._

    def renderDefinition(defn: Definition): String =
      defn match {
        case e: ExecutableDefinition => renderExecutableDefinition(e)
        case _ => ""
      }

    def renderExecutableDefinition(ex: ExecutableDefinition): String =
      ex match {
        case op: OperationDefinition => renderOperationDefinition(op)
        case frag: FragmentDefinition => renderFragmentDefinition(frag)
      }

    def renderOperationDefinition(op: OperationDefinition): String =
      op match {
        case qs: QueryShorthand => renderSelectionSet(qs.selectionSet)
        case op: Operation => renderOperation(op)
      }

    def renderOperation(op: Operation): String =
      renderOperationType(op.operationType) +
      op.name.map(nme => s" ${nme.value}").getOrElse("") +
      renderVariableDefns(op.variables)+
      renderDirectives(op.directives)+
      renderSelectionSet(op.selectionSet)

    def renderOperationType(op: OperationType): String =
      op match {
        case Query => "query"
        case Mutation => "mutation"
        case Subscription => "subscription"
      }

    def renderDirectives(dirs: List[Directive]): String =
      dirs.map { case Directive(name, args) => s"@${name.value}${renderArguments(args)}" }.mkString("")

    def renderVariableDefns(vars: List[VariableDefinition]): String =
      vars match {
        case Nil => ""
        case _ =>
          vars.map {
            case VariableDefinition(name, tpe, default, dirs) =>
              s"$$${name.value}:${tpe.name}${default.map(v => s"=${renderValue(v)}").getOrElse("")}${renderDirectives(dirs)}"
          }.mkString("(", ",", ")")
      }

    def renderSelectionSet(sels: List[Selection]): String =
      sels match {
        case Nil => ""
        case _ => sels.map(renderSelection).mkString("{", ",", "}")
      }

    def renderSelection(sel: Selection): String =
      sel match {
        case f: Field => renderField(f)
        case s: FragmentSpread => renderFragmentSpread(s)
        case i: InlineFragment => renderInlineFragment(i)
      }

    def renderField(f: Field) = {
      f.alias.map(a => s"${a.value}:").getOrElse("")+
      f.name.value+
      renderArguments(f.arguments)+
      renderDirectives(f.directives)+
      renderSelectionSet(f.selectionSet)
    }

    def renderArguments(args: List[(Name, Value)]): String =
      args match {
        case Nil => ""
        case _ => args.map { case (n, v) => s"${n.value}:${renderValue(v)}" }.mkString("(", ",", ")")
      }

    def renderInputObject(args: List[(Name, Value)]): String =
      args match {
        case Nil => ""
        case _ => args.map { case (n, v) => s"${n.value}:${renderValue(v)}" }.mkString("{", ",", "}")
      }

    def renderTypeCondition(tpe: Type): String =
      s"on ${tpe.name}"

    def renderFragmentDefinition(frag: FragmentDefinition): String =
      s"fragment ${frag.name.value} ${renderTypeCondition(frag.typeCondition)}${renderDirectives(frag.directives)}${renderSelectionSet(frag.selectionSet)}"

    def renderFragmentSpread(spread: FragmentSpread): String =
      s"...${spread.name.value}${renderDirectives(spread.directives)}"

    def renderInlineFragment(frag: InlineFragment): String =
      s"...${frag.typeCondition.map(renderTypeCondition).getOrElse("")}${renderDirectives(frag.directives)}${renderSelectionSet(frag.selectionSet)}"

    def renderValue(v: Value): String =
      v match {
        case Variable(name) => s"$$${name.value}"
        case IntValue(value) => value.toString
        case FloatValue(value) => value.toString
        case StringValue(value) => s""""$value""""
        case BooleanValue(value) => value.toString
        case NullValue => "null"
        case EnumValue(name) => name.value
        case ListValue(values) => values.map(renderValue).mkString("[", ",", "]")
        case ObjectValue(fields) => renderInputObject(fields)
      }

    doc.map(renderDefinition).mkString(",")
  }
}
