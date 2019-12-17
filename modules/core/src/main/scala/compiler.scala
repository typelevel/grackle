// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.data.{ Ior, NonEmptyChain }
import cats.implicits._

import Query._, Binding._
import QueryCompiler._
import QueryInterpreter.{ mkError, mkErrorResult }
import Ast.{ Type => _, _ }, OperationDefinition._, OperationType._, Selection._, Value._

object QueryParser {
  def toResult[T](pr: Either[String, T]): Result[T] =
    Ior.fromEither(pr).leftMap(msg => NonEmptyChain.one(mkError(msg)))

  def parseText(text: String): Result[Query] =
    for {
      doc   <- toResult(Parser.Document.parseOnly(text).either)
      query <- parseDocument(doc)
    } yield query

  def parseDocument(doc: Document): Result[Query] = doc match {
    case List(Left(op: Operation)) => parseOperation(op)
    case List(Left(qs: QueryShorthand)) => parseQueryShorthand(qs)
    case _ => mkErrorResult("Operation required")
  }

  def parseOperation(op: Operation): Result[Query] = op match {
    case Operation(Query, _, _, _, sels) =>
      parseSelections(sels)
    case _ => mkErrorResult("Selection required")
  }

  def parseQueryShorthand(qs: QueryShorthand): Result[Query] = qs match {
    case QueryShorthand(sels) => parseSelections(sels)
    case _ => mkErrorResult("Selection required")
  }

  def parseSelections(sels: List[Selection]): Result[Query] =
    sels.traverse(parseSelection).map { sels0 =>
      if (sels0.size == 1) sels0.head else Group(sels0)
    }

  def parseSelection(sel: Selection): Result[Query] = sel match {
    case Field(_, name, args, _, sels) =>
      for {
        args0 <- parseArgs(args)
        sels0 <- parseSelections(sels)
      } yield {
        if (sels.isEmpty) Select(name.value, args0, Empty)
        else Select(name.value, args0, sels0)
      }
    case _ => mkErrorResult("Field required")
  }

  def parseArgs(args: List[(Name, Value)]): Result[List[Binding]] =
    args.traverse((parseArg _).tupled)

  def parseArg(name: Name, value: Value): Result[Binding] = value match {
    case IntValue(i) => IntBinding(name.value, i).rightIor
    case FloatValue(d) => FloatBinding(name.value, d).rightIor
    case StringValue(s) => StringBinding(name.value, s).rightIor
    case BooleanValue(b) => BooleanBinding(name.value, b).rightIor
    case EnumValue(e) => UntypedEnumBinding(name.value, e.value).rightIor
    case _ => mkErrorResult("Argument required")
  }
}

abstract class QueryCompiler(schema: Schema) {
  val phases: List[Phase]

  def compile(text: String): Result[Query] = {
    val query = QueryParser.parseText(text)
    val queryType = schema.queryType
    phases.foldLeft(query) { (acc, phase) => acc.flatMap(phase(_, queryType)) }
  }
}

object QueryCompiler {
  trait Phase {
    def apply(query: Query, tpe: Type): Result[Query]
  }

  class SelectElaborator(mapping: Map[Type, PartialFunction[Select, Result[Query]]]) extends Phase {
    def apply(query: Query, tpe: Type): Result[Query] =
      query match {
        case Select(fieldName, args, child) =>
          val childTpe = tpe.underlyingField(fieldName)
          val elaborator: Select => Result[Query] = mapping.get(tpe.underlyingObject) match {
            case Some(e) => (s: Select) => if (e.isDefinedAt(s)) e(s) else s.rightIor
            case _ => (s: Select) => s.rightIor
          }

          for {
            elaboratedChild <- apply(child, childTpe)
            elaboratedArgs <- elaborateArgs(tpe, fieldName, args)
            elaborated <- elaborator(Select(fieldName, elaboratedArgs, elaboratedChild))
          } yield elaborated

        case w@Wrap(_, child)         => apply(child, tpe).map(ec => w.copy(child = ec))
        case g@Group(queries)         => queries.traverse(q => apply(q, tpe)).map(eqs => g.copy(queries = eqs))
        case u@Unique(_, child)       => apply(child, tpe.nonNull).map(ec => u.copy(child = ec))
        case f@Filter(_, child)       => apply(child, tpe.item).map(ec => f.copy(child = ec))
        case c@Component(_, _, child) => apply(child, tpe).map(ec => c.copy(child = ec))
        case d@Defer(_, child)        => apply(child, tpe).map(ec => d.copy(child = ec))
        case Empty                    => Empty.rightIor
      }

    def elaborateArgs(tpe: Type, fieldName: String, args: List[Binding]): Result[List[Binding]] =
      tpe.underlyingObject match {
        case twf: TypeWithFields =>
          twf.fieldInfo(fieldName) match {
            case Some(field) =>
              val infos = field.args
              val argMap = args.groupMapReduce(_.name)(identity)((x, _) => x)
              infos.traverse { info =>
                argMap.get(info.name) match {
                  case Some(arg) => Binding.forArg(arg, info)
                  case None => Binding.defaultForInputValue(info)
                }
              }
            case _ => mkErrorResult(s"No field '$fieldName' in type $tpe")
          }
        case _ => mkErrorResult(s"Type $tpe is not an object or interface type")
      }
  }

  class ComponentElaborator private (mapping: Map[(Type, String), (String, (Cursor, Query) => Result[Query])]) extends Phase {
    def apply(query: Query, tpe: Type): Result[Query] =
      query match {
        case Select(fieldName, args, child) =>
          val childTpe = tpe.underlyingField(fieldName)
          mapping.get((tpe.underlyingObject, fieldName)) match {
            case Some((cid, join)) =>
              apply(child, childTpe).map { elaboratedChild =>
                Wrap(fieldName, Component(cid, join, Select(fieldName, args, elaboratedChild)))
              }
            case None =>
              apply(child, childTpe).map { elaboratedChild =>
                Select(fieldName, args, elaboratedChild)
              }
          }

        case w@Wrap(_, child)         => apply(child, tpe).map(ec => w.copy(child = ec))
        case g@Group(queries)         => queries.traverse(q => apply(q, tpe)).map(eqs => g.copy(queries = eqs))
        case u@Unique(_, child)       => apply(child, tpe.nonNull).map(ec => u.copy(child = ec))
        case f@Filter(_, child)       => apply(child, tpe.item).map(ec => f.copy(child = ec))
        case c@Component(_, _, child) => apply(child, tpe).map(ec => c.copy(child = ec))
        case d@Defer(_, child)        => apply(child, tpe).map(ec => d.copy(child = ec))
        case Empty                    => Empty.rightIor
      }
  }

  object ComponentElaborator {
    val TrivialJoin = (_: Cursor, q: Query) => q.rightIor

    case class Mapping(tpe: Type, fieldName: String, componentId: String, join: (Cursor, Query) => Result[Query] = TrivialJoin)

    def apply(mappings: Mapping*): ComponentElaborator =
      new ComponentElaborator(mappings.map(m => ((m.tpe, m.fieldName), (m.componentId, m.join))).toMap)
  }
}
