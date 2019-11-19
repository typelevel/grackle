// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.data.{ Ior, NonEmptyChain }
import cats.implicits._

import Query._, Binding._
import QueryInterpreter.{ mkError, mkErrorResult }
import Ast.{ Type => _, _ }, OperationDefinition._, OperationType._, Selection._, Value._

object Compiler {
  def toResult[T](pr: Either[String, T]): Result[T] =
    Ior.fromEither(pr).leftMap(msg => NonEmptyChain.one(mkError(msg)))

  def compileText(text: String): Result[Query] =
    for {
      doc   <- toResult(Parser.Document.parseOnly(text).either)
      query <- compileDocument(doc)
    } yield query

  def compileDocument(doc: Document): Result[Query] = doc match {
    case List(Left(op: Operation)) => compileOperation(op)
    case _ => mkErrorResult("Operation required")
  }

  def compileOperation(op: Operation): Result[Query] = op match {
    case Operation(Query, _, _, _, sels) =>
      compileSelections(sels)
    case _ => mkErrorResult("Selection required")
  }

  def compileSelections(sels: List[Selection]): Result[Query] =
    sels.traverse(compileSelection).map { sels0 =>
      if (sels0.size == 1) sels0.head else Group(sels0)
    }

  def compileSelection(sel: Selection): Result[Query] = sel match {
    case Field(_, name, args, _, sels) =>
      for {
        args0 <- compileArgs(args)
        sels0 <- compileSelections(sels)
      } yield {
        if (sels.isEmpty) Select(name.value, args0, Empty)
        else Select(name.value, args0, sels0)
      }
    case _ => mkErrorResult("Field required")
  }

  def compileArgs(args: List[(Name, Value)]): Result[List[Binding]] =
    args.traverse((compileArg _).tupled)

  def compileArg(name: Name, value: Value): Result[Binding] = value match {
    case StringValue(s) => StringBinding(name.value, s).rightIor
    case _ => mkErrorResult("Argument required")
  }
}

class SelectElaborator(mapping: Map[Type, PartialFunction[Select, Result[Query]]]) {
  def apply(query: Query, tpe: Type): Result[Query] =
    query match {
      case Select(fieldName, args, child) =>
        val childTpe = tpe.underlyingField(fieldName)
        apply(child, childTpe).flatMap { elaboratedChild =>
          val elaborated0 = Select(fieldName, args, elaboratedChild)
          mapping.get(tpe.underlyingObject) match {
            case Some(elaborator) if elaborator.isDefinedAt(elaborated0) => elaborator(elaborated0)
            case _ => elaborated0.rightIor
          }
        }

      case w@Wrap(fieldName, child) => apply(child, tpe.underlyingField(fieldName)).map(ec => w.copy(child = ec))
      case g@Group(queries)         => queries.traverse(q => apply(q, tpe)).map(eqs => g.copy(queries = eqs))
      case u@Unique(_, child)       => apply(child, tpe.nonNull).map(ec => u.copy(child = ec))
      case f@Filter(_, child)       => apply(child, tpe.item).map(ec => f.copy(child = ec))
      case c@Component(_, _, child) => apply(child, tpe).map(ec => c.copy(child = ec))
      case Empty                    => Empty.rightIor
    }
}

class ComponentElaborator private (mapping: Map[(Type, String), (SchemaComponent, (Cursor, Query) => Result[Query])]) {
  def apply(query: Query, tpe: Type): Result[Query] =
    query match {
      case Select(fieldName, args, child) =>
        val childTpe = tpe.underlyingField(fieldName)
        mapping.get((tpe.underlyingObject, fieldName)) match {
          case Some((schema, join)) =>
            apply(child, childTpe).map { elaboratedChild =>
              Wrap(fieldName, Component(schema, join, elaboratedChild))
            }
          case None =>
            apply(child, childTpe).map { elaboratedChild =>
              Select(fieldName, args, elaboratedChild)
            }
        }

      case Wrap(fieldName, child) =>
        val childTpe = tpe.underlyingField(fieldName)
        mapping.get((tpe.underlyingObject, fieldName)) match {
          case Some((schema, join)) =>
            apply(child, childTpe).map { elaboratedChild =>
              Wrap(fieldName, Component(schema, join, Wrap(fieldName, elaboratedChild)))
            }
          case None =>
            apply(child, childTpe).map { elaboratedChild =>
              Wrap(fieldName, elaboratedChild)
            }
        }

      case g@Group(queries)         => queries.traverse(q => apply(q, tpe)).map(eqs => g.copy(queries = eqs))
      case u@Unique(_, child)       => apply(child, tpe.nonNull).map(ec => u.copy(child = ec))
      case f@Filter(_, child)       => apply(child, tpe.item).map(ec => f.copy(child = ec))
      case c@Component(_, _, child) => apply(child, tpe).map(ec => c.copy(child = ec))
      case Empty                    => Empty.rightIor
    }
}

object ComponentElaborator {
  val TrivialJoin = (_: Cursor, q: Query) => q.rightIor

  case class Mapping(tpe: Type, fieldName: String, schema: SchemaComponent, join: (Cursor, Query) => Result[Query] = TrivialJoin)

  def apply(mappings: Mapping*): ComponentElaborator =
    new ComponentElaborator(mappings.map(m => ((m.tpe, m.fieldName), (m.schema, m.join))).toMap)
}
