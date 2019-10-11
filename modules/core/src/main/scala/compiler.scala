// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.implicits._

import Query._, Binding._
import Ast._, OperationDefinition._, OperationType._, Selection._, Value._

object Compiler {
  def compileText(text: String): Option[Query] =
    for {
      doc   <- Parser.Document.parseOnly(text).option
      query <- compileDocument(doc)
    } yield query

  def compileDocument(doc: Document): Option[Query] = doc match {
    case List(Left(op: Operation)) => compileOperation(op)
    case _ => None
  }

  def compileOperation(op: Operation): Option[Query] = op match {
    case Operation(Query, _, _, _, sels) =>
      compileSelections(sels)
    case _ => None
  }

  def compileSelections(sels: List[Selection]): Option[Query] =
    sels.map(compileSelection).sequence.map { sels0 =>
      if (sels0.size == 1) sels0.head else Group(sels0)
    }

  def compileSelection(sel: Selection): Option[Query] = sel match {
    case Field(_, name, args, _, sels) =>
      for {
        args0 <- compileArgs(args)
        sels0 <- compileSelections(sels)
      } yield {
        if (sels.isEmpty) Select(name.value, args0, Empty)
        else Select(name.value, args0, sels0)
      }
    case _ => None
  }

  def compileArgs(args: List[(Name, Value)]): Option[List[Binding]] =
    args.map((compileArg _).tupled).sequence

  def compileArg(name: Name, value: Value): Option[Binding] = value match {
    case StringValue(s) => Some(StringBinding(name.value, s))
    case _ => None
  }
}
