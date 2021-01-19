// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.NonEmptyChain
import cats.syntax.all._
import io.circe.literal.JsonStringContext

import Query._

sealed trait UntypedOperation {
  val query: Query
  val variables: UntypedVarDefs
  def rootTpe(schema: Schema): Result[NamedType] =
    this match {
      case UntypedOperation.UntypedQuery(_, _)        => schema.queryType.rightIor
      case UntypedOperation.UntypedMutation(_, _)     => schema.mutationType.toRight(NonEmptyChain.one(json"""{"message": "No mutation type defined in this schema."}""")).toIor
      case UntypedOperation.UntypedSubscription(_, _) => schema.subscriptionType.toRight(NonEmptyChain.one(json"""{"message": "No subscription type defined in this schema."}""")).toIor
    }
}
object UntypedOperation {
  case class UntypedQuery(query: Query,  variables: UntypedVarDefs) extends UntypedOperation
  case class UntypedMutation(query: Query,  variables: UntypedVarDefs) extends UntypedOperation
  case class UntypedSubscription(query: Query,  variables: UntypedVarDefs) extends UntypedOperation
}

case class Operation(query: Query, rootTpe: NamedType)

