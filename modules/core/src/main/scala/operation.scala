// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle

import syntax._
import Query._

sealed trait UntypedOperation {
  val name: Option[String]
  val query: Query
  val variables: UntypedVarDefs
  val directives: List[Directive]
  def rootTpe(schema: Schema): Result[NamedType] =
    this match {
      case _: UntypedOperation.UntypedQuery        => schema.queryType.success
      case _: UntypedOperation.UntypedMutation     => schema.mutationType.toResult("No mutation type defined in this schema.")
      case _: UntypedOperation.UntypedSubscription => schema.subscriptionType.toResult("No subscription type defined in this schema.")
    }
}
object UntypedOperation {
  case class UntypedQuery(
    name: Option[String],
    query: Query,
    variables: UntypedVarDefs,
    directives: List[Directive]
  ) extends UntypedOperation
  case class UntypedMutation(
    name: Option[String],
    query: Query,
    variables: UntypedVarDefs,
    directives: List[Directive]
  ) extends UntypedOperation
  case class UntypedSubscription(
    name: Option[String],
    query: Query,
    variables: UntypedVarDefs,
    directives: List[Directive]
  ) extends UntypedOperation
}

case class Operation(
  query: Query,
  rootTpe: NamedType,
  directives: List[Directive]
)
