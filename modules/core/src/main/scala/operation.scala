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
