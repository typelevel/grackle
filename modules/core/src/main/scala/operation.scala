// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import Query._

sealed trait UntypedOperation {
  val query: Query
  val variables: UntypedVarDefs
}
object UntypedOperation {
  case class UntypedQuery(query: Query,  variables: UntypedVarDefs) extends UntypedOperation
  case class UntypedMutation(query: Query,  variables: UntypedVarDefs) extends UntypedOperation
  case class UntypedSubscription(query: Query,  variables: UntypedVarDefs) extends UntypedOperation
}
