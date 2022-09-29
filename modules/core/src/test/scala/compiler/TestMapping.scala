// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.{Id, Monad}

import edu.gemini.grackle._

abstract class TestMapping(implicit val M: Monad[Id]) extends Mapping[Id] {
  val typeMappings: List[TypeMapping] = Nil
}
