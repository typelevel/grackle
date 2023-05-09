// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.MonadThrow
import cats.effect.IO

import edu.gemini.grackle._

abstract class TestMapping(implicit val M: MonadThrow[IO]) extends Mapping[IO] {
  val typeMappings: List[TypeMapping] = Nil
}
