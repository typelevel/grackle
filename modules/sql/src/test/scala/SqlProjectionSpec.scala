// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import edu.gemini.grackle.QueryExecutor

trait SqlProjectionSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]
}
