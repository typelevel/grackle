// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package grackle.sql.test

import cats.effect.IO
import cats.implicits._
import grackle.sql._
import munit.CatsEffectSuite

trait SqlMappingValidatorValidSuite extends CatsEffectSuite {
  def mapping: SqlMappingLike[IO]

  lazy val M = mapping

  test("valid mapping is valid") {
    val es = M.validate()

    es match {
      case Nil => ()
      case _   => fail(es.foldMap(_.toErrorMessage))
    }
  }
}
