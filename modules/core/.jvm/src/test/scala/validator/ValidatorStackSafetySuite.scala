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

package validator

import cats.effect.IO
import compiler.TestMapping
import munit.CatsEffectSuite

import grackle.{Schema, ValidationFailure}

/**
 * JVM only: needs a thread with a fixed, small stack so the result does not depend on the
 * stack size the test runner happens to give us.
 */
final class ValidatorStackSafetySuite extends CatsEffectSuite {
  test("validation is stack safe for large mappings") {
    val n = 5000
    val names = (0 until n).map(i => s"Foo$i")

    object M extends TestMapping {
      val schema =
        Schema(
          s"""
            type Query {
              ${names.map(nm => s"${nm.toLowerCase}: $nm").mkString("\n")}
            }

            ${names.map(nm => s"type $nm { bar: String }").mkString("\n")}
          """
        ).toOption.get

      override val typeMappings =
        TypeMappings.unchecked(
          ObjectMapping(schema.ref("Query"))(
            names.map(nm => CursorField[String](nm.toLowerCase, _ => ???, Nil)): _*
          ) ::
          names.toList.map(nm =>
            ObjectMapping(schema.ref(nm))(
              CursorField[String]("bar", _ => ???, Nil)
            )
          )
        )
    }

    onSmallStack(M.validate()).assertEquals(Nil)
  }

  private def onSmallStack(validate: => List[ValidationFailure]): IO[List[ValidationFailure]] =
    IO.async_ { cb =>
      // A StackOverflowError is fatal, so catch Throwable or the callback never fires.
      val run: Runnable = () => cb(try Right(validate) catch { case t: Throwable => Left(t) })
      val t = new Thread(null, run, "validator", 256L * 1024)
      t.setDaemon(true)
      t.start()
    }
}
