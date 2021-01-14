// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package validator

import cats.effect.IO
import edu.gemini.grackle._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.sql.SqlMappingValidator
import _root_.skunk.codec.all._

final class ValidatorSpec extends AnyFunSuite {

  abstract class BaseTestMapping extends SkunkMapping[IO](null, SkunkMonitor.noopMonitor)

  test("inconsistent type mapping") {

    object M extends BaseTestMapping {
      val schema = Schema("type Foo { bar: Baz }, scalar Baz").right.get
      val typeMappings: List[TypeMapping] =
      List(
        ObjectMapping(
          tpe = schema.ref("Foo"),
          fieldMappings =
            List(
              SqlField("bar", ColumnRef("foo", "bar", int2))
            )
        ),
        LeafMapping[String](schema.ref("Baz")),
      )
    }

    val v =  SqlMappingValidator(M)
    val es = v.validateMapping()
    es.toList match {
      case List(v.InconsistentTypeMapping(_, _, _, _)) => succeed
      case _ => fail(es.toString())
    }

  }

}
