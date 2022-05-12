// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package validator

import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename.TypeName
import skunk.codec.all._

import edu.gemini.grackle.syntax._
import edu.gemini.grackle.skunk.{SkunkMapping, SkunkMonitor}

final class SqlMappingValidatorSpec extends AnyFunSuite {

  abstract class BaseTestMapping extends SkunkMapping[IO](null, SkunkMonitor.noopMonitor)

  object M extends BaseTestMapping {
    val schema = schema"type Foo { bar: Baz }, scalar Baz"
    val typeMappings: List[TypeMapping] =
    List(
      ObjectMapping(
        tpe = schema.ref("Foo"),
        fieldMappings =
          List(
            SqlField("bar", ColumnRef("foo", "bar", (int2, false), implicitly[TypeName[Int]].value, implicitly[SourcePos]))
          )
      ),
      LeafMapping[String](schema.ref("Baz")),
    )
  }

  test("inconsistent type mapping") {
    val es = M.validator.validateMapping()
    es.toList match {
      case List(M.validator.InconsistentTypeMapping(_, _, _, _)) => succeed
      case _ => fail(es.toString())
    }
  }

}

object SqlMappingValidatorSpec {
  def main(args: Array[String]): Unit =
    new SqlMappingValidatorSpec().M.validator.unsafeValidate()
}
