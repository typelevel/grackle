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

package grackle.sql.test

import cats.effect.IO
import munit.CatsEffectSuite

import grackle.sql._

// TableName is a path-dependent member of SqlMappingLike[F[_]], not a free-standing top-level
// type, so it needs a concrete mapping instance to reach - any SqlMappingLike[IO] will do, since
// these tests never touch the mapping's own fields or run a query.
trait SqlTableNameSuite extends CatsEffectSuite {
  def mapping: SqlMappingLike[IO]

  lazy val M = mapping

  test("unqualified name has no schema") {
    val tn = M.TableName("country")
    assertEquals(tn.schema, None)
    assertEquals(tn.name, "country")
  }

  test("schema-qualified name splits on the last dot") {
    val tn = M.TableName("public.country")
    assertEquals(tn.schema, Some("public"))
    assertEquals(tn.name, "country")
  }

  test("sqlRef renders the qualified reference, dots intact") {
    assertEquals(M.TableName("public.country").sqlRef, "public.country")
    assertEquals(M.TableName("country").sqlRef, "country")
  }

  test("identifier folds the qualifier to a bare-identifier-safe form") {
    assertEquals(M.TableName("public.country").identifier, "public_country")
    assertEquals(M.TableName("country").identifier, "country")
  }

  test("toString matches sqlRef") {
    val tn = M.TableName("public.country")
    assertEquals(tn.toString, tn.sqlRef)
  }
}
