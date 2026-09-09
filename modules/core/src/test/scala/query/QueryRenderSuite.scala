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

package query

import munit.CatsEffectSuite

import grackle.Query._
import grackle.Value._

final class QueryRenderSuite extends CatsEffectSuite {
  test("binding renders the value") {
    assertEquals(Binding("id", StringValue("some-id")).render, "id: \"some-id\"")
  }

  test("select without a child renders the name only") {
    assertEquals(Select("name").render, "name")
  }

  test("select renders the alias and the child") {
    assertEquals(Select("hero", Some("r2"), Select("name")).render, "r2:hero { name }")
  }

  test("untyped select without arguments renders no parentheses") {
    assertEquals(UntypedSelect("character", None, Nil, Nil, Empty).render, "character")
  }

  test("untyped select renders argument values") {
    val query = UntypedSelect(
      "character",
      None,
      List(Binding("id", StringValue("some-id")), Binding("n", IntValue(3))),
      Nil,
      Empty)
    assertEquals(query.render, "character(id: \"some-id\", n: 3)")
  }

  test("nested untyped select renders arguments at each level") {
    val child = UntypedSelect("friends", None, List(Binding("first", IntValue(1))), Nil, Empty)
    val query =
      UntypedSelect("hero", None, List(Binding("ep", EnumValue("JEDI"))), Nil, child)
    assertEquals(query.render, "hero(ep: JEDI) { friends(first: 1) }")
  }

  test("group renders its members in braces") {
    assertEquals(Group(List(Select("name"), Select("age"))).render, "{name, age}")
  }
}
