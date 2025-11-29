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

package compiler

import grackle._
import Query._
import QueryCompiler._

object PreserveArgsElaborator extends SelectElaborator {
  case class Preserved(args: List[Binding], directives: List[Directive])

  def subst(query: Query, fieldName: String, preserved: Preserved): Query = {
    def loop(query: Query): Query =
      query match {
        case Select(`fieldName`, alias, child) =>
          UntypedSelect(fieldName, alias, preserved.args, directives = preserved.directives, child)
        case Environment(env, child) if env.contains("preserved") => loop(child)
        case e@Environment(_, child) => e.copy(child = loop(child))
        case g: Group => g.copy(queries = g.queries.map(loop))
        case t@TransformCursor(_, child) => t.copy(child = loop(child))
        case other => other
      }

    loop(query)
  }

  override def transform(query: Query): Elab[Query] = {
    query match {
      case UntypedSelect(fieldName, _, _, _, _) =>
        for {
          t         <- super.transform(query)
          preserved <- Elab.envE[Preserved]("preserved")
        } yield subst(t, fieldName, preserved)

      case other => super.transform(other)
    }
  }

  def select(ref: TypeRef, name: String, args: List[Binding], directives: List[Directive]): Elab[Unit] =
    Elab.env("preserved", Preserved(args, directives))
}
