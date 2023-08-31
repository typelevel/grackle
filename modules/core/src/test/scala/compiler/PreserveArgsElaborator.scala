// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import edu.gemini.grackle._
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
