// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import edu.gemini.grackle._
import Predicate._

trait SqlInterfacesMapping2[F[_]] extends SqlInterfacesMapping[F] { self =>

  override lazy val entityTypeDiscriminator = new SqlDiscriminator {

    // discriminator always fails
    def discriminate(c: Cursor): Result[Type] =
      Result.failure("no")

    // same as in SqlInterfacesMapping
    def narrowPredicate(subtpe: Type): Option[Predicate] = {
      def mkPredicate(tpe: EntityType): Option[Predicate] =
        Some(Eql(EType / "entityType", Const(tpe)))

      subtpe match {
        case FilmType => mkPredicate(EntityType.Film)
        case SeriesType => mkPredicate(EntityType.Series)
        case _ => None
      }
    }
  }

}
