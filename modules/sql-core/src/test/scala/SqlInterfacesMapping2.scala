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

import grackle._
import grackle.syntax._
import Predicate._

trait SqlInterfacesMapping2[F[_]] extends SqlInterfacesMapping[F] { self =>

  override lazy val entityTypeDiscriminator = new SqlDiscriminator {

    // discriminator always fails
    def discriminate(c: Cursor): Result[Type] =
      Result.internalError("Boom!!!")

    // same as in SqlInterfacesMapping
    def narrowPredicate(subtpe: Type): Result[Predicate] = {
      def mkPredicate(tpe: EntityType): Result[Predicate] =
        Eql(EType / "entityType", Const(tpe)).success

      subtpe match {
        case FilmType => mkPredicate(EntityType.Film)
        case SeriesType => mkPredicate(EntityType.Series)
        case _ => Result.internalError(s"Invalid discriminator: $subtpe")
      }
    }
  }
}
