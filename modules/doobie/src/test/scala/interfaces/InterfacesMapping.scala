// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import cats.effect.Sync
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import utils.{DatabaseSuite, DoobieTestMapping}
import grackle.test.{SqlInterfacesSpec, SqlRecursiveInterfacesSpec}

object InterfacesMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlInterfacesMapping[F] {
      def entityType: Codec =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

object RecursiveInterfacesMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlRecursiveInterfacesMapping[F] {
      def itemType: Codec =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class InterfacesSpec extends DatabaseSuite with SqlInterfacesSpec {
  lazy val mapping = InterfacesMapping.mkMapping(xa)
}

final class RecursiveInterfacesSpec extends DatabaseSuite with SqlRecursiveInterfacesSpec {
  lazy val mapping = RecursiveInterfacesMapping.mkMapping(xa)
}
