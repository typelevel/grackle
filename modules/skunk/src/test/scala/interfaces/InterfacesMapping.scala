// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import cats.effect.{Resource, Sync}
import skunk.Session
import skunk.codec.{all => codec}

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}

import utils.{DatabaseSuite, SkunkTestMapping}
import grackle.test.{SqlInterfacesSpec, SqlRecursiveInterfacesSpec}

object InterfacesMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlInterfacesMapping[F] {
      def entityType: Codec =
        (codec.int4.imap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

object RecursiveInterfacesMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlRecursiveInterfacesMapping[F] {
      def itemType: Codec =
        (codec.int4.imap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class InterfacesSpec extends DatabaseSuite with SqlInterfacesSpec {
  lazy val mapping = InterfacesMapping.mkMapping(pool)
}

final class RecursiveInterfacesSpec extends DatabaseSuite with SqlRecursiveInterfacesSpec {
  lazy val mapping = RecursiveInterfacesMapping.mkMapping(pool)
}
