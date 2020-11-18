// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import _root_.skunk.Session
import cats.arrow.FunctionK
import cats.data.StateT
import cats.effect.{ Resource, Sync }
import cats.Monad
import cats.syntax.functor._
import io.circe.Json

trait SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F]

  final def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]]): Mapping[F] =
    mkMapping(pool, SkunkMonitor.noopMonitor)

}

// trait LoggedSkunkMappingCompanion {
//   def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: SkunkMonitor[F]): Mapping[F]

//   def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): Mapping[F] = {
//     val monitor: SkunkMonitor[F] = SkunkMonitor.loggerMonitor[F](Logger[F])

//     mkMapping(transactor, monitor)
//   }
// }

trait TracedSkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F]

  def fromSessionPool[F[_] : Sync](pool: Resource[F, Session[F]]): QueryExecutor[F, (Json, List[List[SkunkStats]])] = {

    type G[A] = StateT[F, List[List[SkunkStats]], A]

    def liftF[T](ft: F[T]): G[T] = StateT.liftF(ft)

    val sm: SkunkMonitor[G] = SkunkMonitor.stateMonitor[F]

    val fk: FunctionK[F, G] = FunctionK.lift(liftF)

    val stateMapping = mkMapping(pool.mapK(fk).map(_.mapK(fk)), sm)

    new QueryExecutor[F, (Json, List[List[SkunkStats]])] {
      implicit val M: Monad[F] = Sync[F]

      def run(query: Query, rootTpe: Type): F[(Json, List[List[SkunkStats]])] =
        stateMapping.run(query, rootTpe).run(Nil).map(_.swap)

      def compileAndRun(text: String, name: Option[String] = None, untypedEnv: Option[Json] = None, useIntrospection: Boolean = true): F[(Json, List[List[SkunkStats]])] =
        stateMapping.compileAndRun(text, name, untypedEnv, useIntrospection).run(Nil).map(_.swap)
    }
  }
}
