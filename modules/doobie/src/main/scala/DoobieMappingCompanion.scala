// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import _root_.doobie.util.transactor.Transactor
// import cats.Monad
// import cats.arrow.FunctionK
// import cats.data.StateT
import cats.effect.Sync
// import cats.implicits._
import io.chrisdavenport.log4cats.Logger
// import io.circe.Json

// import Cursor.Env
// import QueryCompiler.IntrospectionLevel
// import IntrospectionLevel.Full

trait DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F]

  def fromTransactor[F[_] : Sync](transactor: Transactor[F]): Mapping[F] = {
    val monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[F]

    mkMapping(transactor, monitor)
  }
}

trait LoggedDoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F]

  def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): Mapping[F] = {
    val monitor: DoobieMonitor[F] = DoobieMonitor.loggerMonitor[F](Logger[F])

    mkMapping(transactor, monitor)
  }
}

// trait TracedDoobieMappingCompanion {
//   def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F]

//   def fromTransactor[F[_] : Sync](transactor: Transactor[F]): QueryExecutor[F, (Json, List[List[DoobieStats]])] = {
//     def liftF[T](ft: F[T]): StateT[F, List[List[DoobieStats]], T] = StateT.liftF(ft)
//     val stateMapping = mkMapping(transactor.mapK(FunctionK.lift(liftF)), DoobieMonitor.stateMonitor[F])

//     new QueryExecutor[F, (Json, List[List[DoobieStats]])] {
//       implicit val M: Monad[F] = Sync[F]

//       def run(query: Query, rootTpe: Type, env: Env): F[(Json, List[List[DoobieStats]])] =
//         stateMapping.run(query, rootTpe, env).run(Nil).map(_.swap)

//       def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty): F[(Json, List[List[DoobieStats]])] =
//         stateMapping.compileAndRun(text, name, untypedVars, introspectionLevel).run(Nil).map(_.swap)
//     }
//   }
// }
