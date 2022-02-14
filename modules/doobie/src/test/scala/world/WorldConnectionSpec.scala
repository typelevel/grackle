// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Sync
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor
import edu.gemini.grackle._
import grackle.test.SqlWorldConnectionMapping
import grackle.test.SqlWorldConnectiondSpec
import utils.DatabaseSuite

import doobie.{DoobieMapping, DoobieMappingCompanion, DoobieMonitor}

trait WorldConnectionMapping[F[_]] extends DoobieMapping[F] with SqlWorldConnectionMapping[F] {

  def country = new TableDef("country") with CountryTable {
    def code           = col("code", Meta[String])
    def name           = col("name", Meta[String])
    def continent      = col("continent", Meta[String])
    def region         = col("region", Meta[String])
    def surfacearea    = col("surfacearea", Meta[String])
    def indepyear      = col("indepyear", Meta[Int], true)
    def population     = col("population", Meta[Int])
    def lifeexpectancy = col("lifeexpectancy", Meta[String], true)
    def gnp            = col("gnp", Meta[String], true)
    def gnpold         = col("gnpold", Meta[String], true)
    def localname      = col("localname", Meta[String])
    def governmentform = col("governmentform", Meta[String])
    def headofstate    = col("headofstate", Meta[String], true)
    def capitalId      = col("capitalId", Meta[String], true)
    def code2          = col("code2", Meta[String])
  }

}

object WorldConnectionMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with WorldConnectionMapping[F]
}

final class WorldConnectionSpec extends DatabaseSuite with SqlWorldConnectiondSpec {
  lazy val mapping = WorldConnectionMapping.fromTransactor(xa)
}
