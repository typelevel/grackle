// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.{Resource, Sync}
import skunk.Session
import edu.gemini.grackle._, skunk._
import utils.DatabaseSuite
import _root_.skunk.codec.all._
import grackle.test.SqlWorldConnectiondSpec
import grackle.test.SqlWorldConnectionMapping

trait WorldConnectionMapping[F[_]] extends SkunkMapping[F] with SqlWorldConnectionMapping[F] {

  def country = new TableDef("country") with CountryTable {
    def code           = col("code", bpchar(3))
    def name           = col("name", text)
    def continent      = col("continent", text)
    def region         = col("region", text)
    def surfacearea    = col("surfacearea", varchar)
    def indepyear      = col("indepyear", int2.imap(_.toInt)(_.toShort).opt)
    def population     = col("population", int4)
    def lifeexpectancy = col("lifeexpectancy", varchar.opt)
    def gnp            = col("gnp", varchar.opt)
    def gnpold         = col("gnpold", varchar.opt)
    def localname      = col("localname", varchar)
    def governmentform = col("governmentform", varchar)
    def headofstate    = col("headofstate", varchar.opt)
    def capitalId      = col("capitalId", varchar.opt)
    def code2          = col("code2", varchar)
  }

}

object WorldConnectionMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with WorldConnectionMapping[F]
}

final class WorldConnectionSpec extends DatabaseSuite with SqlWorldConnectiondSpec {
  lazy val mapping = WorldConnectionMapping.mkMapping(pool)
}
