// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package subscription

import utils.DatabaseSuite
import edu.gemini.grackle.syntax._
import io.circe.Json
import cats.effect.IO
import skunk.implicits._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class SubscriptionSpec extends DatabaseSuite {

  lazy val mapping = SubscriptionMapping.mkMapping(pool)
  implicit val ioTimer = IO.timer(ExecutionContext.global)

  test("subscription driven by a Postgres channel") {

    val query = """
      subscription {
        channel {
          name
          country {
            name
          }
        }
      }
    """

    val expected = List(
      json"""
        {
          "data" : {
            "channel" : {
              "name" : "Godoy Cruz",
              "country" : {
                "name" : "Argentina"
              }
            }
          }
        }
      """,
      json"""
        {
          "data" : {
            "channel" : {
              "name" : "Posadas",
              "country" : {
                "name" : "Argentina"
              }
            }
          }
        }
      """,
    )

    val prog: IO[List[Json]] =
      for {

        // start a fiber that subscibes and takes the first two notifications
        fi <- mapping.compileAndRunAll(query).take(2).compile.toList.start

        // We're racing now, so wait a sec before starting notifications
        _  <- IO.sleep(1.second)

        // Send some notifications through Postgres, which will trigger queries on the subscription.
        _  <- pool.use { s =>
                val ch = s.channel(id"city_channel").contramap[Int](_.toString)
                List(101, 102, 103).traverse_(ch.notify)
              }

        // Now rejoin the fiber
        js <- fi.join

      } yield js

    // Done
    assert(prog.unsafeRunSync() == expected)

  }
}
