// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.skunk.test.subscription

import scala.concurrent.duration._

import cats.effect.IO
import cats.implicits._
import io.circe.Json
import io.circe.literal._
import skunk.implicits._

import grackle.skunk.test.SkunkDatabaseSuite

class SubscriptionSuite extends SkunkDatabaseSuite {

  lazy val mapping = SubscriptionMapping.mkMapping(pool)

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
        fi <- mapping.compileAndRunSubscription(query).take(2).compile.toList.start

        // We're racing now, so wait a sec before starting notifications
        _  <- IO.sleep(1.second)

        // Send some notifications through Postgres, which will trigger queries on the subscription.
        _  <- pool.use { s =>
                val ch = s.channel(id"city_channel").contramap[Int](_.toString)
                List(101, 102, 103).traverse_(ch.notify)
              }

        // Now rejoin the fiber
        out <- fi.join
        js  <- out.embedNever

      } yield js

    // Done
    assertIO(prog, expected)

  }
}
