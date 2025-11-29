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
