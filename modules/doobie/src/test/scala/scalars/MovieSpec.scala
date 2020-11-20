// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import io.circe.literal.JsonStringContext
import utils.DatabaseSuite
import grackle.test.SqlMovieSpec

final class MovieSpec extends DatabaseSuite with SqlMovieSpec {
  lazy val mapping = MovieMapping.fromTransactor(xa)

  // N.B. this doesn't work in skunk yet, when it does we'll pull it out
  test("query with arrays") {
    val query = """
      query {
        movieById(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          categories
          features
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movieById" : {
            "categories" : [
              "drama",
              "comedy"
            ],
            "features" : [
              "HD",
              "HLS"
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

}
