// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlEmbeddingSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("simple embedded query (1)") {
    val query = """
      query {
        films {
          title
          synopses {
            short
            long
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "title" : "Film 1",
              "synopses" : {
                "short" : "Short film 1",
                "long" : "Long film 1"
              }
            },
            {
              "title" : "Film 2",
              "synopses" : {
                "short" : "Short film 2",
                "long" : "Long film 2"
              }
            },
            {
              "title" : "Film 3",
              "synopses" : {
                "short" : "Short film 3",
                "long" : "Long film 3"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("simple embedded query (2)") {
    val query = """
      query {
        series {
          title
          synopses {
            short
            long
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "series" : [
            {
              "title" : "Series 1",
              "synopses" : {
                "short" : "Short series 1",
                "long" : "Long series 1"
              }
            },
            {
              "title" : "Series 2",
              "synopses" : {
                "short" : "Short series 2",
                "long" : "Long series 2"
              }
            },
            {
              "title" : "Series 3",
              "synopses" : {
                "short" : "Short series 3",
                "long" : "Long series 3"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("Multiply embedded query") {
    val query = """
      query {
        films {
          title
          synopses {
            short
            long
          }
        }
        series {
          title
          synopses {
            short
            long
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "title" : "Film 1",
              "synopses" : {
                "short" : "Short film 1",
                "long" : "Long film 1"
              }
            },
            {
              "title" : "Film 2",
              "synopses" : {
                "short" : "Short film 2",
                "long" : "Long film 2"
              }
            },
            {
              "title" : "Film 3",
              "synopses" : {
                "short" : "Short film 3",
                "long" : "Long film 3"
              }
            }
          ],
          "series" : [
            {
              "title" : "Series 1",
              "synopses" : {
                "short" : "Short series 1",
                "long" : "Long series 1"
              }
            },
            {
              "title" : "Series 2",
              "synopses" : {
                "short" : "Short series 2",
                "long" : "Long series 2"
              }
            },
            {
              "title" : "Series 3",
              "synopses" : {
                "short" : "Short series 3",
                "long" : "Long series 3"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("nested embedded query") {
    val query = """
      query {
        series {
          title
          synopses {
            short
            long
          }
          episodes {
            title
            synopses {
              short
              long
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "series" : [
            {
              "title" : "Series 1",
              "synopses" : {
                "short" : "Short series 1",
                "long" : "Long series 1"
              },
              "episodes" : [
                {
                  "title" : "S01E01",
                  "synopses" : {
                    "short" : "Short S01E01",
                    "long" : "Long S01E01"
                  }
                },
                {
                  "title" : "S01E02",
                  "synopses" : {
                    "short" : "Short S01E02",
                    "long" : "Long S01E02"
                  }
                },
                {
                  "title" : "S01E03",
                  "synopses" : {
                    "short" : "Short S01E03",
                    "long" : "Long S01E03"
                  }
                }
              ]
            },
            {
              "title" : "Series 2",
              "synopses" : {
                "short" : "Short series 2",
                "long" : "Long series 2"
              },
              "episodes" : [
                {
                  "title" : "S02E01",
                  "synopses" : {
                    "short" : "Short S02E01",
                    "long" : "Long S02E01"
                  }
                },
                {
                  "title" : "S02E02",
                  "synopses" : {
                    "short" : "Short S02E02",
                    "long" : "Long S02E02"
                  }
                },
                {
                  "title" : "S02E03",
                  "synopses" : {
                    "short" : "Short S02E03",
                    "long" : "Long S02E03"
                  }
                }
              ]
            },
            {
              "title" : "Series 3",
              "synopses" : {
                "short" : "Short series 3",
                "long" : "Long series 3"
              },
              "episodes" : [
                {
                  "title" : "S03E01",
                  "synopses" : {
                    "short" : "Short S03E01",
                    "long" : "Long S03E01"
                  }
                },
                {
                  "title" : "S03E02",
                  "synopses" : {
                    "short" : "Short S03E02",
                    "long" : "Long S03E02"
                  }
                },
                {
                  "title" : "S03E03",
                  "synopses" : {
                    "short" : "Short S03E03",
                    "long" : "Long S03E03"
                  }
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
