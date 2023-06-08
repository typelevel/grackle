// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle.QueryExecutor

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlProjectionSuite extends CatsEffectSuite {
  def mapping: QueryExecutor[IO, Json]

  test("base query") {
    val query = """
      query {
        level0 {
          id
          level1 {
            id
            level2 {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "00",
              "level1" : [
                {
                  "id" : "10",
                  "level2" : [
                    {
                      "id" : "20",
                      "attr" : false
                    },
                    {
                      "id" : "21",
                      "attr" : false
                    }
                  ]
                },
                {
                  "id" : "11",
                  "level2" : [
                    {
                      "id" : "22",
                      "attr" : false
                    },
                    {
                      "id" : "23",
                      "attr" : false
                    }
                  ]
                }
              ]
            },
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    },
                    {
                      "id" : "25",
                      "attr" : false
                    }
                  ]
                },
                {
                  "id" : "13",
                  "level2" : [
                    {
                      "id" : "26",
                      "attr" : false
                    },
                    {
                      "id" : "27",
                      "attr" : false
                    }
                  ]
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

  test("simple projected queries") {
    val query = """
      query {
        level0(filter: { attr: true }) {
          id
        }
        level1(filter: { attr: true }) {
          id
        }
        level2(filter: { attr: true }) {
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01"
            }
          ],
          "level1" : [
            {
              "id" : "12"
            }
          ],
          "level2" : [
            {
              "id" : "24"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("projected query with nested query") {
    val query = """
      query {
        level0(filter: { attr: true }) {
          id
          level1 {
            id
            level2 {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    },
                    {
                      "id" : "25",
                      "attr" : false
                    }
                  ]
                },
                {
                  "id" : "13",
                  "level2" : [
                    {
                      "id" : "26",
                      "attr" : false
                    },
                    {
                      "id" : "27",
                      "attr" : false
                    }
                  ]
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

  test("projected query with nested filtered queries (0)") {
    val query = """
      query {
        level0(filter: { attr: true }) {
          id
          level1(filter: { attr: true }) {
            id
            level2(filter: { attr: true }) {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    }
                  ]
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

  test("projected query with nested filtered queries (1)") {
    val query = """
      query {
        level0(filter: { attr: true }) {
          id
          level1 {
            id
            level2(filter: { attr: true }) {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    }
                  ]
                },
                {
                  "id" : "13",
                  "level2" : [
                  ]
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

  test("projected query with nested filtered queries (2)") {
    val query = """
      query {
        level0 {
          id
          level1(filter: { attr: true }) {
            id
            level2(filter: { attr: true }) {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    }
                  ]
                }
              ]
            },
            {
              "id" : "00",
              "level1" : [
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("projected query with nested filtered queries (3)") {
    val query = """
      query {
        level0 {
          id
          level1 {
            id
            level2(filter: { attr: true }) {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "00",
              "level1" : [
                {
                  "id" : "10",
                  "level2" : [
                  ]
                },
                {
                  "id" : "11",
                  "level2" : [
                  ]
                }
              ]
            },
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    }
                  ]
                },
                {
                  "id" : "13",
                  "level2" : [
                  ]
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

  test("projected query with nested filtered queries (4)") {
    val query = """
      query {
        level1 {
          id
          level2(filter: { attr: true }) {
            id
            attr
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level1" : [
            {
              "id" : "12",
              "level2" : [
                {
                  "id" : "24",
                  "attr" : true
                }
              ]
            },
            {
              "id" : "10",
              "level2" : [
              ]
            },
            {
              "id" : "11",
              "level2" : [
              ]
            },
            {
              "id" : "13",
              "level2" : [
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
