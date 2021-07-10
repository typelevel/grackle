// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package projection

import edu.gemini.grackle.syntax._

import grackle.test.SqlProjectionSpec
import grackle.test.GraphQLResponseTests.assertWeaklyEqual
import utils.DatabaseSuite

final class ProjectionSpec extends DatabaseSuite with SqlProjectionSpec {
  lazy val mapping = ProjectionMapping.fromTransactor(xa)

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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }
}
