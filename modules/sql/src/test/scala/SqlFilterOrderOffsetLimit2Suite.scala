// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlFilterOrderOffsetLimit2Suite extends CatsEffectSuite {
  def mapping: QueryExecutor[IO, Json]

  test("base query") {
    val query = """
      query {
        root {
          id
          containers {
            id
            listA {
              id
            }
            listB {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "containers" : [
                {
                  "id" : "c0",
                  "listA" : [
                    {
                      "id" : "a00"
                    },
                    {
                      "id" : "a01"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b00"
                    },
                    {
                      "id" : "b01"
                    }
                  ]
                },
                {
                  "id" : "c1",
                  "listA" : [
                    {
                      "id" : "a10"
                    },
                    {
                      "id" : "a11"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b10"
                    },
                    {
                      "id" : "b11"
                    }
                  ]
                }
              ]
            },
            {
              "id" : "r1",
              "containers" : [
                {
                  "id" : "c2",
                  "listA" : [
                    {
                      "id" : "a20"
                    },
                    {
                      "id" : "a21"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b20"
                    },
                    {
                      "id" : "b21"
                    }
                  ]
                },
                {
                  "id" : "c3",
                  "listA" : [
                    {
                      "id" : "a30"
                    },
                    {
                      "id" : "a31"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b30"
                    },
                    {
                      "id" : "b31"
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

  test("top level limit") {
    val query = """
      query {
        containers(limit: 1) {
          id
          listA {
            id
          }
          listB {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "containers" : [
            {
              "id" : "c0",
              "listA" : [
                {
                  "id" : "a00"
                },
                {
                  "id" : "a01"
                }
              ],
              "listB" : [
                {
                  "id" : "b00"
                },
                {
                  "id" : "b01"
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

  test("nested limit (1)") {
    val query = """
      query {
        root {
          id
          containers(limit: 1) {
            id
            listA {
              id
            }
            listB {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "containers" : [
                {
                  "id" : "c0",
                  "listA" : [
                    {
                      "id" : "a00"
                    },
                    {
                      "id" : "a01"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b00"
                    },
                    {
                      "id" : "b01"
                    }
                  ]
                }
              ]
            },
            {
              "id" : "r1",
              "containers" : [
                {
                  "id" : "c2",
                  "listA" : [
                    {
                      "id" : "a20"
                    },
                    {
                      "id" : "a21"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b20"
                    },
                    {
                      "id" : "b21"
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

  test("nested limit (2)") {
    val query = """
      query {
        root {
          id
          containers {
            id
            listA(limit: 1) {
              id
            }
            listB(limit: 1) {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "containers" : [
                {
                  "id" : "c0",
                  "listA" : [
                    {
                      "id" : "a00"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b00"
                    }
                  ]
                },
                {
                  "id" : "c1",
                  "listA" : [
                    {
                      "id" : "a10"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b10"
                    }
                  ]
                }
              ]
            },
            {
              "id" : "r1",
              "containers" : [
                {
                  "id" : "c2",
                  "listA" : [
                    {
                      "id" : "a20"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b20"
                    }
                  ]
                },
                {
                  "id" : "c3",
                  "listA" : [
                    {
                      "id" : "a30"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b30"
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

  test("nested limit (3)") {
    val query = """
      query {
        root {
          id
          containers(limit: 1) {
            id
            listA(limit: 1) {
              id
            }
            listB(limit: 1) {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "containers" : [
                {
                  "id" : "c0",
                  "listA" : [
                    {
                      "id" : "a00"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b00"
                    }
                  ]
                }
              ]
            },
            {
              "id" : "r1",
              "containers" : [
                {
                  "id" : "c2",
                  "listA" : [
                    {
                      "id" : "a20"
                    }
                  ],
                  "listB" : [
                    {
                      "id" : "b20"
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

  test("nested limit (4)") {
    val query = """
      query {
        root {
          id
          containers(limit: 1) {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "containers" : [
                {
                  "id" : "c0"
                }
              ]
            },
            {
              "id" : "r1",
              "containers" : [
                {
                  "id" : "c2"
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

  test("multi join base query") {
    val query = """
      query {
        root {
          id
          listA {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "listA" : [
                {
                  "id" : "a00"
                },
                {
                  "id" : "a01"
                },
                {
                  "id" : "a10"
                },
                {
                  "id" : "a11"
                }
              ]
            },
            {
              "id" : "r1",
              "listA" : [
                {
                  "id" : "a20"
                },
                {
                  "id" : "a21"
                },
                {
                  "id" : "a30"
                },
                {
                  "id" : "a31"
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

  test("multi join root limit") {
    val query = """
      query {
        root(limit: 1) {
          id
          listA {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "listA" : [
                {
                  "id" : "a00"
                },
                {
                  "id" : "a01"
                },
                {
                  "id" : "a10"
                },
                {
                  "id" : "a11"
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

  test("multi join nested limit (1)") {
    val query = """
      query {
        root {
          id
          listA(limit: 1) {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "listA" : [
                {
                  "id" : "a00"
                }
              ]
            },
            {
              "id" : "r1",
              "listA" : [
                {
                  "id" : "a20"
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

  test("multi join nested limit (2)") {
    val query = """
      query {
        root(limit: 1) {
          id
          listA(limit: 1) {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "id" : "r0",
              "listA" : [
                {
                  "id" : "a00"
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
