// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import PartialFunction.condOpt

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Query._
import QueryCompiler._
import Value._

final class CascadeSuite extends CatsEffectSuite {
  test("elaboration of simple query") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded {
            foo
            bar
            fooBar
            limit
          }
        }
      }
    """

    val expected =
      Environment(
        Env.NonEmptyEnv(Map("filter" -> CascadeMapping.CascadedFilter(Some("foo"), None, Some(23), Some(10)))),
        Select("foo",
          Select("cascaded",
            Group(
              List(
                Select("foo"),
                Select("bar"),
                Select("fooBar"),
                Select("limit")
              )
            )
          )
        )
      )

    val res = CascadeMapping.compiler.compile(query)

    assertEquals(res.map(_.query), Result.Success(expected))
  }

  test("simple query") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded {
            foo
            bar
            fooBar
            limit
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo",
              "bar" : null,
              "fooBar" : 23,
              "limit" : 10
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("simple cascade (1)") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded { foo bar fooBar limit }
          bar {
            cascaded { foo bar fooBar limit }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo",
              "bar" : null,
              "fooBar" : 23,
              "limit" : 10
            },
            "bar" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : null,
                "fooBar" : 23,
                "limit" : null
              }
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("simple cascade (2)") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded { foo bar fooBar limit }
          bar {
            cascaded { foo bar fooBar limit }
            foo {
              cascaded { foo bar fooBar limit }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo",
              "bar" : null,
              "fooBar" : 23,
              "limit" : 10
            },
            "bar" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : null,
                "fooBar" : 23,
                "limit" : null
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo",
                  "bar" : null,
                  "fooBar" : 23,
                  "limit" : null
                }
              }
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("cascade with override (1)") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded { foo bar fooBar limit }
          bar1:bar(filter: { bar: true, fooBar: 13 }, limit: 5) {
            cascaded { foo bar fooBar limit }
            foo {
              cascaded { foo bar fooBar limit }
            }
          }
          bar2:bar(filter: { bar: false, fooBar: 11 }, limit: 7) {
            cascaded { foo bar fooBar limit }
            foo {
              cascaded { foo bar fooBar limit }
            }
          }
          bar3:bar {
            cascaded { foo bar fooBar limit }
            foo {
              cascaded { foo bar fooBar limit }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo",
              "bar" : null,
              "fooBar" : 23,
              "limit" : 10
            },
            "bar1" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : true,
                "fooBar" : 13,
                "limit" : 5
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo",
                  "bar" : true,
                  "fooBar" : 13,
                  "limit" : null
                }
              }
            },
            "bar2" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : false,
                "fooBar" : 11,
                "limit" : 7
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo",
                  "bar" : false,
                  "fooBar" : 11,
                  "limit" : null
                }
              }
            },
            "bar3" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : null,
                "fooBar" : 23,
                "limit" : null
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo",
                  "bar" : null,
                  "fooBar" : 23,
                  "limit" : null
                }
              }
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("cascade with override (2)") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded { foo bar fooBar limit }
          bar1:bar(filter: { bar: true, fooBar: 13 }, limit: 5) {
            cascaded { foo bar fooBar limit }
            foo(filter: { foo: "foo1" }, limit: 3) {
              cascaded { foo bar fooBar limit }
            }
          }
          bar2:bar(filter: { bar: false, fooBar: 11 }, limit: 7) {
            cascaded { foo bar fooBar limit }
            foo(filter: { foo: "foo2" }, limit: 5) {
              cascaded { foo bar fooBar limit }
            }
          }
          bar3:bar {
            cascaded { foo bar fooBar limit }
            foo(filter: { foo: "foo3" }, limit: 2) {
              cascaded { foo bar fooBar limit }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo",
              "bar" : null,
              "fooBar" : 23,
              "limit" : 10
            },
            "bar1" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : true,
                "fooBar" : 13,
                "limit" : 5
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo1",
                  "bar" : true,
                  "fooBar" : 13,
                  "limit" : 3
                }
              }
            },
            "bar2" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : false,
                "fooBar" : 11,
                "limit" : 7
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo2",
                  "bar" : false,
                  "fooBar" : 11,
                  "limit" : 5
                }
              }
            },
            "bar3" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : null,
                "fooBar" : 23,
                "limit" : null
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo3",
                  "bar" : null,
                  "fooBar" : 23,
                  "limit" : 2
                }
              }
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("cascade with reset (1)") {
    val query = """
      query {
        foo(filter: { foo: "foo", fooBar: 23 }, limit: 10) {
          cascaded { foo bar fooBar limit }
          reset {
            bar(filter: { bar: true, fooBar: 13 }, limit: 5) {
              cascaded { foo bar fooBar limit }
              foo {
                cascaded { foo bar fooBar limit }
              }
            }
          }
          bar1:bar(filter: null, limit: 5) {
            cascaded { foo bar fooBar limit }
            foo {
              cascaded { foo bar fooBar limit }
            }
          }
          bar2:bar(filter: { fooBar: null }, limit: 5) {
            cascaded { foo bar fooBar limit }
            foo {
              cascaded { foo bar fooBar limit }
            }
          }
          bar3:bar(limit: 5) {
            cascaded { foo bar fooBar limit }
            foo(filter: { foo : null }, limit: 11) {
              cascaded { foo bar fooBar limit }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo",
              "bar" : null,
              "fooBar" : 23,
              "limit" : 10
            },
            "reset" : {
              "bar" : {
                "cascaded" : {
                  "foo" : null,
                  "bar" : true,
                  "fooBar" : 13,
                  "limit" : 5
                },
                "foo" : {
                  "cascaded" : {
                    "foo" : null,
                    "bar" : true,
                    "fooBar" : 13,
                    "limit" : null
                  }
                }
              }
            },
            "bar1" : {
              "cascaded" : {
                "foo" : null,
                "bar" : null,
                "fooBar" : null,
                "limit" : 5
              },
              "foo" : {
                "cascaded" : {
                  "foo" : null,
                  "bar" : null,
                  "fooBar" : null,
                  "limit" : null
                }
              }
            },
            "bar2" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : null,
                "fooBar" : null,
                "limit" : 5
              },
              "foo" : {
                "cascaded" : {
                  "foo" : "foo",
                  "bar" : null,
                  "fooBar" : null,
                  "limit" : null
                }
              }
            },
            "bar3" : {
              "cascaded" : {
                "foo" : "foo",
                "bar" : null,
                "fooBar" : 23,
                "limit" : 5
              },
              "foo" : {
                "cascaded" : {
                  "foo" : null,
                  "bar" : null,
                  "fooBar" : 23,
                  "limit" : 11
                }
              }
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("repeated cascade with overrides and resets") {
    val query = """
      query {
        foo(filter: { foo: "foo" }) {
          cascaded { foo }
          bar {
            foo {
              cascaded { foo }
              bar {
                foo(filter: { foo: "foo2" }) {
                  cascaded { foo }
                  bar {
                    foo(filter: { foo: null }) {
                      cascaded { foo }
                      bar {
                        foo(filter: { foo: "foo3" }) {
                          cascaded { foo }
                          reset {
                            cascaded { foo }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "cascaded" : {
              "foo" : "foo"
            },
            "bar" : {
              "foo" : {
                "cascaded" : {
                  "foo" : "foo"
                },
                "bar" : {
                  "foo" : {
                    "cascaded" : {
                      "foo" : "foo2"
                    },
                    "bar" : {
                      "foo" : {
                        "cascaded" : {
                          "foo" : null
                        },
                        "bar" : {
                          "foo" : {
                            "cascaded" : {
                              "foo" : "foo3"
                            },
                            "reset" : {
                              "cascaded" : {
                                "foo" : null
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    val res = CascadeMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }
}

object CascadeMapping extends ValueMapping[IO] {
  val schema =
    schema"""
      type Query {
        foo(filter: FooFilter, limit: Int): Foo
      }
      type Foo {
        bar(filter: BarFilter, limit: Int): Bar
        reset: Foo!
        cascaded: FilterValue!
      }
      type Bar {
        cascaded: FilterValue!
        reset: Bar!
        foo(filter: FooFilter, limit: Int): Foo
      }
      input FooFilter {
        foo: String
        fooBar: Int
      }
      input BarFilter {
        bar: Boolean
        fooBar: Int
      }
      type FilterValue {
        foo: String
        bar: Boolean
        fooBar: Int
        limit: Int
      }
    """

  val QueryType = schema.ref("Query")
  val FooType = schema.ref("Foo")
  val BarType = schema.ref("Bar")
  val FilterValueType = schema.ref("FilterValue")

  override val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("foo", _ => Some(()))
          )
      ),
      ValueObjectMapping[Unit](
        tpe = FooType,
        fieldMappings =
          List(
            ValueField("cascaded", identity),
            ValueField("reset", identity),
            ValueField("bar", _ => Some(()))
          )
      ),
      ValueObjectMapping[Unit](
        tpe = BarType,
        fieldMappings =
          List(
            ValueField("cascaded", identity),
            ValueField("reset", identity),
            ValueField("foo", _ => Some(()))
          )
      ),
      ValueObjectMapping[CascadedFilter](
        tpe = FilterValueType,
        fieldMappings =
          List(
            CursorField("foo", getFilterValue(_).map(_.foo)),
            CursorField("bar", getFilterValue(_).map(_.bar)),
            CursorField("fooBar", getFilterValue(_).map(_.fooBar)),
            CursorField("limit", getFilterValue(_).map(_.limit))
          )
      )
    )

  def getFilterValue(c: Cursor): Result[CascadedFilter] =
    c.envR[CascadedFilter]("filter")

  type Tri[T] = Either[Unit, Option[T]]
  def triToOption[T](t: Tri[T]): Option[T] =
    t match {
      case Right(t) => t
      case Left(()) => None
    }

  abstract class TriValue[T](matchValue: Value => Option[T]) {
    def unapply(v: Value): Option[Tri[T]] =
      v match {
        case NullValue => Some(Right(None))
        case AbsentValue => Some(Left(()))
        case _ => matchValue(v).map(t => Right(Some(t)))
      }
  }

  object TriString extends TriValue[String](condOpt(_) { case StringValue(s) => s })
  object TriInt extends TriValue[Int](condOpt(_) { case IntValue(i) => i })
  object TriBoolean extends TriValue[Boolean](condOpt(_) { case BooleanValue(b) => b })

  case class CascadedFilter(foo: Option[String], bar: Option[Boolean], fooBar: Option[Int], limit: Option[Int]) {
    def combine[T](current: Option[T], next: Tri[T]): Option[T] =
      next match {
        case Left(()) => current
        case Right(None) => None
        case Right(Some(t)) => Some(t)
      }

    def cascadeFoo(foo0: Tri[String], fooBar0: Tri[Int]): CascadedFilter =
      copy(
        foo = combine(foo, foo0),
        fooBar = combine(fooBar, fooBar0),
        limit = None
      )

    def cascadeBar(bar0: Tri[Boolean], fooBar0: Tri[Int]): CascadedFilter =
      copy(
        bar = combine(bar, bar0),
        fooBar = combine(fooBar, fooBar0),
        limit = None
      )

    def withLimit(limit0: Tri[Int]): CascadedFilter =
      copy(limit = triToOption(limit0))
  }

  object CascadedFilter {
    def empty: CascadedFilter =
      CascadedFilter(None, None, None, None)
  }

  object FooFilter {
    def unapply(v: Value): Option[CascadedFilter => CascadedFilter] =
      v match {
        case ObjectValue(List(("foo", TriString(foo)), ("fooBar", TriInt(fooBar)))) =>
          Some(_.cascadeFoo(foo, fooBar))
        case NullValue => Some(_ => CascadedFilter.empty)
        case AbsentValue => Some(identity)
        case _ => None
      }
  }

  object BarFilter {
    def unapply(v: Value): Option[CascadedFilter => CascadedFilter] =
      v match {
        case ObjectValue(List(("bar", TriBoolean(bar)), ("fooBar", TriInt(fooBar)))) =>
          Some(_.cascadeBar(bar, fooBar))
        case NullValue => Some(_ => CascadedFilter.empty)
        case AbsentValue => Some(identity)
        case _ => None
      }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType | BarType, "foo", List(Binding("filter", FooFilter(filter)), Binding("limit", TriInt(limit)))) =>
      for {
        current0 <- Elab.env[CascadedFilter]("filter")
        current  =  current0.getOrElse(CascadedFilter.empty)
        _        <- Elab.env("filter", filter(current).withLimit(limit))
      } yield ()

    case (FooType, "bar", List(Binding("filter", BarFilter(filter)), Binding("limit", TriInt(limit)))) =>
      for {
        current0 <- Elab.env[CascadedFilter]("filter")
        current  =  current0.getOrElse(CascadedFilter.empty)
        _        <- Elab.env("filter", filter(current).withLimit(limit))
      } yield ()

    case (FooType | BarType, "reset", Nil) =>
      Elab.env("filter", CascadedFilter.empty)
  }
}
