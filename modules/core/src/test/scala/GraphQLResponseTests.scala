// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import io.circe.{Json, JsonObject}

object GraphQLResponseTests {
  def assertWeaklyEqual(x: Json, y: Json, strictPaths: List[List[String]] = Nil): Unit =
    assert(weaklyEqual(x, y, strictPaths))

  def assertNoErrors(x: Json): Unit =
    assert(noErrors(x))

  def hasField(x: Json, fieldName: String): Boolean =
    (for {
      x0 <- x.asObject
      _  <- x0(fieldName)
    } yield true).getOrElse(false)

  def errorsOf(x: Json): Seq[Json] =
    (for {
      x0          <- x.asObject
      errorsField <- x0("errors")
      errors      <- errorsField.asArray
    } yield errors).getOrElse(Nil)

  def noErrors(x: Json): Boolean =
    hasField(x, "data") && !hasField(x, "errors") || errorsOf(x).isEmpty

  def weaklyEqual(x: Json, y: Json, strictPaths: List[List[String]] = Nil): Boolean = {
    def cmpObject(x: JsonObject, y: JsonObject, strictPaths: List[List[String]], path: List[String]): Boolean = {
      val xkeys = x.keys
      val ykeys = y.keys
      xkeys.sizeCompare(ykeys) == 0 && {
        xkeys.forall { k =>
          lazy val path0 = path ++ List(k)
          lazy val strictPaths0 = strictPaths.collect {
            case hd :: tl if hd == k => tl
          }
          (x(k), y(k)) match {
            case (Some(vx), Some(vy)) => cmpJson(vx, vy, strictPaths0, path0)
            case _ => false
          }
        }
      }
    }

    def cmpArray(xs: Vector[Json], ys: Vector[Json], strictPaths: List[List[String]], path: List[String]): Boolean = {
      xs.sizeCompare(ys) == 0 && {
        if (strictPaths.contains(path))
          xs.zip(ys).forall { case (x, y) => cmpJson(x, y, strictPaths, path) }
        else {
          def loop(xs: Iterator[Json], ys: Vector[Json]): Boolean = {
            if (!xs.hasNext) ys.isEmpty
            else {
              val x = xs.next()
              ys.indexWhere(y => cmpJson(x, y, strictPaths, path)) match {
                case -1 => false
                case n =>
                  val (py, sy) = ys.splitAt(n)
                  loop(xs, py ++ sy.tail)
              }
            }
          }
          loop(xs.iterator, ys)
        }
      }
    }

    def cmpJson(x: Json, y: Json, strictPaths: List[List[String]], path: List[String]): Boolean = {
      if (x.isObject && y.isObject)
        (for {
          x0 <- x.asObject
          y0 <- y.asObject
        } yield cmpObject(x0, y0, strictPaths, path)).getOrElse(false)
      else if(x.isArray && y.isArray)
        (for {
          x0 <- x.asArray
          y0 <- y.asArray
        } yield cmpArray(x0, y0, strictPaths, path)).getOrElse(false)
      else x == y
    }

    cmpJson(x, y, strictPaths, Nil)
  }
}

