// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import _root_.skunk.{ Encoder, AppliedFragment }
import _root_.skunk.implicits._
import cats._
import cats.syntax.all._

/** Module of `AppliedFragment` constructors. */
object Fragments {

  /** Returns `f IN (fs0, fs1, ...)`. */
  def in[F[_]: Reducible, A](f: AppliedFragment, fs: F[A], enc: Encoder[A]): AppliedFragment =
    fs.toList.map(sql"$enc".apply).foldSmash(f |+| void" IN (", void", ", void")")

  // /** Returns `f NOT IN (fs0, fs1, ...)`. */
  // def notIn[F[_]: Reducible, A: util.Put](f: AppliedFragment, fs: F[A]): AppliedFragment =
  //   fs.toList.map(a => fr0"$a").foldSmash1(f ++ fr0"NOT IN (", fr",", fr")")

  /** Returns `(f1) AND (f2) AND ... (fn)`. */
  def and(fs: AppliedFragment*): AppliedFragment =
    fs.toList.map(parentheses).intercalate(void" AND ")

  /** Returns `(f1) AND (f2) AND ... (fn)` for all defined fragments. */
  def andOpt(fs: Option[AppliedFragment]*): AppliedFragment =
    and(fs.toList.unite: _*)

  /** Returns `(f1) OR (f2) OR ... (fn)`. */
  def or(fs: AppliedFragment*): AppliedFragment =
    fs.toList.map(parentheses).intercalate(void" OR ")

  /** Returns `(f1) OR (f2) OR ... (fn)` for all defined fragments. */
  def orOpt(fs: Option[AppliedFragment]*): AppliedFragment =
    or(fs.toList.unite: _*)

  /** Returns `WHERE (f1) AND (f2) AND ... (fn)` or the empty fragment if `fs` is empty. */
  def whereAnd(fs: AppliedFragment*): AppliedFragment =
    if (fs.isEmpty) AppliedFragment.empty else void"WHERE " |+| and(fs: _*)

  /** Returns `WHERE (f1) AND (f2) AND ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
  def whereAndOpt(fs: Option[AppliedFragment]*): AppliedFragment =
    whereAnd(fs.toList.unite: _*)

  // /** Returns `WHERE (f1) OR (f2) OR ... (fn)` or the empty fragment if `fs` is empty. */
  // def whereOr(fs: AppliedFragment*): AppliedFragment =
  //   if (fs.isEmpty) AppliedFragment.empty else fr"WHERE" ++ or(fs: _*)

  // /** Returns `WHERE (f1) OR (f2) OR ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
  // def whereOrOpt(fs: Option[AppliedFragment]*): AppliedFragment =
  //   whereOr(fs.toList.unite: _*)

  // /** Returns `SET f1, f2, ... fn` or the empty fragment if `fs` is empty. */
  // def set(fs: AppliedFragment*): AppliedFragment =
  //   if (fs.isEmpty) AppliedFragment.empty else fr"SET" ++ fs.toList.intercalate(fr",")

  // /** Returns `SET f1, f2, ... fn` for defined `f`, if any, otherwise the empty fragment. */
  // def setOpt(fs: Option[AppliedFragment]*): AppliedFragment =
  //   set(fs.toList.unite: _*)

  /** Returns `(f)`. */
  def parentheses(f: AppliedFragment): AppliedFragment = void"(" |+| f |+| void")"

  // /** Returns `?,?,...,?` for the values in `a`. */
  // def values[A](a: A)(implicit w: util.Write[A]): AppliedFragment =
  //   w.toFragment(a)

}
