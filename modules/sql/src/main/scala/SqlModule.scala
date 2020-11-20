// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import cats.{ Monoid, Reducible }

/** These are the bits that are specific to the underlying database layer. */
trait SqlModule[F[_]] {

  def monitor: SqlMonitor[F, Fragment]

  /** The type of a codec that reads and writes column values of type `A`. */
  type Codec[A]

  /** The type of an encoder that writes column values of type `A`. */
  type Encoder[A]

  /** Extract an encoder from a codec. */
  def toEncoder[A](c: Codec[A]): Encoder[A]

  /** Typeclass for SQL fragments. */
  trait SqlFragment[T] extends Monoid[T] {

    def bind[A](encoder: Encoder[A], nullable: Boolean, value: A): T

    def const(s: String): T

    /** Returns `(f1) AND (f2) AND ... (fn)` for all defined fragments. */
    def andOpt(fs: Option[T]*): T

    /** Returns `(f1) OR (f2) OR ... (fn)` for all defined fragments. */
    def orOpt(fs: Option[T]*): T

    /** Returns `WHERE (f1) AND (f2) AND ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
    def whereAndOpt(fs: Option[T]*): T

    def in[G[_]: Reducible, A](f: T, fs: G[A], enc: Encoder[A]): T

  }

  /** The type of a fragment of SQL together with any interpolated arguments. */
  type Fragment

  implicit def Fragments: SqlFragment[Fragment]

  def intEncoder:     Encoder[Int]
  def stringEncoder:  Encoder[String]
  def booleanEncoder: Encoder[Boolean]
  def doubleEncoder:  Encoder[Double]

  def fetch(fragment: Fragment, metas: List[(Boolean, (Codec[_], NullabilityKnown))]): F[Table]

}
