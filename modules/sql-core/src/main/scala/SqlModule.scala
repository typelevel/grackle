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

package grackle
package sql

import cats.{Monoid, Reducible}

/** These are the bits that are specific to the underlying database layer. */
trait SqlModule[F[_]] {

  def monitor: SqlMonitor[F, Fragment]

  /** The type of a codec that reads and writes column values of type `A`. */
  type Codec

  /** The type of an encoder that writes column values of type `A`. */
  type Encoder

  /** Extract an encoder from a codec. */
  def toEncoder(c: Codec): Encoder

  def isNullable(c: Codec): Boolean

  /** Typeclass for SQL fragments. */
  trait SqlFragment[T] extends Monoid[T] {

    def bind[A](encoder: Encoder, value: A): T

    def const(s: String): T

    /** Returns `(f1) AND (f2) AND ... (fn)` for all fragments. */
    def and(fs: T*): T

    /** Returns `(f1) AND (f2) AND ... (fn)` for all defined fragments. */
    def andOpt(fs: Option[T]*): T

    /** Returns `(f1) OR (f2) OR ... (fn)` for all defined fragments. */
    def orOpt(fs: Option[T]*): T

    /** Returns `WHERE (f1) AND (f2) AND ... (fn)` or the empty fragment if `fs` is empty. */
    def whereAnd(fs: T*): T

    /** Returns `WHERE (f1) AND (f2) AND ... (fn)` for defined `f`, if any, otherwise the empty fragment. */
    def whereAndOpt(fs: Option[T]*): T

    def in[G[_]: Reducible, A](f: T, fs: G[A], enc: Encoder): T

    def parentheses(f: T): T

    def needsCollation(codec: Codec): Boolean

    def sqlTypeName(codec: Codec): Option[String]
  }

  /** The type of a fragment of SQL together with any interpolated arguments. */
  type Fragment

  implicit def Fragments: SqlFragment[Fragment]

  def intEncoder:     Encoder
  def stringEncoder:  Encoder
  def booleanEncoder: Encoder
  def doubleEncoder:  Encoder

  def intCodec: Codec

  def fetch(fragment: Fragment, codecs: List[(Boolean, Codec)]): F[Vector[Array[Any]]]
}
