// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini

import cats.data.IorNec
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.syntax.all._

package object grackle {
  /**
   * A result value.
   *
   * A result of type `T`, a non-empty collection of errors encoded as
   * Json, or both.
   */
  type Result[+T] = IorNec[Problem, T]

  object Result {

    val unit: Result[Unit] =
      apply(())

    def apply[A](a: A): Result[A] = Ior.right(a)

    def failure[A](s: String): Result[A] =
      failure(Problem(s))

    def failure[A](p: Problem): Result[A] =
      Ior.left(NonEmptyChain(p))

    def fromOption[A](oa: Option[A], ifNone: => Problem): Result[A] =
      oa match {
        case Some(a) => Result(a)
        case None    => Result.failure(ifNone)
      }

    def fromEither[A](ea: Either[Problem, A]): Result[A] =
      ea.fold(Result.failure(_), Result.apply)

    def warning[A](warning: Problem, value: A): Result[A] =
      Result.failure[A](warning).putRight(value)

    def fromOption[A](oa: Option[A], ifNone: => String)(implicit ev: DummyImplicit): Result[A] =
      fromOption(oa, Problem(ifNone))

    def fromEither[A](ea: Either[String, A])(implicit ev: DummyImplicit): Result[A] =
      fromEither(ea.leftMap(Problem(_)))

    def warning[A](warning: String, value: A): Result[A] =
      this.warning(Problem(warning), value)

  }

}
