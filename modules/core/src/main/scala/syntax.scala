// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle

object syntax extends VersionSpecificSyntax {
  implicit class ResultIdOps[A](val a: A) extends AnyVal {
    def success: Result[A] = Result.success(a)
  }

  implicit class ResultOptionOps[T](val opt: Option[T]) extends AnyVal {
    def toResult(ifNone: => Problem)(implicit dummy: DummyImplicit): Result[T] =
      opt.fold(Result.failure[T](ifNone))(Result.success)

    def toResult(ifNone: => String): Result[T] =
      opt.fold(Result.failure[T](ifNone))(Result.success)

    def toResultOrError(ifNone: => Throwable)(implicit dummy: DummyImplicit): Result[T] =
      opt.fold(Result.internalError[T](ifNone))(Result.success)

    def toResultOrError(ifNone: => String): Result[T] =
      opt.fold(Result.internalError[T](new Throwable(ifNone)))(Result.success)
  }
}
