// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini

import cats.data.IorNec
import io.circe.Json
import cats.data.Ior
import cats.data.NonEmptyChain

package object grackle {
  /**
   * A result value.
   *
   * A result of type `T`, a non-empty collection of errors encoded as
   * Json, or both.
   */
  type Result[+T] = IorNec[Json, T]

  object Result {

    val unit: Result[Unit] =
      apply(())

    def apply[A](a: A): Result[A] = Ior.right(a)

    def failure[A](s: String): Result[A] =
      failure(Json.fromString(s))

    def failure[A](j: Json): Result[A] =
      Ior.left(NonEmptyChain(j))

  }

}
