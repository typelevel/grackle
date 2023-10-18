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
