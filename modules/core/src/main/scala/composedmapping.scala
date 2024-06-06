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

import cats.MonadThrow

import Cursor.AbstractCursor
import syntax._

abstract class ComposedMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] {
  override def mkCursorForMappedField(parent: Cursor, fieldContext: Context, fm: FieldMapping): Result[Cursor] =
    ComposedCursor(fieldContext, parent.env).success

  case class ComposedCursor(context: Context, env: Env) extends AbstractCursor {
    val focus = null
    val parent = None

    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }
}
