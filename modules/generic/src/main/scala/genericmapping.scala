// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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
package generic

import cats.MonadThrow
import org.tpolecat.sourcepos.SourcePos

import syntax._
import Cursor.DeferredCursor

abstract class GenericMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with GenericMappingLike[F]

trait GenericMappingLike[F[_]] extends ScalaVersionSpecificGenericMappingLike[F] {
  def genericCursor[T](path: Path, env: Env, t: T)(implicit cb: => CursorBuilder[T]): Result[Cursor] =
    if(path.isRoot)
      cb.build(Context(path.rootTpe), t, None, env)
    else
      DeferredCursor(path, (context, parent) => cb.build(context, t, Some(parent), env)).success

  override def mkCursorForMappedField(parent: Cursor, fieldContext: Context, fm: FieldMapping): Result[Cursor] =
    fm match {
      case GenericField(_, t, cb, _) =>
        cb().build(fieldContext, t, Some(parent), parent.env)
      case _ =>
        super.mkCursorForMappedField(parent, fieldContext, fm)
    }

  case class GenericField[T](val fieldName: String, t: T, cb: () => CursorBuilder[T], hidden: Boolean)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def subtree: Boolean = true
  }

  def GenericField[T](fieldName: String, t: T, hidden: Boolean = false)(implicit cb: => CursorBuilder[T], pos: SourcePos): GenericField[T] =
    new GenericField(fieldName, t, () => cb, hidden)

  object semiauto {
    final def deriveObjectCursorBuilder[T](tpe: Type)
      (implicit mkBuilder: => MkObjectCursorBuilder[T]): ObjectCursorBuilder[T] = mkBuilder(tpe)
    final def deriveInterfaceCursorBuilder[T](tpe: Type)
      (implicit mkBuilder: => MkInterfaceCursorBuilder[T]): CursorBuilder[T] = mkBuilder(tpe)
  }

  trait ObjectCursorBuilder[T] extends CursorBuilder[T] {
    def renameField(from: String, to: String): ObjectCursorBuilder[T]
    def transformFieldNames(f: String => String): ObjectCursorBuilder[T]
    def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T]
  }
}
