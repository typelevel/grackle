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

package grackle.doobie.postgres

import cats.effect.Sync
import _root_.doobie.Transactor

import grackle.Mapping
import grackle.doobie._
import grackle.sqlpg._

abstract class DoobiePgMapping[F[_]](
  val transactor: Transactor[F],
  val monitor:    DoobieMonitor[F],
)(
  implicit val M: Sync[F]
) extends Mapping[F] with DoobiePgMappingLike[F]

trait DoobiePgMappingLike[F[_]] extends DoobieMappingLike[F] with SqlPgMappingLike[F]
