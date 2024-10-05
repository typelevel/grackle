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

import QueryInterpreter.ProtoJson

/** Monitor for a `SqlMapping` in `F` with fragments of type `A`. */
trait SqlMonitor[F[_], A] {
  def queryMapped(query: Query, fragment: A, rows: Int, cols: Int): F[Unit]
  def resultComputed(result: Result[ProtoJson]): F[Unit]
}
