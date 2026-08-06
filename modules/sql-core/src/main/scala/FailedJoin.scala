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

package grackle.sql

/**
 * A sentinel value representing the empty column values from a failed join.
 */
case object FailedJoin {

  /**
   * Cheap equality check for the `FailedJoin` sentinel.
   *
   * Column values are `Any` and may be a `scala.math.BigDecimal`, whose `equals` throws and
   * catches an `ArithmeticException` whenever compared to a value of another type (see
   * `BigDecimal.isValidLong`). `a == b` calls `a.equals(b)`, so keeping `FailedJoin` on the
   * left runs the singleton's own cheap `equals` instead of the column value's -- do not swap
   * the operands here.
   */
  def isFailedJoin(v: Any): Boolean = FailedJoin == v
}
