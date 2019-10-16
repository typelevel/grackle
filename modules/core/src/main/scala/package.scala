// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini

import cats.data.Ior
import io.circe.Json

package object grackle {
  type Result[T] = Ior[List[Json], T]
}
