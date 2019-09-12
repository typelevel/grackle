// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

import cats.tests.CatsSuite

final class StarsWarsSpec extends CatsSuite {
  val text = """
    query {
      character(id: 1000) {
        name
      }
    }
  """
}
