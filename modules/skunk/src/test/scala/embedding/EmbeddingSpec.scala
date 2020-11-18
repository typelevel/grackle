// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import utils.DatabaseSuite
import grackle.test.SqlEmbeddingSpec

final class EmbeddingSpec extends DatabaseSuite with SqlEmbeddingSpec {
  lazy val mapping = EmbeddingMapping.mkMapping(pool)
}
