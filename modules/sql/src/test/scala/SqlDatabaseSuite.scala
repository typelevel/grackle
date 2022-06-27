// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.tests.CatsSuite
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import org.scalatest.funsuite.AnyFunSuite
import org.testcontainers.containers.BindMode

trait SqlDatabaseSuite extends AnyFunSuite with CatsSuite with ForAllTestContainer {

  override val container: PostgreSQLContainer = {
    val c = PostgreSQLContainer()
    c.container.withClasspathResourceMapping("db", "/docker-entrypoint-initdb.d/", BindMode.READ_ONLY)
    c
  }
}
