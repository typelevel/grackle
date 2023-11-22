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

package grackle.docs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object Output {
  def snip(path: String, tag: String): String = {
    val header = "```scala"
    val footer = "```"

    val prg =
      for {
        txt <- IO.blocking(Files.readString(Path.of(path), StandardCharsets.UTF_8))
      } yield {
        val tagged = txt.split("\n").dropWhile(!_.contains(tag)).drop(1).takeWhile(!_.contains(tag)).mkString("\n")
        s"$header\n$tagged\n$footer"
      }

    prg.unsafeRunSync()
  }

  def header(variant: String): String = {
    variant match {
      case "repo" =>
        val prg = IO.blocking(Files.readString(Path.of("header.md"), StandardCharsets.UTF_8))
        prg.unsafeRunSync()
      case "tutorial" =>
        "# Grackle"
      case _ => ""
    }
  }
}
