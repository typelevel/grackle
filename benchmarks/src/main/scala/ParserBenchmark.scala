package edu.gemini.grackle.benchmarks

import edu.gemini.grackle.Schema
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit
import scala.io.Source

/**
 * To do comparative benchmarks between versions:
 *
 *     benchmarks/run-benchmark ParserBenchmark
 *
 * This will generate results in `benchmarks/results`.
 *
 * Or to run the benchmark from within sbt:
 *
 *     jmh:run -i 10 -wi 10 -f 2 -t 1 edu.gemini.grackle.benchmarks.ParserBenchmark
 *
 * Which means "10 iterations", "10 warm-up iterations", "2 forks", "1 thread".
 * Please note that benchmarks should be usually executed at least in
 * 10 iterations (as a rule of thumb), but more is better.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ParserBenchmark {

  @Param(Array("100"))
  var size: Int = _

  val schema = Source.fromResource("github.graphql").mkString

  @Benchmark
  def parseSchema(blackhole: Blackhole) = {
    for (_ <- 0 to size) {
      val parsed = Schema(schema)
      blackhole.consume(parsed)
    }
  }

}

