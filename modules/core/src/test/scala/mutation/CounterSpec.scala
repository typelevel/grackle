package counter

import edu.gemini.grackle._
import cats.effect.IO
import edu.gemini.grackle.Schema
import org.scalatest.funsuite.AnyFunSuite
import io.circe.literal.JsonStringContext
import fs2.Stream
import fs2.concurrent.SignallingRef
import scala.concurrent.ExecutionContext
import cats.effect.Timer
import scala.concurrent.duration._
import cats.Monad
import cats.syntax.all._
import cats.effect.Concurrent

class CounterMapping[F[_]: Monad: Timer](sig: SignallingRef[F, Int]) extends ValueMapping[Stream[F, *]] {

  val schema: Schema =
    Schema("""
      type Query {
        get: Int!
      }
      type Mutation {
        inc: Int!
      }
      type Subscription {
        sig: Int!
      }
    """).right.get

  val QueryType        = schema.queryType
  val MutationType     = schema.mutationType.get
  val SubscriptionType = schema.subscriptionType.get

  val typeMappings: List[TypeMapping] =
    List(
      ObjectMapping(
        QueryType,
        List(
          ValueComputedRoot("get", Stream.eval(sig.get)),
        )
      ),
      ObjectMapping(
        MutationType,
        List(
          // This should really be a topic+ref because Signal drops values if we do things too fast
          ValueComputedRoot("inc",  Stream.eval(Timer[F].sleep(100.milli) >> sig.updateAndGet(_ + 1)))
        )
      ),
      ObjectMapping(
        SubscriptionType,
        List(
          ValueComputedRoot("sig",  sig.discrete)
        )
      )
    )

}

object CounterMapping {

  def newInstance[F[_]: Concurrent: Timer]: F[CounterMapping[F]] =
    SignallingRef[F, Int](0).map { sig =>
      new CounterMapping(sig)
    }
}

final class CounterSpec extends AnyFunSuite {

  implicit val cs = IO.contextShift(ExecutionContext.global)
  implicit val ti = IO.timer(ExecutionContext.global)

  // This just tests that the effectful read works.
  test("initial get should be 0") {
    val prog = Stream.eval(CounterMapping.newInstance[IO]).flatMap { cm =>
      cm.compileAndRun("query { get }")
    }
    assert(prog.compile.toList.unsafeRunSync() == List(
      json"""{ "data" : { "get" : 0 } }""")
    )
  }

  // This just tests that the effectful update works.
  test("inc *> inc *> get should yield 1, 2, then 2") {
    val prog = Stream.eval(CounterMapping.newInstance[IO]).flatMap { cm =>
      cm.compileAndRun("mutation { inc }") ++
      cm.compileAndRun("mutation { inc }") ++
      cm.compileAndRun("query { get }")
    }
    assert(prog.compile.toList.unsafeRunSync() == List(
      json"""{ "data" : { "inc" : 1 } }""",
      json"""{ "data" : { "inc" : 2 } }""",
      json"""{ "data" : { "get" : 2 } }""",
    ))
  }

  // And this really is a stream that re-evaluates the same query many times, based on an async
  // trigger. It's cool that we get this for free!
  test("sig.take(5) with concurrent incs should yield 0, 1, 2, 3, 4") {
    val prog = CounterMapping.newInstance[IO].flatMap { cm =>
      cm.compileAndRun("subscription { sig }")
        .take(5)
        .concurrently(cm.compileAndRun("mutation { inc }").repeatN(10))
        .compile
        .toList
    }
    assert(prog.unsafeRunSync() == List(
      json"""{ "data" : { "sig" : 0 } }""",
      json"""{ "data" : { "sig" : 1 } }""",
      json"""{ "data" : { "sig" : 2 } }""",
      json"""{ "data" : { "sig" : 3 } }""",
      json"""{ "data" : { "sig" : 4 } }""",
    ))
  }

}
