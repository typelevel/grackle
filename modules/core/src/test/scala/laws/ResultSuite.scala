// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package laws

import cats.Eq
import cats.data.NonEmptyChain
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import cats.laws.discipline.{ApplicativeTests, MonadErrorTests, ParallelTests, TraverseTests}
import cats.laws.discipline.arbitrary._
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

import edu.gemini.grackle.{Problem, Result}

class ResultSuite extends CatsSuite {
  implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals

  implicit val grackleLawsArbitraryForProblem: Arbitrary[Problem] =
    Arbitrary(getArbitrary[String].map(Problem(_)))

  implicit val grackleLawsCogenForProblem: Cogen[Problem] =
    Cogen[String].contramap(_.message)

  implicit def grackleArbitraryFnForResult[T](implicit arbF: Arbitrary[T => T]): Arbitrary[Result[T] => Result[T]] =
    Arbitrary(arbF.arbitrary.map(f => (r: Result[T]) => r.map(f)))

  implicit def grackleLawsArbitraryForResult[T](implicit T: Arbitrary[T]): Arbitrary[Result[T]] =
    Arbitrary(
      Gen.oneOf(
        T.arbitrary.map(Result.Success(_)),
        for {
          ps <- getArbitrary[NonEmptyChain[Problem]]
          t  <- T.arbitrary
        } yield Result.Warning(ps, t),
        getArbitrary[NonEmptyChain[Problem]].map(Result.Failure(_)),
        getArbitrary[Throwable].map(Result.InternalError(_))
      )
    )

  checkAll("MonadError[Result] @ Int", MonadErrorTests[Result, Either[Throwable, NonEmptyChain[Problem]]].monadError[Int, Int, Int])

  checkAll("Traverse[Result] @ Int with Option", TraverseTests[Result].traverse[Int, Int, Int, Int, Option, Option])

  checkAll("Parallel[Result] @ Int", ParallelTests[Result].parallel[Either[Throwable, NonEmptyChain[Problem]], Int])

  checkAll("Semigroup[Result[List[T: Semigroup]]]", SemigroupTests[Result[List[Int]]].semigroup)

  checkAll("Eq[Result[Int]]", EqTests[Result[Int]].eqv)

  checkAll("Applicative[ResultT] @ Int", ApplicativeTests[Result].applicative[Int, Int, Int])
}
