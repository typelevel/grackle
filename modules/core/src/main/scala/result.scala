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

import scala.annotation.tailrec
import scala.util.control.NonFatal

import cats.{~>, Applicative, Eq, Eval, Functor, Monad, MonadError, Parallel, Semigroup, Traverse}
import cats.arrow.FunctionK
import cats.data.{Chain, NonEmptyChain}
import cats.implicits._

/**
  * A result value.
  *
  * A result of type `T`, a non-empty collection of errors encoded as
  * Json, or both.
  */
sealed trait Result[+T] {
  def fold[U](failure: NonEmptyChain[Problem] => U, success: T => U, warning: (NonEmptyChain[Problem], T) => U, error: Throwable => U): U =
    this match {
      case Result.Success(value) => success(value)
      case Result.Warning(problems, value) => warning(problems, value)
      case Result.Failure(problems) => failure(problems)
      case Result.InternalError(thr) => error(thr)
    }

  def map[U](f: T => U): Result[U] =
    this match {
      case Result.Success(value) => Result.Success(f(value))
      case Result.Warning(problems, value) => Result.Warning(problems, f(value))
      case other@Result.Failure(_) => other
      case other@Result.InternalError(_) => other
    }

  def flatMap[U](f: T => Result[U]): Result[U] =
    this match {
      case Result.Success(value) => f(value)
      case Result.Warning(problems, value) =>
        f(value) match {
          case Result.Success(fv) => Result.Warning(problems, fv)
          case Result.Warning(fps, fv) => Result.Warning(problems ++ fps, fv)
          case Result.Failure(fps) => Result.Failure(problems ++ fps)
          case other@Result.InternalError(_) => other
        }
      case other@Result.Failure(_) => other
      case other@Result.InternalError(_) => other
    }

  def traverse[F[_], U](f: T => F[U])(implicit F: Applicative[F]): F[Result[U]] =
    this match {
      case Result.Success(value) => F.map(f(value))(Result.Success(_))
      case Result.Warning(problems, value) => F.map(f(value))(Result.Warning(problems, _))
      case other@Result.Failure(_) => F.pure(other)
      case other@Result.InternalError(_) => F.pure(other)
    }

  final def exists(p: T => Boolean): Boolean = toOption.exists(p)

  final def forall(p: T => Boolean): Boolean = toOption.forall(p)

  final def foldLeft[U](u: U)(f: (U, T) => U): U =
    fold(_ => u, f(u, _), (_, t) => f(u, t), _ => u)

  final def foldRight[U](lu: Eval[U])(f: (T, Eval[U]) => Eval[U]): Eval[U] =
    fold(_ => lu, f(_, lu), (_, t) => f(t, lu), _ => lu)

  def toEither: Either[Either[Throwable, NonEmptyChain[Problem]], T] =
    this match {
      case Result.Success(value) => Right(value)
      case Result.Warning(_, value) => Right(value)
      case Result.Failure(problems) => Left(Right(problems))
      case Result.InternalError(thr) => Left(Left(thr))
    }

  def toOption: Option[T] =
    this match {
      case Result.Success(value) => Some(value)
      case Result.Warning(_, value) => Some(value)
      case _ => None
    }

  def withProblems(problems: NonEmptyChain[Problem]): Result[T] =
    this match {
      case Result.Success(value) => Result.Warning(problems, value)
      case Result.Warning(ps, value) => Result.Warning(ps ++ problems, value)
      case Result.Failure(ps) => Result.Failure(ps ++ problems)
      case other@Result.InternalError(_) => other
    }

  def combine[U >: T](that: Result[U])(implicit S: Semigroup[U]): Result[U] =
    this match {
      case Result.Success(t) =>
        that match {
          case Result.Success(u) => Result.Success(S.combine(t, u))
          case Result.Warning(ps2, u) => Result.Warning(ps2, S.combine(t, u))
          case Result.Failure(ps2) => Result.Warning(ps2, t)
          case err@Result.InternalError(_) => err
        }
      case Result.Warning(ps1, t) =>
        that match {
          case Result.Success(u) => Result.Warning(ps1, S.combine(t, u))
          case Result.Warning(ps2, u) => Result.Warning(ps1 ++ ps2, S.combine(t, u))
          case Result.Failure(ps2) => Result.Warning(ps1 ++ ps2, t)
          case err@Result.InternalError(_) => err
        }
      case Result.Failure(ps1) =>
        that match {
          case Result.Success(u) => Result.Warning(ps1, u)
          case Result.Warning(ps2, u) => Result.Warning(ps1 ++ ps2, u)
          case Result.Failure(ps2) => Result.Failure(ps1 ++ ps2)
          case Result.InternalError(thr2) => Result.InternalError(thr2)
        }
      case err@Result.InternalError(_) => err
    }

  def ===[TT >: T](that: Result[TT])(implicit TT: Eq[TT]): Boolean =
    (this, that) match {
      case (Result.Failure(p), Result.Failure(pp))             => p == pp
      case (Result.Success(t), Result.Success(tt))             => TT.eqv(t, tt)
      case (Result.Warning(p, t), Result.Warning(pp, tt))      => (p == pp) && TT.eqv(t, tt)
      case (Result.InternalError(e), Result.InternalError(ee)) => e == ee
      case _                                                   => false
    }

  def getOrElse[U >: T](ifNone: => U): U = toOption.getOrElse(ifNone)

  def hasValue: Boolean =
    this match {
      case Result.Success(_) => true
      case Result.Warning(_, _) => true
      case _ => false
    }

  def hasProblems: Boolean =
    this match {
      case Result.Warning(_, _) => true
      case Result.Failure(_) => true
      case _ => false
    }

  def toProblems: Chain[Problem] =
    this match {
      case Result.Warning(problems, _) => problems.toChain
      case Result.Failure(problems) => problems.toChain
      case _ => Chain.empty
    }

  def isInternalError: Boolean =
    this match {
      case Result.InternalError(_) => true
      case _ => false
    }
}

object Result extends ResultInstances {
  final case class Success[+T](value: T) extends Result[T]
  final case class Warning[+T](problems: NonEmptyChain[Problem], value: T) extends Result[T]
  final case class Failure(problems: NonEmptyChain[Problem]) extends Result[Nothing]
  final case class InternalError(error: Throwable) extends Result[Nothing]

  /** Yields a Success with the given value. */
  def apply[A](a: A): Result[A] = Success(a)

  /** Yields a Success with the given value. */
  def pure[A](a: A): Result[A] = Success(a)

  /** Yields a Success with unit value. */
  val unit: Result[Unit] = pure(())

  /** Yields a Success with the given value. */
  def success[A](a: A): Result[A] = Success(a)

  def warning[A](warning: Problem, value: A): Result[A] =
    Warning(NonEmptyChain(warning), value)

  def warning[A](warning: String, value: A): Result[A] =
    this.warning(Problem(warning), value)

  def failure[A](s: String): Result[A] =
    failure(Problem(s))

  def failure[A](p: Problem): Result[A] =
    Failure(NonEmptyChain(p))

  def internalError[A](err: Throwable): Result[A] =
    InternalError(err)

  def internalError[A](err: String): Result[A] =
    InternalError(new Throwable(err))

  def fromOption[A](oa: Option[A], ifNone: => Problem): Result[A] =
    oa match {
      case Some(a) => Result(a)
      case None    => Result.failure(ifNone)
    }

  def fromOption[A](oa: Option[A], ifNone: => String)(implicit ev: DummyImplicit): Result[A] =
    fromOption(oa, Problem(ifNone))

  def fromEither[A](ea: Either[Problem, A]): Result[A] =
    ea.fold(Result.failure(_), Result.apply)

  def fromEither[A](ea: Either[String, A])(implicit ev: DummyImplicit): Result[A] =
    fromEither(ea.leftMap(Problem(_)))

  def fromProblems(problems: Seq[Problem]): Result[Unit] =
    NonEmptyChain.fromSeq(problems).map(Result.Failure(_)).getOrElse(Result.unit)

  def fromProblems(problems: Chain[Problem]): Result[Unit] =
    NonEmptyChain.fromChain(problems).map(Result.Failure(_)).getOrElse(Result.unit)

  def catchNonFatal[T](body: => T): Result[T] =
    try {
      success(body)
    } catch {
      case t if NonFatal(t) => internalError(t)
    }

  // Combine a list of results, collecting all problems and preserving the order and
  // number of elements by inserting default values for failures. If the argument list
  // contains any internal errors the result will be the first of these.
  def combineAllWithDefault[T](ress: List[Result[T]], default: => T): Result[List[T]] = {
    lazy val default0 = default
    (ress.foldLeft(Right((Chain.empty, Nil)): Either[Throwable, (Chain[Problem], List[T])]) {
      case (err@Left(_), _) => err
      case (_, Result.InternalError(err)) => Left(err)
      case (Right((ps, ts)), elem) =>
        elem match {
          case Result.Success(t) => Right((ps, t :: ts))
          case Result.Failure(ps0) => Right((ps ++ ps0.toChain, default0 :: ts))
          case Result.Warning(ps0, t) => Right((ps ++ ps0.toChain, t :: ts))
          case Result.InternalError(err) => Left(err)
        }
    }) match {
      case Left(err) => Result.internalError(err)
      case Right((ps, ts)) =>
        val ts0 = ts.reverse
        NonEmptyChain.fromChain(ps).map(ps0 => Result.Warning(ps0, ts0)).getOrElse(Result.Success(ts0))
    }
  }
}

trait ResultInstances extends ResultInstances0 {
  implicit def grackleSemigroupForResult[A: Semigroup]: Semigroup[Result[A]] = _ combine _

  implicit val grackleMonadErrorForResult: MonadError[Result, Either[Throwable, NonEmptyChain[Problem]]] =
    new MonadError[Result, Either[Throwable, NonEmptyChain[Problem]]] {

      def raiseError[A](e: Either[Throwable, NonEmptyChain[Problem]]): Result[A] =
        e.fold(Result.InternalError(_), Result.Failure(_))

      def handleErrorWith[A](fa: Result[A])(f: Either[Throwable, NonEmptyChain[Problem]] => Result[A]): Result[A] =
        fa match {
          case Result.Failure(ps)      => f(Right(ps))
          case Result.InternalError(e) => f(Left(e))
          case _                       => fa
        }

      def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B] = fa.flatMap(f)

      override def map2Eval[A, B, C](fa: Result[A], fb: Eval[Result[B]])(f: (A, B) => C): Eval[Result[C]] =
        fa match {
          case err@Result.InternalError(_) => Eval.now(err)  // no need to evaluate fb
          case fail@Result.Failure(_)      => Eval.now(fail) // no need to evaluate fb
          case notLeft                     => fb.map(fb => map2(notLeft, fb)(f))
        }

      def tailRecM[A, B](a: A)(fn: A => Result[Either[A, B]]): Result[B] = {
        @tailrec
        def loop(v: Result[Either[A, B]]): Result[B] =
          v match {
            case err@Result.InternalError(_)  => err
            case fail@Result.Failure(_)       => fail
            case Result.Success(Right(b))     => Result.success(b)
            case Result.Warning(ps, Right(b)) => Result.Warning(ps, b)
            case Result.Success(Left(a))      => loop(fn(a))
            case Result.Warning(ps, Left(a)) =>
              fn(a) match {
                case err@Result.InternalError(_) => err
                case Result.Success(a)       => loop(Result.Warning(ps, a))
                case Result.Failure(ps0)     => Result.Failure(ps ++ ps0)
                case Result.Warning(ps0, a)  => loop(Result.Warning(ps ++ ps0, a))
              }
          }
        loop(fn(a))
      }

      override def pure[A](a: A): Result[A] = Result.success(a)

      override def map[A, B](fa: Result[A])(f: A => B): Result[B] = fa.map(f)
    }

  implicit def grackleParallelForResult[E]: Parallel.Aux[Result, Result] =
    new Parallel[Result] {
      type F[x] = Result[x]

      private[this] val identityK: Result ~> Result = FunctionK.id

      def parallel: Result ~> Result = identityK
      def sequential: Result ~> Result = identityK

      val applicative: Applicative[Result] = new Applicative[Result] {
        def pure[A](a: A): Result[A] = Result.success(a)
        def ap[A, B](ff: Result[A => B])(fa: Result[A]): Result[B] =
          fa match {
            case err@Result.InternalError(_) => err
            case fail@Result.Failure(ps) =>
              ff match {
                case err@Result.InternalError(_) => err
                case Result.Success(_)           => fail
                case Result.Failure(ps0)         => Result.Failure(ps0 ++ ps)
                case Result.Warning(ps0, _)       => Result.Failure(ps0 ++ ps)
              }
            case Result.Success(a) =>
              ff match {
                case err@Result.InternalError(_) => err
                case fail@Result.Failure(_)      => fail
                case Result.Success(f)           => Result.success(f(a))
                case Result.Warning(ps, f)       => Result.Warning(ps, f(a))
              }
            case Result.Warning(ps, a) =>
              ff match {
                case err@Result.InternalError(_) => err
                case fail@Result.Failure(_)      => fail
                case Result.Success(f)           => Result.Warning(ps, f(a))
                case Result.Warning(ps0, f)      => Result.Warning(ps0 ++ ps, f(a))
              }
          }
      }

      lazy val monad: Monad[Result] = grackleMonadErrorForResult
    }
}

trait ResultInstances0 {
  implicit val grackleTraverseFunctorForResult: Traverse[Result] =
    new Traverse[Result] {
      def traverse[F[_]: Applicative, A, B](fa: Result[A])(f: A => F[B]): F[Result[B]] =
        fa.traverse(f)

      override def mapAccumulate[S, B, C](init: S, fa: Result[B])(f: (S, B) => (S, C)): (S, Result[C]) =
        fa match {
          case err @ Result.InternalError(_) => (init, err)
          case fail @ Result.Failure(_) => (init, fail)
          case Result.Success(b) =>
            val (snext, c) = f(init, b)
            (snext, Result.Success(c))
          case Result.Warning(a, b) =>
            val (snext, c) = f(init, b)
            (snext, Result.Warning(a, c))
        }
      def foldLeft[B, C](fa: Result[B], b: C)(f: (C, B) => C): C =
        fa.foldLeft(b)(f)
      def foldRight[B, C](fa: Result[B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
        fa.foldRight(lc)(f)

      override def size[B](fa: Result[B]): Long =
        if (fa.hasValue) 1L else 0L

      override def get[B](fa: Result[B])(idx: Long): Option[B] =
        if (idx == 0L) fa.toOption else None

      override def forall[B](fa: Result[B])(p: (B) => Boolean): Boolean = fa.forall(p)

      override def exists[B](fa: Result[B])(p: (B) => Boolean): Boolean = fa.exists(p)

      override def map[B, C](fa: Result[B])(f: B => C): Result[C] =
        fa.map(f)
    }

  implicit def grackleEqForResult[A: Eq]: Eq[Result[A]] = _ === _
}

final case class ResultT[F[_], A](value: F[Result[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ResultT[F, B] = ResultT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => ResultT[F, B])(implicit F: Monad[F]): ResultT[F, B] =
    ResultT(F.flatMap(value) {
      case err@Result.InternalError(_) => F.pure(err)
      case fail@Result.Failure(_)      => F.pure(fail)
      case Result.Success(a)           => f(a).value
      case Result.Warning(ps, a) =>
        F.map(f(a).value) {
          case err@Result.InternalError(_) => err
          case Result.Success(b)       => Result.Warning(ps, b)
          case Result.Failure(ps0)     => Result.Failure(ps ++ ps0)
          case Result.Warning(ps0, b)  => Result.Warning(ps ++ ps0, b)
        }
    })
}

object ResultT {
  implicit def grackleApplicativeForResultT[F[_]](implicit F: Applicative[F]): Applicative[ResultT[F, *]] =
    new Applicative[ResultT[F, *]] {
      override def pure[A](a: A): ResultT[F, A] = ResultT(F.pure(Result.success(a)))

      def ap[A, B](fab: ResultT[F, A => B])(fa: ResultT[F, A]): ResultT[F, B] = ResultT(
        F.map2(fab.value, fa.value) {
          case (fab, a) => fab.ap(a)
        }
      )
    }

  def liftF[F[_]: Functor, A](fa: F[A]): ResultT[F, A] =
    ResultT(fa.map(Result.success))

  def success[F[_]: Applicative, A](a: A): ResultT[F, A] = 
    ResultT(Result.success(a).pure[F])

  def warning[F[_]: Applicative, A](warning: NonEmptyChain[Problem], value: A): ResultT[F, A] =
    ResultT(Result.Warning(warning, value).pure[F].widen)

  def warning[F[_]: Applicative, A](warning: Problem, value: A): ResultT[F, A] =
    ResultT(Result.warning(warning, value).pure[F])

  def warning[F[_]: Applicative, A](warning: String, value: A): ResultT[F, A] =
    ResultT(Result.warning(warning, value).pure[F])

  def failure[F[_]: Applicative, A](s: String): ResultT[F, A] =
    ResultT(Result.failure[A](s).pure[F])

  def failure[F[_]: Applicative, A](p: Problem): ResultT[F, A] =
    ResultT(Result.failure[A](p).pure[F])

  def failure[F[_]: Applicative, A](ps: NonEmptyChain[Problem]): ResultT[F, A] =
    ResultT(Result.Failure(ps).pure[F].widen)

  def internalError[F[_]: Applicative, A](err: Throwable): ResultT[F, A] =
    ResultT(Result.internalError[A](err).pure[F])

  def internalError[F[_]: Applicative, A](err: String): ResultT[F, A] =
    ResultT(Result.internalError[A](err).pure[F])

}
