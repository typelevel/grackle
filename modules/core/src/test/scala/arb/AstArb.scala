// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package arb

import edu.gemini.grackle._
import org.scalacheck.{ Arbitrary, Gen }

trait AstArb {
  import Arbitrary.arbitrary
  import Gen._
  import Ast._

  def shortListOf[A](gen: Gen[A]): Gen[List[A]] =
    choose(0, 5).flatMap(listOfN(_, gen))

  implicit lazy val arbName: Arbitrary[Name] =
    Arbitrary {
      val initial    = ('A' to 'Z').toList ++ ('a' to 'z').toList :+ '_'
      val subsequent = initial ++ ('0' to '9').toList
      for {
        h <- oneOf(initial)
        t <- shortListOf(oneOf(subsequent))
      } yield Name((h :: t).mkString)
    }

  implicit lazy val arbValueVariable: Arbitrary[Value.Variable] =
    Arbitrary {
      arbitrary[Name].map(Value.Variable.apply)
    }

  // We want to generate reasonable named types.
  implicit val arbTypeNamed: Arbitrary[Type.Named] =
    Arbitrary {
      oneOf("String", "Float").map(s => Type.Named(Name(s)))
    }

  implicit val arbTypeList: Arbitrary[Type.List]  =
    Arbitrary {
      arbitrary[Type].map(Type.List.apply)
    }

  implicit val arbTypeNonNull: Arbitrary[Type.NonNull] =
    Arbitrary {
      arbitrary[Either[Type.Named, Type.List]].map(Type.NonNull.apply)
    }

  implicit lazy val arbType: Arbitrary[Type] =
    Arbitrary {
      lzy(oneOf(arbitrary[Type.Named], arbitrary[Type.List], arbitrary[Type.NonNull]))
    }

  implicit lazy val arbValue: Arbitrary[Value] =
    Arbitrary {
      oneOf(
        arbitrary[Value.Variable],
        arbitrary[Int].map(Value.IntValue.apply),
        arbitrary[Double].map(Value.FloatValue.apply),
        alphaStr.map(Value.StringValue.apply),
        arbitrary[Boolean].map(Value.BooleanValue.apply),
        Gen.const(Value.NullValue),
        // TODO: enum
        // TODO: list
        // TODO: object
      )
    }

  implicit lazy val arbDirective: Arbitrary[Directive] =
    Arbitrary {
      for {
        name <- arbitrary[Name]
        args <- shortListOf(arbitrary[(Name, Value)])
      } yield Directive(name, args)
    }

  def genValueForType(t: Type): Gen[Value] =
    t match {

      case Type.Named(Name("String")) => oneOf(alphaStr.map(Value.StringValue.apply), const(Value.NullValue))
      case Type.Named(Name("Float"))  => oneOf(arbitrary[Double].map(Value.FloatValue.apply), const(Value.NullValue))
      // no other named types are known yet
      case Type.Named(_) => fail[Value]

      case Type.NonNull(t) => genValueForType(t.merge).flatMap { case Value.NullValue => fail ; case v => const(v) }

      case Type.List(t) => shortListOf(genValueForType(t)).map(vs => Value.ListValue(vs))

    }

  implicit lazy val arbVariableDefinition: Arbitrary[VariableDefinition] =
    Arbitrary {
      for {
        variable     <- arbitrary[Name]
        tpe          <- arbitrary[Type]
        defaultValue <- option(genValueForType(tpe))
      } yield VariableDefinition(variable, tpe, defaultValue)
    }

  implicit lazy val arbSelectionFragmentSpread: Arbitrary[Selection.FragmentSpread] =
    Arbitrary {
      for {
        name       <- arbitrary[Name]
        directives <- shortListOf(arbitrary[Directive])
      } yield Selection.FragmentSpread(name, directives)
    }

  implicit lazy val arbSelection: Arbitrary[Selection] =
    Arbitrary {
      arbitrary[Selection.FragmentSpread]
      // TODO: field, inlinefragment
    }


}
