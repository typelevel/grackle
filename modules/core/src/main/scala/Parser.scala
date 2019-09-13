// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto._, Atto._
import cats.implicits._

object Parser {

  // TODO: .token needs to also absorb commas, as do all the paren combinators

  def keyword(s: String) = string(s).token

  lazy val Document: Parser[Ast.Document] =
    many(whitespace) ~> many(Definition)

  lazy val Definition =
    ExecutableDefinition // | TypeSystemDefinition | TypeSystemExtension

  lazy val ExecutableDefinition =
    OperationDefinition || FragmentDefinition

  lazy val OperationDefinition: Parser[Ast.OperationDefinition] =
    QueryShorthand.widen[Ast.OperationDefinition] | Operation.widen

  lazy val QueryShorthand: Parser[Ast.OperationDefinition.QueryShorthand] =
    SelectionSet.map(Ast.OperationDefinition.QueryShorthand)

  lazy val Operation: Parser[Ast.OperationDefinition.Operation] =
    for {
      op   <- OperationType
      name <- opt(Name)
      vars <- opt(VariableDefinitions)
      dirs <- Directives
      sels <- SelectionSet
    } yield Ast.OperationDefinition.Operation(op, name, vars.orEmpty, dirs, sels)

  lazy val OperationType: Parser[Ast.OperationType] =
    keyword("query")       .as(Ast.OperationType.Query)       .widen[Ast.OperationType] |
    keyword("mutation")    .as(Ast.OperationType.Mutation)    .widen                    |
    keyword("subscription").as(Ast.OperationType.Subscription).widen

  lazy val SelectionSet: Parser[List[Ast.Selection]] =
    braces(many(Selection)).token

  lazy val Selection: Parser[Ast.Selection] =
    Field.widen[Ast.Selection] |
    FragmentSpread.widen       |
    InlineFragment.widen

  lazy val Field: Parser[Ast.Selection.Field] =
    for {
      alias <- opt(Alias)
      name  <- Name
      args  <- opt(Arguments)
      dirs  <- Directives
      sel   <- opt(SelectionSet)
    } yield Ast.Selection.Field(alias, name, args.orEmpty, dirs, sel.orEmpty)

  lazy val Alias: Parser[Ast.Name] =
    Name <~ keyword(":")

  lazy val Arguments: Parser[List[(Ast.Name, Ast.Value)]] =
    parens(many(Argument)).token

  lazy val Argument: Parser[(Ast.Name, Ast.Value)] =
    (Name <~ keyword(":")) ~ Value

  lazy val FragmentSpread: Parser[Ast.Selection.FragmentSpread] =
    keyword("...") ~> (FragmentName, Directives).mapN(Ast.Selection.FragmentSpread)

  lazy val InlineFragment: Parser[Ast.Selection.InlineFragment] =
    for {
      _    <- keyword("...")
      cond <- opt(TypeCondition)
      dirs <- Directives
      sel  <- SelectionSet
    } yield Ast.Selection.InlineFragment(cond, dirs, sel)

  lazy val FragmentDefinition =
    for {
      _    <- string("fragment").token
      name <- FragmentName
      cond <- TypeCondition
      dirs <- Directives
      sel  <- SelectionSet
    } yield Ast.FragmentDefinition(name, cond, dirs, sel)

  lazy val FragmentName =
    Name.filter(_ != Ast.Name("on"))

  lazy val TypeCondition =
    keyword("on") ~> NamedType

  lazy val Value: Parser[Ast.Value] =
    Variable    .widen[Ast.Value] |
    IntValue    .widen            |
    FloatValue  .widen            |
    StringValue .widen            |
    BooleanValue.widen            |
    NullValue   .widen            |
    EnumValue   .widen            |
    ListValue   .widen            |
    ObjectValue .widen

  lazy val NullValue: Parser[Ast.Value.NullValue.type] =
    keyword("null").as(Ast.Value.NullValue)

  lazy val EnumValue: Parser[Ast.Value.EnumValue] =
    Name.filter {
      case Ast.Name("true")  => false
      case Ast.Name("false") => false
      case Ast.Name("null")  => false
      case _                 => true
    } .map(Ast.Value.EnumValue)

  lazy val ListValue: Parser[Ast.Value.ListValue] =
    squareBrackets(many(Value)).map(Ast.Value.ListValue).token

  lazy val IntValue: Parser[Ast.Value.IntValue] =
    bigDecimal.token.filter(_.isValidInt).map(a => Ast.Value.IntValue(a.toInt))

  lazy val FloatValue: Parser[Ast.Value.FloatValue] =
    bigDecimal.token.filter(_.isDecimalDouble).map(a => Ast.Value.FloatValue(a.toDouble))

  lazy val StringValue: Parser[Ast.Value.StringValue] =
    stringLiteral.token.map(Ast.Value.StringValue)

  lazy val BooleanValue: Parser[Ast.Value.BooleanValue] =
    (keyword("true").as(true) | keyword("false").as(false)).map(Ast.Value.BooleanValue)

  lazy val ObjectValue: Parser[Ast.Value.ObjectValue] =
    braces(many(ObjectField)).map(Ast.Value.ObjectValue).token

  lazy val ObjectField: Parser[(Ast.Name, Ast.Value)] =
    (Name <~ keyword(":")) ~ Value

  lazy val VariableDefinitions =
    parens(many(VariableDefinition)).token

  lazy val VariableDefinition: Parser[Ast.VariableDefinition] =
    for {
      v    <- Variable
      _    <- keyword(":")
      tpe  <- Type
      dv   <- opt(DefaultValue)
      dirs <- Directives
    } yield Ast.VariableDefinition(v, tpe, dv, dirs)

  lazy val Variable: Parser[Ast.Value.Variable] =
    keyword("$") ~> Name.map(Ast.Value.Variable)

  lazy val DefaultValue: Parser[Ast.Value] =
    keyword("=") ~> Value

  lazy val Type: Parser[Ast.Type] =
    NonNullType.widen[Ast.Type] | ListType.widen | NamedType.widen

  lazy val NamedType: Parser[Ast.Type.Named] =
    Name.map(Ast.Type.Named)

  lazy val ListType: Parser[Ast.Type.List] =
    squareBrackets(Type).map(Ast.Type.List).token

  lazy val NonNullType: Parser[Ast.Type.NonNull] =
    (NamedType <~ keyword("!")).map(a => Ast.Type.NonNull(Left(a))).widen |
    (ListType  <~ keyword("!")).map(a => Ast.Type.NonNull(Right(a))).widen

  lazy val Directives =
    many(Directive)

  lazy val Directive: Parser[Ast.Directive] =
    keyword("@") ~> (Name, opt(Arguments)).mapN((n, ods) => Ast.Directive(n, ods.orEmpty))

  lazy val Name: Parser[Ast.Name] = {
    val initial    = ('A' to 'Z').toSet ++ ('a' to 'z').toSet + '_'
    val subsequent = initial ++ ('0' to '9').toSet
    (satisfy(initial), many(satisfy(subsequent))).mapN((h, t) => Ast.Name((h :: t).mkString)).token
  }

}
