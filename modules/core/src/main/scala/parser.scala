// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.parse.{Parser, Parser0}
import cats.parse.Parser._
import cats.parse.Numbers._
import cats.parse.Rfc5234._
import cats.implicits._
import CommentedText2._

object GraphQLParser2 {

  def keyword(s: String) = token(string(s))

  lazy val Document: Parser0[Ast.Document] =
    many(whitespace | comment) *> many(Definition) <* Parser.end

  lazy val Definition =
    ExecutableDefinition | TypeSystemDefinition // | TypeSystemExtension

  lazy val TypeSystemDefinition =
    SchemaDefinition | TypeDefinition | DirectiveDefinition

  lazy val SchemaDefinition =
    for {
      _        <- keyword("schema")
      dirs     <- opt(Directives)
      rootdefs <- braces(many(RootOperationTypeDefinition))
    } yield Ast.SchemaDefinition(rootdefs, dirs.getOrElse(Nil))

  lazy val RootOperationTypeDefinition =
    for {
      optpe <- OperationType
      _     <- keyword(":")
      tpe   <- NamedType
    } yield Ast.RootOperationTypeDefinition(optpe, tpe)

  lazy val TypeDefinition =
    ScalarTypeDefinition.widen[Ast.TypeDefinition] |
    ObjectTypeDefinition |
    InterfaceTypeDefinition |
    UnionTypeDefinition |
    EnumTypeDefinition |
    InputObjectTypeDefinition

  lazy val ScalarTypeDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("scalar")
      name   <- Name
      dirs   <- opt(Directives)
    } yield Ast.ScalarTypeDefinition(name, desc.map(_.value), dirs.getOrElse(Nil))

  lazy val ObjectTypeDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("type")
      name   <- Name
      ifs    <- opt(ImplementsInterfaces)
      dirs   <- opt(Directives)
      fields <- FieldsDefinition
    } yield Ast.ObjectTypeDefinition(name, desc.map(_.value), fields, ifs.getOrElse(Nil), dirs.getOrElse(Nil))

  lazy val InterfaceTypeDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("interface")
      name   <- Name
      ifs    <- opt(ImplementsInterfaces)
      dirs   <- opt(Directives)
      fields <- FieldsDefinition
    } yield Ast.InterfaceTypeDefinition(name, desc.map(_.value), fields, ifs.getOrElse(Nil), dirs.getOrElse(Nil))

  lazy val UnionTypeDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("union")
      name   <- Name
      dirs   <- opt(Directives)
      members <- UnionMemberTypes
    } yield Ast.UnionTypeDefinition(name, desc.map(_.value), dirs.getOrElse(Nil), members)

  lazy val EnumTypeDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("enum")
      name   <- Name
      dirs   <- opt(Directives)
      values <- EnumValuesDefinition
    } yield Ast.EnumTypeDefinition(name, desc.map(_.value), dirs.getOrElse(Nil), values)

  lazy val InputObjectTypeDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("input")
      name   <- Name
      dirs   <- opt(Directives)
      fields <- InputFieldsDefinition
    } yield Ast.InputObjectTypeDefinition(name, desc.map(_.value), fields, dirs.getOrElse(Nil))

  lazy val Description = StringValue

  lazy val ImplementsInterfaces =
    (keyword("implements") ~ opt(keyword("&"))) *> NamedType.repSep0(keyword("&"))

  lazy val FieldsDefinition =
    braces(many(FieldDefinition))

  lazy val FieldDefinition =
    for {
      desc   <- opt(Description)
      name   <- Name
      args   <- opt(ArgumentsDefinition)
      _      <- keyword(":")
      tpe    <- Type
      dirs   <- opt(Directives)
    } yield Ast.FieldDefinition(name, desc.map(_.value), args.getOrElse(Nil), tpe, dirs.getOrElse(Nil))

  lazy val ArgumentsDefinition =
    parens(many(InputValueDefinition))

  lazy val InputFieldsDefinition =
    braces(many(InputValueDefinition))

  lazy val InputValueDefinition =
    for {
      desc   <- opt(Description)
      name   <- Name
      _      <- keyword(":")
      tpe    <- Type
      dv     <- opt(DefaultValue)
      dirs   <- opt(Directives)
    } yield Ast.InputValueDefinition(name, desc.map(_.value), tpe, dv, dirs.getOrElse(Nil))

  lazy val UnionMemberTypes =
    (keyword("=") ~ opt(keyword("|"))) *> NamedType.repSep0(keyword("|"))

  lazy val EnumValuesDefinition =
    braces(many(EnumValueDefinition))

  lazy val EnumValueDefinition =
    for {
      desc   <- opt(Description)
      name   <- Name
      dirs   <- opt(Directives)
    } yield Ast.EnumValueDefinition(name, desc.map(_.value), dirs.getOrElse(Nil))

  lazy val DirectiveDefinition =
    for {
      desc   <- opt(Description)
      _      <- keyword("directive")
      _      <- keyword("@")
      name   <- Name
      args   <- ArgumentsDefinition
      rpt    <- opt(keyword("repeatable"))
      _      <- keyword("on")
      locs   <- DirectiveLocations
    } yield Ast.DirectiveDefinition(name, desc.map(_.value), args, rpt.isDefined, locs)

  lazy val DirectiveLocations =
    opt(keyword("|")) *> DirectiveLocation.repSep0(keyword("|"))

  lazy val DirectiveLocation =
    keyword("QUERY")       .as(Ast.DirectiveLocation.QUERY).widen[Ast.DirectiveLocation]     |
    keyword("MUTATION")    .as(Ast.DirectiveLocation.MUTATION).widen                         |
    keyword("SUBSCRIPTION").as(Ast.DirectiveLocation.SUBSCRIPTION).widen                     |
    keyword("FIELD").as(Ast.DirectiveLocation.FIELD).widen                                   |
    keyword("FRAGMENT_DEFINITION").as(Ast.DirectiveLocation.FRAGMENT_DEFINITION).widen       |
    keyword("FRAGMENT_SPREAD").as(Ast.DirectiveLocation.FRAGMENT_SPREAD).widen               |
    keyword("INLINE_FRAGMENT").as(Ast.DirectiveLocation.INLINE_FRAGMENT).widen               |
    keyword("VARIABLE_DEFINITION").as(Ast.DirectiveLocation.VARIABLE_DEFINITION).widen       |
    keyword("SCHEMA").as(Ast.DirectiveLocation.SCHEMA).widen                                 |
    keyword("SCALAR").as(Ast.DirectiveLocation.SCALAR).widen                                 |
    keyword("OBJECT").as(Ast.DirectiveLocation.OBJECT).widen                                 |
    keyword("FIELD_DEFINITION").as(Ast.DirectiveLocation.FIELD_DEFINITION).widen             |
    keyword("ARGUMENT_DEFINITION").as(Ast.DirectiveLocation.ARGUMENT_DEFINITION).widen       |
    keyword("INTERFACE").as(Ast.DirectiveLocation.INTERFACE).widen                           |
    keyword("UNION").as(Ast.DirectiveLocation.UNION).widen                                   |
    keyword("ENUM").as(Ast.DirectiveLocation.ENUM).widen                                     |
    keyword("ENUM_VALUE").as(Ast.DirectiveLocation.ENUM_VALUE).widen                         |
    keyword("INPUT_OBJECT").as(Ast.DirectiveLocation.INPUT_OBJECT).widen                     |
    keyword("INPUT_FIELD_DEFINITION").as(Ast.DirectiveLocation.INPUT_FIELD_DEFINITION).widen

  lazy val ExecutableDefinition =
    OperationDefinition | FragmentDefinition

  lazy val OperationDefinition: Parser[Ast.OperationDefinition] =
    QueryShorthand.widen[Ast.OperationDefinition] | Operation.widen

  lazy val QueryShorthand: Parser[Ast.OperationDefinition.QueryShorthand] =
    SelectionSet.map(Ast.OperationDefinition.QueryShorthand.apply)

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
    braces(many(Selection))

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
    Name <* keyword(":")

  lazy val Arguments: Parser[List[(Ast.Name, Ast.Value)]] =
    parens(many(Argument))

  lazy val Argument: Parser[(Ast.Name, Ast.Value)] =
    (Name <* keyword(":")) ~ Value

  lazy val FragmentSpread: Parser[Ast.Selection.FragmentSpread] =
    keyword("...") *> (FragmentName, Directives).mapN(Ast.Selection.FragmentSpread.apply)

  lazy val InlineFragment: Parser[Ast.Selection.InlineFragment] =
    for {
      _    <- keyword("...")
      cond <- opt(TypeCondition)
      dirs <- Directives
      sel  <- SelectionSet
    } yield Ast.Selection.InlineFragment(cond, dirs, sel)

  lazy val FragmentDefinition =
    for {
      _    <- keyword("fragment")
      name <- FragmentName
      cond <- TypeCondition
      dirs <- Directives
      sel  <- SelectionSet
    } yield Ast.FragmentDefinition(name, cond, dirs, sel)

  lazy val FragmentName =
    Name.filter(_ != Ast.Name("on"))

  lazy val TypeCondition =
    keyword("on") *> NamedType

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
    } .map(Ast.Value.EnumValue.apply)

  lazy val ListValue: Parser[Ast.Value.ListValue] =
    token(squareBrackets(many(Value)).map(Ast.Value.ListValue.apply))

  lazy val IntValue: Parser[Ast.Value.IntValue] =
    token(bigInt).filter(_.isValidInt).map(a => Ast.Value.IntValue(a.toInt))

  lazy val FloatValue: Parser[Ast.Value.FloatValue] =
    token(bigDecimal).filter(_.isDecimalDouble).map(a => Ast.Value.FloatValue(a.toDouble))

  lazy val StringValue: Parser[Ast.Value.StringValue] =
    token(stringLiteral).map(Ast.Value.StringValue.apply)

  lazy val BooleanValue: Parser[Ast.Value.BooleanValue] =
    (keyword("true").as(true) | keyword("false").as(false)).map(Ast.Value.BooleanValue.apply)

  lazy val ObjectValue: Parser[Ast.Value.ObjectValue] =
    braces(many(ObjectField)).map(Ast.Value.ObjectValue.apply)

  lazy val ObjectField: Parser[(Ast.Name, Ast.Value)] =
    (Name <* keyword(":")) ~ Value

  lazy val VariableDefinitions =
    parens(many(VariableDefinition))

  lazy val VariableDefinition: Parser[Ast.VariableDefinition] =
    for {
      v    <- Variable
      _    <- keyword(":")
      tpe  <- Type
      dv   <- opt(DefaultValue)
    } yield Ast.VariableDefinition(v.name, tpe, dv)

  lazy val Variable: Parser[Ast.Value.Variable] =
    keyword("$") *> Name.map(Ast.Value.Variable.apply)

  lazy val DefaultValue: Parser[Ast.Value] =
    keyword("=") *> Value

  lazy val Type: Parser[Ast.Type] =
    NonNullType.widen[Ast.Type] | ListType.widen | NamedType.widen

  lazy val NamedType: Parser[Ast.Type.Named] =
    Name.map(Ast.Type.Named.apply)

  lazy val ListType: Parser[Ast.Type.List] =
    squareBrackets(Type).map(Ast.Type.List.apply)

  lazy val NonNullType: Parser[Ast.Type.NonNull] =
    (NamedType <* keyword("!")).map(a => Ast.Type.NonNull(Left(a))).widen |
    (ListType  <* keyword("!")).map(a => Ast.Type.NonNull(Right(a))).widen

  lazy val Directives =
    many(Directive)

  lazy val Directive: Parser[Ast.Directive] =
    keyword("@") *> (Name, opt(Arguments)).mapN((n, ods) => Ast.Directive(n, ods.orEmpty))

  lazy val Name: Parser[Ast.Name] = {
    val initial    = ('A' to 'Z').toSet ++ ('a' to 'z').toSet + '_'
    val subsequent = initial ++ ('0' to '9').toSet
    token((charWhere(initial), many(charWhere(subsequent))).mapN((h, t) => Ast.Name((h :: t).mkString)))
  }
}

object CommentedText2 {

  lazy val whitespace: Parser[Char] = charWhere(_.isWhitespace)

  def opt[A](parser: Parser0[A]): Parser0[Option[A]] = parser.backtrack.?

  def many[A](parser: Parser[A]): Parser0[List[A]] = parser.rep0

  def skipWhitespace: Parser[Unit] =
    charsWhile(c => c.isWhitespace || c == ',').void.withContext("whitespace")

  /** Parser that consumes a comment */
  def comment: Parser[Unit] = {
    (char('#') *> many((charWhere(c => c != '\n' && c != '\r'))) <* charIn('\n', '\r') <* skipWhitespace).void
  }

  /** Turns a parser into one that skips trailing whitespace and comments */
  def token[A](p: Parser[A]): Parser[A] =
    p <* skipWhitespace <* many(comment)

  def token0[A](p: Parser0[A]): Parser0[A] =
    p <* skipWhitespace <* many(comment)

  /**
   * Consumes `left` and `right`, including the trailing and preceding whitespace,
   * respectively, and returns the value of `p`.
   */
  private def _bracket[A,B,C](left: Parser[B], p: => Parser0[A], right: => Parser[C]): Parser[A] =
    token(left) *> token0(p) <* token(right)

  /** Turns a parser into one that consumes surrounding parentheses `()` */
  def parens[A](p: => Parser0[A]): Parser[A] =
    _bracket(char('('), p, char(')')).withContext(s"parens(${p.toString})")

  /** Turns a parser into one that consumes surrounding curly braces `{}` */
  def braces[A](p: => Parser0[A]): Parser[A] = {
    lazy val q = p
    _bracket(char('{'), q, char('}')).withContext(s"braces(${q.toString})")
  }

  /** Turns a parser into one that consumes surrounding square brackets `[]` */
  def squareBrackets[A](p: => Parser0[A]): Parser[A] = {
    lazy val q = p
    _bracket(char('['), q, char(']')).withContext(s"squareBrackets(${q.toString})")
  }
}

object Literals {

  lazy val stringLiteral: Parser[String] = (stringCharacter.rep0.surroundedBy(char('"')) | (blockStringCharacter.rep0.surroundedBy(stringIn("\"\"\"")))).string

  //TODO more options
  lazy val stringCharacter = (string("\\u") ~ escapedUnicode).string | (char('\\') ~ escapedCharacter).string

  lazy val blockStringCharacter = ???

  //TODO should this be a Parser[Char] which converts to unicode? I guess so?
  //See Atto for reference
  lazy val escapedUnicode = (digit | hexdig).repExactlyAs[String](4)

  lazy val escapedCharacter = charIn('"', '\\', '/', 'b', 'f', 'n', 'r', 't')

}
