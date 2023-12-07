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

import scala.util.matching.Regex

import cats.implicits._
import cats.parse.{Parser, Parser0}
import cats.parse.Parser._
import cats.parse.Numbers._
import cats.parse.Rfc5234.{cr, crlf, digit, hexdig, lf}

trait GraphQLParser {
  def parseText(text: String): Result[Ast.Document]
}

object GraphQLParser {
  case class Config(
    maxSelectionDepth: Int,
    maxSelectionWidth: Int,
    maxInputValueDepth: Int,
    maxListTypeDepth: Int
  )

  val defaultConfig: Config =
    Config(
      maxSelectionDepth = 100,
      maxSelectionWidth = 1000,
      maxInputValueDepth = 5,
      maxListTypeDepth = 5
    )

  def apply(config: Config): GraphQLParser =
    new Impl(config)

  def toResult[T](pr: Either[Parser.Error, T]): Result[T] =
    Result.fromEither(pr.leftMap(_.show))

  import CommentedText._
  import Literals._

  private final class Impl(config: Config) extends GraphQLParser {
    import config._

    def parseText(text: String): Result[Ast.Document] =
      toResult(Document.parseAll(text))

    val nameInitial    = ('A' to 'Z') ++ ('a' to 'z') ++ Seq('_')
    val nameSubsequent = nameInitial ++ ('0' to '9')

    def keyword(s: String) = token(string(s) <* not(charIn(nameSubsequent)))

    def punctuation(s: String) = token(string(s))

    lazy val Document: Parser0[Ast.Document] =
      (whitespace.void | comment).rep0 *> Definition.rep0 <* Parser.end

    lazy val Definition: Parser[Ast.Definition] =
      ExecutableDefinition | TypeSystemDefinition | TypeSystemExtension

    lazy val TypeSystemDefinition: Parser[Ast.TypeSystemDefinition] = {
      val SchemaDefinition: Parser[Ast.SchemaDefinition] =
        ((keyword("schema") *> Directives.?) ~ braces(RootOperationTypeDefinition.rep0)).map {
          case (dirs, rootdefs) => Ast.SchemaDefinition(rootdefs, dirs.getOrElse(Nil))
        }

      def typeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.TypeDefinition] = {

        def scalarTypeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.ScalarTypeDefinition] =
          ((keyword("scalar") *> Name) ~ Directives.?).map {
            case (name, dirs) => Ast.ScalarTypeDefinition(name, desc.map(_.value), dirs.getOrElse(Nil))
          }

        def objectTypeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.ObjectTypeDefinition] =
          ((keyword("type") *> Name) ~ ImplementsInterfaces.? ~ Directives.? ~ FieldsDefinition).map {
            case (((name, ifs), dirs), fields) => Ast.ObjectTypeDefinition(name, desc.map(_.value), fields, ifs.getOrElse(Nil), dirs.getOrElse(Nil))
          }

        def interfaceTypeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.InterfaceTypeDefinition] =
          ((keyword("interface") *> Name) ~ ImplementsInterfaces.? ~ Directives.? ~ FieldsDefinition).map {
            case (((name, ifs), dirs), fields) => Ast.InterfaceTypeDefinition(name, desc.map(_.value), fields, ifs.getOrElse(Nil), dirs.getOrElse(Nil))
          }

        def unionTypeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.UnionTypeDefinition] =
          ((keyword("union") *> Name) ~ Directives.? ~ UnionMemberTypes).map {
            case ((name, dirs), members) => Ast.UnionTypeDefinition(name, desc.map(_.value), dirs.getOrElse(Nil), members)
          }

        def enumTypeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.EnumTypeDefinition] =
          ((keyword("enum") *> Name) ~ Directives.? ~ EnumValuesDefinition).map {
            case ((name, dirs), values) => Ast.EnumTypeDefinition(name, desc.map(_.value), dirs.getOrElse(Nil), values)
          }

        def inputObjectTypeDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.InputObjectTypeDefinition] =
          ((keyword("input") *> Name) ~ Directives.? ~ InputFieldsDefinition).map {
            case ((name, dirs), fields) => Ast.InputObjectTypeDefinition(name, desc.map(_.value), fields, dirs.getOrElse(Nil))
          }

        scalarTypeDefinition(desc)|
          objectTypeDefinition(desc) |
          interfaceTypeDefinition(desc) |
          unionTypeDefinition(desc) |
          enumTypeDefinition(desc) |
          inputObjectTypeDefinition(desc)
      }

      def directiveDefinition(desc: Option[Ast.Value.StringValue]): Parser[Ast.DirectiveDefinition] =
        ((keyword("directive") *> punctuation("@") *> Name) ~
          ArgumentsDefinition.? ~ (keyword("repeatable").? <* keyword("on")) ~ DirectiveLocations).map {
          case (((name, args), rpt), locs) => Ast.DirectiveDefinition(name, desc.map(_.value), args.getOrElse(Nil), rpt.isDefined, locs)
        }

      SchemaDefinition |
        Description.?.with1.flatMap { desc =>
          typeDefinition(desc) | directiveDefinition(desc)
        }
    }

    lazy val TypeSystemExtension: Parser[Ast.TypeSystemExtension] = {

      val SchemaExtension: Parser[Ast.SchemaExtension] =
        ((keyword("schema") *> Directives.?) ~ braces(RootOperationTypeDefinition.rep0).?).map {
          case (dirs, rootdefs) => Ast.SchemaExtension(rootdefs.getOrElse(Nil), dirs.getOrElse(Nil))
        }

      val TypeExtension: Parser[Ast.TypeExtension] = {

        val ScalarTypeExtension: Parser[Ast.ScalarTypeExtension] =
          ((keyword("scalar") *> NamedType) ~ Directives.?).map {
            case (((name), dirs)) => Ast.ScalarTypeExtension(name, dirs.getOrElse(Nil))
          }

        val ObjectTypeExtension: Parser[Ast.ObjectTypeExtension] =
          ((keyword("type") *> NamedType) ~ ImplementsInterfaces.? ~ Directives.? ~ FieldsDefinition.?).map {
            case (((name, ifs), dirs), fields) => Ast.ObjectTypeExtension(name, fields.getOrElse(Nil), ifs.getOrElse(Nil), dirs.getOrElse(Nil))
          }

        val InterfaceTypeExtension: Parser[Ast.InterfaceTypeExtension] =
          ((keyword("interface") *> NamedType) ~ ImplementsInterfaces.? ~ Directives.? ~ FieldsDefinition.?).map {
            case (((name, ifs), dirs), fields) => Ast.InterfaceTypeExtension(name, fields.getOrElse(Nil), ifs.getOrElse(Nil), dirs.getOrElse(Nil))
          }

        val UnionTypeExtension: Parser[Ast.UnionTypeExtension] =
          ((keyword("union") *> NamedType) ~ Directives.? ~ UnionMemberTypes.?).map {
            case (((name), dirs), members) => Ast.UnionTypeExtension(name, dirs.getOrElse(Nil), members.getOrElse(Nil))
          }

        val EnumTypeExtension: Parser[Ast.EnumTypeExtension] =
          ((keyword("enum") *> NamedType) ~ Directives.? ~ EnumValuesDefinition.?).map {
            case (((name), dirs), values) => Ast.EnumTypeExtension(name, dirs.getOrElse(Nil), values.getOrElse(Nil))
          }

        val InputObjectTypeExtension: Parser[Ast.InputObjectTypeExtension] =
          ((keyword("input") *> NamedType) ~ Directives.? ~ InputFieldsDefinition.?).map {
            case (((name), dirs), fields) => Ast.InputObjectTypeExtension(name, dirs.getOrElse(Nil), fields.getOrElse(Nil))
          }

        ScalarTypeExtension|
        ObjectTypeExtension|
        InterfaceTypeExtension|
        UnionTypeExtension|
        EnumTypeExtension|
        InputObjectTypeExtension
      }

      keyword("extend") *> (SchemaExtension | TypeExtension)
    }

    lazy val RootOperationTypeDefinition: Parser[Ast.RootOperationTypeDefinition] =
      (OperationType ~ punctuation(":") ~ NamedType ~ Directives).map {
        case (((optpe, _), tpe), dirs) => Ast.RootOperationTypeDefinition(optpe, tpe, dirs)
      }


    lazy val Description = StringValue

    lazy val ImplementsInterfaces =
      (keyword("implements") ~ punctuation("&").?) *> NamedType.repSep0(punctuation("&"))

    lazy val FieldsDefinition: Parser[List[Ast.FieldDefinition]] =
      braces(FieldDefinition.rep0)

    lazy val FieldDefinition: Parser[Ast.FieldDefinition] =
      (Description.?.with1 ~ Name ~ ArgumentsDefinition.? ~ punctuation(":") ~ Type ~ Directives.?).map {
        case (((((desc, name), args), _), tpe), dirs) => Ast.FieldDefinition(name, desc.map(_.value), args.getOrElse(Nil), tpe, dirs.getOrElse(Nil))
      }

    lazy val ArgumentsDefinition: Parser[List[Ast.InputValueDefinition]] =
      parens(InputValueDefinition.rep0)

    lazy val InputFieldsDefinition: Parser[List[Ast.InputValueDefinition]] =
      braces(InputValueDefinition.rep0)

    lazy val InputValueDefinition: Parser[Ast.InputValueDefinition] =
      (Description.?.with1 ~ (Name <* punctuation(":")) ~ Type ~ DefaultValue.? ~ Directives.?).map {
        case ((((desc, name), tpe), dv), dirs) => Ast.InputValueDefinition(name, desc.map(_.value), tpe, dv, dirs.getOrElse(Nil))
      }

    lazy val UnionMemberTypes: Parser[List[Ast.Type.Named]] =
      (punctuation("=") *> punctuation("|").?) *> NamedType.repSep0(punctuation("|"))

    lazy val EnumValuesDefinition: Parser[List[Ast.EnumValueDefinition]] =
      braces(EnumValueDefinition.rep0)

    lazy val EnumValueDefinition: Parser[Ast.EnumValueDefinition] =
      (Description.?.with1 ~ Name ~ Directives.?).map {
        case ((desc, name), dirs) => Ast.EnumValueDefinition(name, desc.map(_.value), dirs.getOrElse(Nil))
      }

    lazy val DirectiveLocations: Parser0[List[Ast.DirectiveLocation]] =
      punctuation("|").? *> DirectiveLocation.repSep0(punctuation("|"))

    lazy val DirectiveLocation: Parser[Ast.DirectiveLocation] =
      keyword("QUERY")       .as(Ast.DirectiveLocation.QUERY) |
      keyword("MUTATION")    .as(Ast.DirectiveLocation.MUTATION) |
      keyword("SUBSCRIPTION").as(Ast.DirectiveLocation.SUBSCRIPTION) |
      keyword("FIELD_DEFINITION").as(Ast.DirectiveLocation.FIELD_DEFINITION) |
      keyword("FIELD").as(Ast.DirectiveLocation.FIELD) |
      keyword("FRAGMENT_DEFINITION").as(Ast.DirectiveLocation.FRAGMENT_DEFINITION) |
      keyword("FRAGMENT_SPREAD").as(Ast.DirectiveLocation.FRAGMENT_SPREAD) |
      keyword("INLINE_FRAGMENT").as(Ast.DirectiveLocation.INLINE_FRAGMENT) |
      keyword("VARIABLE_DEFINITION").as(Ast.DirectiveLocation.VARIABLE_DEFINITION) |
      keyword("SCHEMA").as(Ast.DirectiveLocation.SCHEMA) |
      keyword("SCALAR").as(Ast.DirectiveLocation.SCALAR) |
      keyword("OBJECT").as(Ast.DirectiveLocation.OBJECT) |
      keyword("ARGUMENT_DEFINITION").as(Ast.DirectiveLocation.ARGUMENT_DEFINITION) |
      keyword("INTERFACE").as(Ast.DirectiveLocation.INTERFACE) |
      keyword("UNION").as(Ast.DirectiveLocation.UNION) |
      keyword("ENUM_VALUE").as(Ast.DirectiveLocation.ENUM_VALUE) |
      keyword("ENUM").as(Ast.DirectiveLocation.ENUM) |
      keyword("INPUT_OBJECT").as(Ast.DirectiveLocation.INPUT_OBJECT) |
      keyword("INPUT_FIELD_DEFINITION").as(Ast.DirectiveLocation.INPUT_FIELD_DEFINITION)

    lazy val ExecutableDefinition: Parser[Ast.ExecutableDefinition] =
      OperationDefinition | FragmentDefinition

    lazy val OperationDefinition: Parser[Ast.OperationDefinition] =
      QueryShorthand | Operation

    lazy val QueryShorthand: Parser[Ast.OperationDefinition.QueryShorthand] =
      SelectionSet.map(Ast.OperationDefinition.QueryShorthand.apply)

    lazy val Operation: Parser[Ast.OperationDefinition.Operation] =
      (OperationType ~ Name.? ~ VariableDefinitions.? ~ Directives ~ SelectionSet).map {
        case ((((op, name), vars), dirs), sels) => Ast.OperationDefinition.Operation(op, name, vars.orEmpty, dirs, sels)
      }

    lazy val OperationType: Parser[Ast.OperationType] =
      keyword("query")       .as(Ast.OperationType.Query) |
      keyword("mutation")    .as(Ast.OperationType.Mutation) |
      keyword("subscription").as(Ast.OperationType.Subscription)

    lazy val Alias: Parser[Ast.Name] =
      Name <* punctuation(":")

    lazy val FragmentSpread: Parser[Ast.Selection.FragmentSpread] =
      (FragmentName ~ Directives).map{ case (name, dirs) => Ast.Selection.FragmentSpread.apply(name, dirs)}

    def Field(n: Int): Parser[Ast.Selection.Field] =
      (Alias.backtrack.?.with1 ~ Name ~ Arguments.? ~ Directives ~ SelectionSetN(n).?).map {
        case ((((alias, name), args), dirs), sel) => Ast.Selection.Field(alias, name, args.orEmpty, dirs, sel.orEmpty)
      }

    def InlineFragment(n: Int): Parser[Ast.Selection.InlineFragment] =
      ((TypeCondition.? ~ Directives).with1 ~ SelectionSetN(n)).map {
        case ((cond, dirs), sel) => Ast.Selection.InlineFragment(cond, dirs, sel)
      }

    def Selection(n: Int): Parser[Ast.Selection] =
      Field(n) |
      (punctuation("...") *> (InlineFragment(n) | FragmentSpread))

    lazy val SelectionSet: Parser[List[Ast.Selection]] =
      SelectionSetN(maxSelectionDepth)

    def SelectionSetN(n: Int): Parser[List[Ast.Selection]] =
      braces(guard0(n, "exceeded maximum selection depth")(Selection(_).repAs0(max = maxSelectionWidth)))

    lazy val Arguments: Parser[List[(Ast.Name, Ast.Value)]] =
      parens(Argument.rep0)

    lazy val Argument: Parser[(Ast.Name, Ast.Value)] =
      (Name <* punctuation(":")) ~ Value

    lazy val FragmentName: Parser[Ast.Name] =
      not(string("on")).with1 *> Name

    lazy val FragmentDefinition: Parser[Ast.FragmentDefinition] =
      ((keyword("fragment") *> FragmentName) ~ TypeCondition ~ Directives ~ SelectionSet).map {
        case (((name, cond), dirs), sel) => Ast.FragmentDefinition(name, cond, dirs, sel)
      }

    lazy val TypeCondition: Parser[Ast.Type.Named] =
      keyword("on") *> NamedType

    lazy val NullValue: Parser[Ast.Value.NullValue.type] =
      keyword("null").as(Ast.Value.NullValue)

    lazy val EnumValue: Parser[Ast.Value.EnumValue] =
      (not(string("true") | string("false") | string("null")).with1 *> Name)
        .map(Ast.Value.EnumValue.apply)

    def ListValue(n: Int): Parser[Ast.Value.ListValue] =
      token(squareBrackets(guard0(n, "exceeded maximum input value depth")(ValueN(_).rep0)).map(Ast.Value.ListValue.apply))

    lazy val NumericLiteral: Parser[Ast.Value] = {
      def narrow(d: BigDecimal): Ast.Value.FloatValue =
        Ast.Value.FloatValue(d.toDouble)

      token(
        (intLiteral ~ (char('.') *> digit.rep.string).? ~ ((char('e') | char('E')) *> intLiteral.string).?)
          .map {
            case ((a, Some(b)), None) => narrow(BigDecimal(s"$a.$b"))
            case ((a, None), Some(c)) => narrow(BigDecimal(s"${a}E$c"))
            case ((a, Some(b)), Some(c)) => narrow(BigDecimal(s"$a.${b}E$c"))
            case ((a, None), None) => Ast.Value.IntValue(a)
          }
      )
    }

    lazy val BooleanValue: Parser[Ast.Value.BooleanValue] =
      token(booleanLiteral).map(Ast.Value.BooleanValue.apply)

    def ObjectField(n: Int): Parser[(Ast.Name, Ast.Value)] =
      (Name <* punctuation(":")) ~ ValueN(n)

    def ObjectValue(n: Int): Parser[Ast.Value.ObjectValue] =
      braces(guard0(n, "exceeded maximum input value depth")(ObjectField(_).rep0)).map(Ast.Value.ObjectValue.apply)

    lazy val StringValue: Parser[Ast.Value.StringValue] =
      token(stringLiteral).map(Ast.Value.StringValue.apply)

    def ValueN(n: Int): Parser[Ast.Value] =
      Variable |
        NumericLiteral |
        StringValue |
        BooleanValue |
        NullValue |
        EnumValue |
        ListValue(n) |
        ObjectValue(n)

    lazy val Value: Parser[Ast.Value] =
      ValueN(maxInputValueDepth)

    lazy val VariableDefinitions: Parser[List[Ast.VariableDefinition]] =
      parens(VariableDefinition.rep0)

    lazy val VariableDefinition: Parser[Ast.VariableDefinition] =
      ((Variable <* punctuation(":")) ~ Type ~ DefaultValue.? ~ Directives.?).map {
        case (((v, tpe), dv), dirs) => Ast.VariableDefinition(v.name, tpe, dv, dirs.getOrElse(Nil))
      }

    lazy val Variable: Parser[Ast.Value.Variable] =
      punctuation("$") *> Name.map(Ast.Value.Variable.apply)

    lazy val DefaultValue: Parser[Ast.Value] =
      punctuation("=") *> Value

    def ListType(n: Int): Parser[Ast.Type.List] =
      squareBrackets(guard(n, "exceeded maximum list type depth")(TypeN)).map(Ast.Type.List.apply)

    lazy val namedMaybeNull: Parser[Ast.Type] = (NamedType ~ punctuation("!").?).map {
      case (t, None) => t
      case (t, _) => Ast.Type.NonNull(Left(t))
    }

    def listMaybeNull(n: Int): Parser[Ast.Type] = (ListType(n) ~ punctuation("!").?).map {
      case (t, None) => t
      case (t, _) => Ast.Type.NonNull(Right(t))
    }

    def TypeN(n: Int): Parser[Ast.Type] =
      namedMaybeNull | listMaybeNull(n)

    lazy val Type: Parser[Ast.Type] =
      TypeN(maxListTypeDepth)

    lazy val NamedType: Parser[Ast.Type.Named] =
      Name.map(Ast.Type.Named.apply)

    lazy val Directives: Parser0[List[Ast.Directive]] =
      Directive.rep0

    lazy val Directive: Parser[Ast.Directive] =
      punctuation("@") *> (Name ~ Arguments.?).map { case (n, ods) => Ast.Directive(n, ods.orEmpty)}

    lazy val Name: Parser[Ast.Name] =
      token(charIn(nameInitial) ~ charIn(nameSubsequent).rep0).map {
        case (h, t) => Ast.Name((h :: t).mkString)
      }

    def guard0[T](n: Int, msg: String)(p: Int => Parser0[T]): Parser0[T] =
      if (n <= 0) Parser.failWith(msg) else defer0(p(n-1))

    def guard[T](n: Int, msg: String)(p: Int => Parser[T]): Parser[T] =
      if (n <= 0) Parser.failWith(msg) else defer(p(n-1))
  }

  private object CommentedText {

    val whitespace: Parser[Char] = charWhere(_.isWhitespace)

    val skipWhitespace: Parser0[Unit] =
      charsWhile0(c => c.isWhitespace || c == ',').void

    /** Parser that consumes a comment */
    val comment: Parser[Unit] =
      (char('#') *> (charWhere(c => c != '\n' && c != '\r')).rep0 <* charIn('\n', '\r') <* skipWhitespace).void

    /** Turns a parser into one that skips trailing whitespace and comments */
    def token[A](p: Parser[A]): Parser[A] =
      p <* skipWhitespace <* comment.rep0

    def token0[A](p: Parser0[A]): Parser0[A] =
      p <* skipWhitespace <* comment.rep0

    /**
    * Consumes `left` and `right`, including the trailing and preceding whitespace,
    * respectively, and returns the value of `p`.
    */
    private def _bracket[A,B,C](left: Parser[B], p: Parser0[A], right: Parser[C]): Parser[A] =
      token(left) *> token0(p) <* token(right)

    /** Turns a parser into one that consumes surrounding parentheses `()` */
    def parens[A](p: Parser0[A]): Parser[A] =
      _bracket(char('('), p, char(')'))

    /** Turns a parser into one that consumes surrounding curly braces `{}` */
    def braces[A](p: Parser0[A]): Parser[A] =
      _bracket(char('{'), p, char('}'))

    /** Turns a parser into one that consumes surrounding square brackets `[]` */
    def squareBrackets[A](p: Parser0[A]): Parser[A] =
      _bracket(char('['), p, char(']'))
  }

  private object Literals {

    val stringLiteral: Parser[String] = {

      val lineTerminator: Parser[String] = (lf | cr | crlf).string

      val sourceCharacter: Parser[String] = (charIn(0x0009.toChar, 0x000A.toChar, 0x000D.toChar) | charIn(0x0020.toChar to 0xFFFF.toChar)).string

      val escapedUnicode: Parser[String] = string("\\u") *>
        hexdig
          .repExactlyAs[String](4)
          .map(hex => Integer.parseInt(hex, 16).toChar.toString)

      val escapedCharacter: Parser[String] = char('\\') *>
        (
          char('"').as("\"") |
            char('\\').as("\\") |
            char('/').as("/") |
            char('b').as("\b") |
            char('f').as("\f") |
            char('n').as("\n") |
            char('r').as("\r") |
            char('t').as("\t")
        )

      val stringCharacter: Parser[String] = (
        (not(charIn('"', '\\') | lineTerminator).with1 *> sourceCharacter) |
          escapedUnicode |
        escapedCharacter
      )

      val blockStringCharacter: Parser[String] = string("\\\"\"\"").as("\"\"\"") |
        (not(string("\"\"\"")).with1 *> sourceCharacter)

      //https://spec.graphql.org/June2018/#BlockStringValue()
      //TODO this traverses over lines a hideous number of times(but matching the
      //algorithm in the spec). Can it be optimized?
      val blockQuotesInner: Parser0[String] = blockStringCharacter.repAs0[String].map { str =>
        val isWhitespace: Regex = "[ \t]*".r
        var commonIndent: Int = -1
        var lineNum: Int = 0
        for (line <- str.linesIterator) {
          if (lineNum != 0) {
            val len = line.length()
            val indent = line.takeWhile(c => c == ' ' || c == '\t').length()
            if (indent < len) {
              if (commonIndent < 0 || indent < commonIndent) {
                commonIndent = indent
              }
            }
          }
          lineNum = lineNum + 1
        }
        val formattedReversed: List[String] = if ( commonIndent >= 0) {
          str.linesIterator.foldLeft[List[String]](Nil) {
            (acc, l) => if (acc == Nil) l :: acc else l.drop(commonIndent) :: acc
          }
        } else {
          str.linesIterator.toList
        }
        val noTrailingEmpty = formattedReversed.dropWhile(isWhitespace.matches(_)).reverse
        noTrailingEmpty.dropWhile(isWhitespace.matches(_)).mkString("\n")
      }


      (not(string("\"\"\"")).with1 *> stringCharacter.repAs0[String].with1.surroundedBy(char('"'))) | blockQuotesInner.with1.surroundedBy(string("\"\"\""))

    }

    val intLiteral: Parser[Int] =
      bigInt.flatMap {
        case v if v.isValidInt => pure(v.toInt)
        case v => failWith(s"$v is larger than max int")
      }

    val booleanLiteral: Parser[Boolean] = string("true").as(true) | string("false").as(false)

  }
}
