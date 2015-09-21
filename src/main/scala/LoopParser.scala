/**
 * Created by cobr123 on 21.09.15.
 */

import org.parboiled2._

class LoopParser(val input: ParserInput) extends Parser {
  def InputText: Rule1[Any] = rule {
    push(statements) ~ EOI
  }

  //pascal ebnf
  def compoundStatement = rule {
    atomic(ignoreCase("begin")) ~ zeroOrMore(statements) ~ atomic(ignoreCase("end"))
  }

  def ws = rule {
    zeroOrMore(anyOf(" \t\r\n"))
  }

  def statements = rule {
    oneOrMore(push(statement) ~ ";")
  }

  def identifier = rule {
    oneOrMore(CharPredicate.AlphaNum)
  }

  def label = rule {
    oneOrMore(CharPredicate.Digit)
  }

  def unsignedInteger = rule {
    oneOrMore(CharPredicate.Digit)
  }

  def unsignedNumber = rule {
    oneOrMore(CharPredicate.Digit)
  }


  def statement: Rule2[Any, Any] = rule {
    optional(push(label) ~ ":") ~ push(unlabelledStatement)
  }

  def unlabelledStatement = rule {
    simpleStatement | structuredStatement
  }

  def structuredStatement = rule {
    compoundStatement | conditionalStatement | repetetiveStatement | withStatement
  }

  def conditionalStatement = rule {
    ifStatement | caseStatement
  }

  def ifStatement = rule {
    "if" ~ expression ~ "then" ~ statement ~ optional("else" ~ statement)
  }

  def caseStatement = rule {
    "case" ~ expression ~ "of" ~ oneOrMore(caseListElement).separatedBy(";") ~ optional(";" ~ "else" ~ statements) ~ "end"
  }

  def caseListElement = rule {
    constList ~ ":" ~ statement
  }

  def constList = rule {
    oneOrMore(constant).separatedBy(",")
  }

  //  def constantDefinitionPart = rule {
  //    "const" ~ constantDefinition ~ zeroOrMore(";" ~ constantDefinition) ~ ";"
  //  }
  //
  //  def constantDefinition = rule {
  //    identifier ~ "=" ~ constant
  //  }

  def constantChr = rule {
    "chr" ~ "(" ~ unsignedInteger ~ ")"
  }

  def constant = rule {
    unsignedNumber | identifier | string | constantChr
  }

  def string = rule {
    "'" ~ zeroOrMore(CharPredicate.Printable | "''") ~ "'"
  }

  def repetetiveStatement = rule {
    whileStatement | repeatStatement | forStatement
  }

  def whileStatement = rule {
    "while" ~ expression ~ "do" ~ statement
  }

  def repeatStatement = rule {
    "repeat" ~ statements ~ "until" ~ expression
  }

  def forStatement = rule {
    "for" ~ identifier ~ ":=" ~ forList ~ "do" ~ statement
  }

  def forList = rule {
    initialValue ~ ("to" | "downto") ~ finalValue
  }

  def initialValue = rule {
    expression
  }

  def finalValue = rule {
    expression
  }

  def withStatement = rule {
    "with" ~ recordVariableList ~ "do" ~ statement
  }

  def recordVariableList = rule {
    oneOrMore(variable).separatedBy(",")
  }

  def simpleStatement = rule {
    assignmentStatement | procedureStatement | gotoStatement
  }

  def procedureStatement = rule {
    identifier ~ optional("(" ~ parameterList ~ ")")
  }

  def parameterList = rule {
    oneOrMore(actualParameter).separatedBy(",")
  }

  def actualParameter = rule {
    expression
  }

  def gotoStatement = rule {
    "goto" ~ label
  }

  def assignmentStatement = rule {
    variable ~ ":=" ~ expression
  }

  //  /** A variable is an id with a suffix and can look like:
  //    * id
  //    * id[expr,...]
  //    * id.id
  //    * id.id[expr,...]
  //    * id^
  //    * id^.id
  //    * id.id[expr,...]
  //    * ...
  //    *
  //    * LL has a really hard time with this construct as it's naturally
  //    * left-recursive.  We have to turn into a simple loop rather than
  //    * recursive loop, hence, the suffixes.  I keep in the same rule
  //    * for easy tree construction.
  //    */
  def variable = rule {
    ("@" ~ identifier
      | identifier
      ) ~ zeroOrMore(set | variable_part_dot | "^")
  }

  def variable_part_dot = rule {
    "." ~ identifier
  }

  def expression_part = rule {
    oneOrMore(("=" | "<>" | "<" | "<=" | ">=" | ">" | "in") ~ simpleExpression)
  }

  def expression = rule {
    simpleExpression ~ optional(expression_part)
  }

  def simpleExpression_part = rule {
    oneOrMore(("+" | "-" | "or") ~ term)
  }

  def simpleExpression = rule {
    term ~ optional(simpleExpression_part)
  }

  def term = rule {
    signedFactor ~ zeroOrMore(("*" | "/" | "div" | "mod" | "and") ~ signedFactor)
  }

  def signedFactor = rule {
    optional("+" | "-") ~ factor
  }

  def factor:Rule1[Any] = rule {
    variable | factorInParen | functionDesignator | unsignedConstant | set | notFactor
  }

  def factorInParen = rule {
    "(" ~ expression ~ ")"
  }

  def notFactor = rule {
    "not" ~ factor
  }

  def set = rule {
    "[" ~ elementList ~ "]" | "(." ~ elementList ~ ".)"
  }

  def elementList = rule {
    oneOrMore(element).separatedBy(",")
  }

  def element = rule {
    oneOrMore(expression).separatedBy("..")
  }

  def unsignedConstant = rule {
    unsignedNumber | constantChr | string | "nil"
  }

  def functionDesignator = rule {
    identifier ~ "(" ~ parameterList ~ ")"
  }
}

object TestLoopParser extends App {
  val cmd = """i := 0;"""
  // val cmd = """showMessage(avAttr[i])""
  //  val cmd = """begin end;"""
  //val cmd = """begin for i := 0 to 10 do begin showMessage(avAttr[i]); end;end;"""
  println(new LoopParser(cmd).InputText.run()) // evaluates to `scala.util.Success(2)
}
