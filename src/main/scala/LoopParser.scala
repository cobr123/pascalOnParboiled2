/**
 * Created by cobr123 on 21.09.15.
 */

import org.parboiled2._

class LoopParser(val input: ParserInput) extends Parser {
  def InputText = rule {
    statements ~ EOI
  }

  //pascal ebnf
  def compoundStatement = rule {
    "begin" ~ zeroOrMore(statements) ~ "end"
  }

  def statements = rule {
    oneOrMore(statement ~ ";")
  }
  def label = rule {
    capture(Digits) ~> (_.toInt)
  }

  def statement = rule {
    label ~ ":" ~ unlabelledStatement | unlabelledStatement
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
    "if" ~ expression ~ "then" ~ statement ~ opt("else" ~ statement)
  }

  def caseStatement = rule {
    "case" ~ expression ~ "of" ~ caseListElement ~ rep(";" ~ caseListElement) ~ ((";" ~ "else" ~ statements) ?) ~ "end"
  }

  def caseListElement = rule {
    constList ~ ":" ~ statement
  }

  def constList = rule {
    constant ~ rep("," ~ constant)
  }

  def constantDefinitionPart = rule {
    "const" ~ constantDefinition ~ rep(";" ~ constantDefinition) ~ ";"
  }

  def constantDefinition = rule {
    identifier ~ "=" ~ constant
  }

  def constantChr = rule {
    "chr" ~ "(" ~ unsignedInteger ~ ")"
  }

  def constant = rule {
    unsignedNumber | identifier | string | constantChr
  }

  def string = rule {
    stringLiteral
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

  def forList = rule {initialValue ~ ("to" | "downto") ~ finalValue

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
    variable ~ rep("," ~ variable)
  }

  def simpleStatement = rule {
    assignmentStatement | procedureStatement | gotoStatement | emptyStatement
  }

  def emptyStatement = rule {
    """[(\r?\n)\s]+""".r
  }

  def procedureStatement = rule {
    identifier ~ opt("(" ~ parameterList ~ ")")
  }

  def parameterList = rule {
    actualParameter ~ rep("," ~ actualParameter)
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

  /** A variable is an id with a suffix and can look like:
    * id
    * id[expr,...]
    * id.id
    * id.id[expr,...]
    * id^
    * id^.id
    * id.id[expr,...]
    * ...
    *
    * LL has a really hard time with this construct as it's naturally
    * left-recursive.  We have to turn into a simple loop rather than
    * recursive loop, hence, the suffixes.  I keep in the same rule
    * for easy tree construction.
    */
  def variable = rule {
    ("@" ~ identifier // AT is root of identifier; then other op becomes root
      | identifier
      ) ~ rep("[" ~ expression ~ rep("," ~ expression) ~ "]" | "(." ~ expression ~ rep("," ~ expression) ~ ".)" | "." ~ identifier | "^")
  }

  def expression = rule {
    simpleExpression ~ rep(("=" | "<>" | "<" | "<=" | ">=" | ">" | "in") ~ simpleExpression)
  }

  def simpleExpression = rule {
    term ~ rep(("+" | "-" | "or") ~ term)
  }

  def term = rule {
    signedFactor ~ rep(("*" | "/" | "div" | "mod" | "and") ~ signedFactor)
  }

  def signedFactor = rule {
    opt("+" | "-") ~ factor
  }

  def factor = rule {
    variable | "(" ~ expression ~ ")" | functionDesignator | unsignedConstant | set | "not" ~ factor
  }

  def set = rule {
    "[" ~ elementList ~ "]" | "(." ~ elementList ~ ".)"
  }

  def elementList = rule {
    element ~ rep("," ~ element)
  }

  def element = rule {
    expression ~ opt(".." ~ expression)
  }

  def unsignedConstant = rule {
    unsignedNumber | constantChr | string | "nil"
  }

  def functionDesignator = rule {
    identifier ~ "(" ~ parameterList ~ ")"
  }
}

object TestLoopParser extends App {
  //val cmd = """i := 0;"""
  // val cmd = """showMessage(avAttr[i])""
  //  val cmd = """begin end;"""
  val cmd = """begin for i := 0 to 10 do begin showMessage(avAttr[i]); end;end;"""
  println(new LoopParser(cmd).InputText.run()) // evaluates to `scala.util.Success(2)
}
