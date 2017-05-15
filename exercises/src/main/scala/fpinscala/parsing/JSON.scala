package fpinscala.parsing

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{token, whitespace, surround, sep, quoted, singleQuoted, double, asParser, regex, ParserOps}

    implicit def string2token(s: String): Parser[String] = token(P.string(s))

    val a: Parser[String] = "aa"

    def array = surround("[", "]") {
      (value sep ",") map (xs => JArray(xs.toIndexedSeq))
    }

    def obj = surround("{", "}") {
      ((quoted | singleQuoted) ** (":" *> value) sep ",") map (xs => JObject(Map(xs: _*)))
    }

    def literal = "null".as(JNull) |
      double.map(JNumber) |
      quoted.map(JString) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))

    def value: Parser[JSON] = literal | obj | array

    whitespace *> value
  }

  def main(args: Array[String]): Unit = {
    import MyParser._
    import MyParserType.MyParser
    val json: MyParser[JSON] = jsonParser(MyParser)

    val test1 = surround("[", "]")("*" sep ",")
    val test2 = surround("[", "]")("+" sep ",")

    println(run(attempt(test1) | test2)("[+ , +]"))
  }
}
