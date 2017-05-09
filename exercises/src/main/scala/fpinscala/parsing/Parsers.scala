package fpinscala.parsing

import language.{higherKinds, implicitConversions}
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[ParseError, Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def string(s: String): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] =
     map2(p, many(p))(_ :: _) | succeed(List())

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    //product(p1, p2).map(x => f(x._1, x._2))
    product(p1, p2).map(f.tupled)

  /*
  def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C]
  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    map2(p1, p2)((_, _))
  */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p.many)(_ :: _)

  def or[A](a1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def asParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = new ParserOps(f(a))

  /*
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = or(p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
  */

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = or(p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = p.product(p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def equal[A](p1: Parser[A], r: Either[ParseError, A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == r)

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(x => x))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      equal(succeed(a), Right(a))(in)

    def charLaw(c: Char): Prop =
      check(run(char(c))(c.toString) == Right(c))

    def stringLaw[A](s: String): Prop =
      check(run(string(s))(s) == Right(s))

    def orLaw[A]: Prop =
      check(run("abra" | "cadabra")("abra") == Right("abra")) &&
        check(run("abra" | "cadabra")("cadabra") == Right("cadabra"))

    def listOfNLaw[A]: Prop =
      check(run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab", "ab", "cad"))) &&
        check(run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab"))) &&
        check(run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab")))

    def productLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(f: A => C, g: B => C)(in: Gen[String]): Prop = {
      def unbiasL(p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
      def unbiasR(p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in) &&
      equal((a map f) ** (b map g), a ** b map { case (x, y) => (f(x), g(y)) })(in)
    }
  }

  val numA: Parser[Int] = char('a').many.map(_.size)
  val manyAB: Parser[(Int, Int)] = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

object Parsers {
  def main(args: Array[String]): Unit = {
  }
}