package fpinscala.parsing

import fpinscala.parsing.MyParserType._
import fpinscala.testing.Prop._
import fpinscala.testing._

import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex


trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, manyAccum(p))(_ :: _) | succeed(List())

  final def manyAccum[A](p: Parser[A]): Parser[List[A]] =
    map2(p, manyAccum(p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {a <- p1; b <- p2} yield (a, b)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
  //product(p1, p2).map(x => f(x._1, x._2))
  //map(product(p1, p2))(f.tupled)
    for {a <- p1; b <- p2} yield f(a, b)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def skipL[A](p: Parser[Any], p2: => Parser[A]): Parser[A] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  def whitespace: Parser[String] = "\\s*".r

  def digits: Parser[String] = "\\d+".r

  def endBy(s: String): Parser[String] = (".*?" + Regex.quote(s)).r

  def quoted: Parser[String] = token("\"" *> endBy("\"").map(_.dropRight(1)))

  def singleQuoted: Parser[String] = token("'" *> endBy("'").map(_.dropRight(1)))

  def surround[A](s: Parser[Any], e: Parser[Any])(p: => Parser[A]): Parser[A] =
    s *> p <* e

  def doubleString: Parser[String] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString map (_.toDouble)

  def sep[A](p: Parser[A], sp: Parser[Any]): Parser[List[A]] =
    sep1(p, sp) or succeed(List())

  def sep1[A](p: Parser[A], sp: Parser[Any]): Parser[List[A]] =
    map2(p, many(whitespace *> sp *> whitespace *> p))(_ :: _)

  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] =
    map2(p, many(op ** p))((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /*
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C]
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2(p1, p2)((_, _))
  */

  /*
  def delay[A](p: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] =
     map2(p, delay(many(p)))(_ :: _) | succeed(List())
  */


  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p.many)(_ :: _)

  /*
  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    ps.foldLeft(succeed(List[A]())) { (r, p) => for { t <- r; h <- p } yield h :: t } map (_.reverse)
  */

  def or[A](a1: Parser[A], s2: => Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  implicit def asParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = new ParserOps(f(a))

  /*
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = or(p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
  */

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = or(p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)

    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p, separator)

    def opL(op: Parser[(A, A) => A]): Parser[A] = self.opL(p)(op)

    def as[B](b: B): Parser[B] = self.map(p.slice)(_ => b)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

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
      def unbiasL(p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

      def unbiasR(p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in) &&
        equal((a map f) ** (b map g), a ** b map { case (x, y) => (f(x), g(y)) })(in)
    }

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => e.latest.get._2 == msg
          case _ => true
        }
      }
  }

  val numA: Parser[Int] = char('a').many.map(_.size)
  val manyAB: Parser[(Int, Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)
  val csgExam: Parser[String] = regex("[0-9]".r).flatMap(n => listOfN(n.toInt, char('a')).slice)

}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(msg: String): ParseError =
    ParseError(latestLoc.map((_, msg)).toList)

  def latestLoc: Option[Location] = latest.map(_._1)

  def latest: Option[(Location, String)] = stack.lastOption
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

}

object MyParserType {
  type MyParser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {
    def advanceBy(n: Int): ParseState = copy(loc = loc.copy(offset = loc.offset + n))

    def input: String = loc.input.substring(loc.offset)

    def slice(n: Int): String = {
      //println(s"loc=$loc, n=$n")
      loc.input.substring(loc.offset, loc.offset + n)
    }
  }

  trait Result[+A] {
    def map[B](f: (A, Int) => (B, Int)): Result[B] = this match {
      case Success(s, c) => Function.tupled(Success.apply[B] _)(f(s, c))
      case e@Failure(_, _) => e
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case a => a
    }

    def addCommit(b: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || b)
      case a => a
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, commited = false)
      case s => s
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, c) => Success(a, n + c)
      case b => b
    }

  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, commited: Boolean) extends Result[Nothing]

}

object MyParser extends Parsers[MyParser] {
  def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = {
    val state = ParseState(Location(input))
    p(state) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }
  }

  override def many[A](p: MyParser[A]): MyParser[List[A]] = s => {
    @tailrec
    def go(xs: List[A], cnt: Int): MyParserType.Result[List[A]] =
      p(s.advanceBy(cnt)) match {
        case Success(w, c) => go(w :: xs, c + cnt)
        case f@Failure(_, true) => f
        case Failure(_, _) => Success(xs, cnt)
      }

    go(List(), 0)
  }


  def firstNonmatchingIndex(in: String, t: String): Int = {
    //println(s"in.length = ${in.length}")
    if (in.length == 0) 0
    else {
      val length = t.length
      val inStream = t.toStream.map(Some(_)).take(length)
      val tStream = in.toStream.map(Some(_)).take(length)
      val zipped = inStream.zipAll(tStream, None, None)
      val xs = zipped.zipWithIndex.dropWhile(x => x._1._1 == x._1._2)
      //println(xs.toList)
      if (xs.isEmpty) -1
      else xs.head._2
    }
  }

  implicit def string(w: String): MyParser[String] = {
    val msg = "Not Found: '" + w + "'"
    s => {
      //println(s"s.input = ${s.input}, w = $w")
      val i = firstNonmatchingIndex(s.input, w)
      if (i == -1)
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  implicit def regex(r: Regex): MyParser[String] = s => {
    r.findPrefixOf(s.input) match {
      case Some(w) => Success(w, w.length)
      case _ => Failure(s.loc.toError(s"Not Found: $r"), commited = false)
    }
  }

  override def succeed[A](a: A): MyParser[A] = _ => Success(a, 0)

  def slice[A](p: MyParser[A]): MyParser[String] = s => {
    p(s) match {
      case Success(w, cnt) => Success(s.slice(cnt), cnt)
      case e@Failure(_, _) => e
    }
  }

  def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = s =>
    p(s) match {
      case Success(a, n) => f(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case e@Failure(_, _) => e
    }

  def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = s =>
    p1(s) match {
      case Failure(_, false) => p2(s)
      case a => a
    }

  def label[A](msg: String)(p: MyParser[A]): MyParser[A] = s => {
    p(s).mapError(_.label(msg))
  }

  def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = s => {
    p(s).mapError(x => x.push(s.loc, msg))
  }

  def attempt[A](p: MyParser[A]): MyParser[A] = s => p(s).uncommit
}
