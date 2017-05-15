package fpinscala.testing

import java.util.concurrent.Executors

import fpinscala.laziness.Stream
import fpinscala.state._
import Prop._
import Gen._
import SGen._
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par

import scala.util.Try

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (m, n, rng) =>
    this.run(m, n, rng) match {
      case Passed | Proved => p.run(m, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop { (m, n, rng) =>
    this.run(m, n, rng) match {
      case Falsified(f, c) => p.tag(f).run(m, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop { (m, n, rng) =>
    this.run(m, n, rng) match {
      case Falsified(f, c) => Falsified(s"$msg \n$f", c)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
  type MaxSize = Int

  implicit class ParOps[A](a: Par[A]) {
    def ===(b: Par[A]): Par[Boolean] = equal(a, b)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar[A](p: Par[Boolean]): Prop = {
    forAllPar(Gen.unit(()))(_ => p)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  val ints2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map { xs =>
    xs.foldLeft(Par.unit(0)) { (p, x) =>
      Par.fork(Par.map(p)(_ + x))
    }
  }

  val p4 = forAllPar(ints2)(n => equal(Par.fork(n), n))

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def apply(run: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => run(n, rng) }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, passed proved property.")
    }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  /*
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State(s => {
    val (a, s2) = sample.run(s)
    f(a).sample.run(s2)
  }))
  */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x => f(x).sample))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(b.sample)(f))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))
}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(a.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State[RNG, Int](RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
  }

  def union[A](a: Gen[A], b: Gen[A]): Gen[A] = boolean.flatMap(x => if (x) a else b)

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Prob = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double).flatMap { d =>
      if (d < g1Prob) g1._1.sample
      else g2._1.sample
    })
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }


  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    val maxProp1 = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    //run(maxProp)
    run(maxProp1)

    val intList = listOf(Gen.choose(-100, 100))
    val sortProp = forAll(intList) { ns =>
      val nss = ns.sorted
      (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists(x => x._1 > x._2)) &&
        !nss.exists(!ns.contains(_)) &&
        !ns.exists(!nss.contains(_))
    }

    run(sortProp)

    val boolProp = forAll(Gen.boolean) { b =>
      if (b) b else !b
    }

    run(boolProp)

    //s.toSet.takeWhile(f).forall(x => !s.toSet.dropWhile(f).exists(x))
    //s.takeWhile(f) ++ s.dropWhile(f) == s

  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen {
    forSize(_).map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i =>
    forSize(i).flatMap(x => f(x).forSize(i))
  }

  def listOfN(size: SGen[Int]): SGen[List[A]] = SGen { i =>
    forSize(i).listOfN(size.forSize(i))
  }

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { n =>
    g.listOfN(n)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { n =>
    //g.listOfN(n + 1)
    g.listOfN(n max 1)
  }
}


