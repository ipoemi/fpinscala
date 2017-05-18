package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): (A) => A = a1 compose a2

    val zero: A => A = identity
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  //trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(gen ** gen ** gen) {
      case ((a1, a2), a3) =>
        m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3)) &&
          m.op(a1, m.zero) == a1 &&
          m.op(m.zero, a1) == a1
    }
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  //as.map(f).foldLeft(m.zero)(m.op)
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    /*
    val m = new Monoid[B => B] {
      def op(a1: B => B, a2: B => B): B => B = (b: B) => a1(a2(b))

      val zero: B => B = identity _
    }
    foldMap(as, m)((a: A) => (b: B) => f(a, b))(z)
    */
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    def zero: A = m.zero
  }


  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    /*
    val m = new Monoid[B => B] {
      def op(a1: B => B, a2: B => B): B => B = (b: B) => a1(a2(b))

      val zero: B => B = identity _
    }
    foldMap(as, m)((a: A) => (b: B) => f(b, a))(z)
    */
    foldMap(as, dual(endoMonoid[B]))((a: A) => (b: B) => f(b, a))(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length < 1) m.zero
    else if (as.length == 1) f(as.head)
    else {
      //val half = as.length / 2
      //m.op(foldMapV(as.slice(0, half), m)(f), foldMapV(as.slice(half, as.length), m)(f))
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    /*
    val m = new Monoid[(Int, Boolean)] {
      def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) = {
        val bool =
          if (!a1._2 || !a2._2) false
          else if (a1._1 <= a2._1) true
          else false
        (a1._1, bool)
      }
      def zero = (Int.MaxValue, true)
    }
    */
    type OrderType = Option[(Int, Int, Boolean)]
    val m = new Monoid[OrderType] {
      def op(a1: OrderType, a2: OrderType): OrderType = (a1, a2) match {
        case (Some((min1, max1, prev1)), Some((min2, max2, prev2))) =>
          if ((!prev1 || !prev2) && max1 <= min2) Some((min1, max2, true))
          else Some((min1, max2, false))
        case (a, None) => a
        case (None, a) => a
      }

      val zero = None
    }
    foldMapV(ints, m)(x => Some((x, x, true))) forall (_._3)
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    //def op(p1: Par[A], p2: Par[A]): Par[A] = Par.flatMap(p1)(x => Par.map(p2)(y => m.op(x, y)))
    def op(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.op)

    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  //foldMapV(v, par(m))(x => Par.lazyUnit(f(x))) // Fold Process on Main Thread
    Par.parMap(v)(f).flatMap { xs =>
      foldMapV(xs, par(m))(x => Par.lazyUnit(x))
    }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(w1), Stub(w2)) => Stub(w1 + w2)
      case (Stub(w1), Part(l, c, r)) => Part(w1 + l, c, r)
      case (Part(l, c, r), Stub(w2)) => Part(l, c, r + w2)
      //case (Part(l1, c1, r1), Part(l2, c2, r2)) => Part(l1, c1 + c2, r2)
      case (Part(l1, c1, r1), Part(l2, c2, r2)) => Part(l1, c1 + c2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }

    def zero = Stub("")
  }

  def count(s: String): Int = {
    /*
     * Stupid !!
     *
    foldMapV(s.toList.toIndexedSeq, wcMonoid)(x => Stub(x.toString)) match {
      case Stub(_) => 1
      case Part(_, c, _) => c + 2
    }
    */
    def c2wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def cntStub(w: String): Int = w.length min 1

    foldMapV(s, wcMonoid)(c2wc) match {
      case Stub(w) => cntStub(w)
      case Part(l, cnt, r) => cnt + cntStub(l) + cntStub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))

    def zero: (A) => B = _ => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (m, k) =>
        m.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }

    def zero: Map[K, V] = Map()
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(x => Map(x -> 1))

}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((h, t) => h :: t)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).fold(mb.zero)(mb.op)

}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    //as.map(f).fold(mb.zero)(mb.op)
    foldMapV(as, mb)(f)

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  import Monoid._

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v) => f(v)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    //foldMap(as)(a => (b: B) => f(b, a))(endoMonoid)(z)
    as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }


  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    //foldMap(as)(f.curried)(dual(endoMonoid))(z)
    as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).fold(mb.zero)(mb.op(mb.zero, _))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

object Main {
  def main(args: Array[String]): Unit = {
    import Monoid.ordered

    import fpinscala.testing.Prop._
    import fpinscala.testing.Gen._

    //println(ordered(List(1, 8, 3, 9).toIndexedSeq))

    forAll(choose(-500, 500).listOfN(choose(0, 100)))(xs => if (ordered(xs.toIndexedSeq)) xs == xs.sorted else true)

    val test: (Int => Unit) = println(_)
    val test2: (Int => Unit) => Unit = println(_)

    test(1)
    test2(test)
  }
}
