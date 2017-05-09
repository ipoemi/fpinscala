package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t())
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] = this.foldRight[Option[A]](None)((a, b) => Some(a) orElse b)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](empty)((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    this.foldRight[Stream[B]](b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight[Stream[B]](empty)((h, t) => f(h) append t)

  def map2[B](f: A => B): Stream[B] =
    unfold[B, Stream[A]](this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def take2(n: Int): Stream[A] = {
    unfold[A, (Int, Stream[A])]((n, this)) {
      case (x, _) if x <= 0 => None
      case (x, Cons(h, t)) => Some((h(), (x - 1, t())))
      case (x, Empty) => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold[A, Stream[A]](this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](xs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold[C, (Stream[A], Stream[B])]((this, xs)) {
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](xs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, xs)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Empty, _) => false
    case (_, Empty) => true
    case (Cons(_, t1), Cons(_, t2)) => t1().startsWith(t2())
  }

  def startsWith2[B](s: Stream[B]): Boolean =
    zipWith(s)((_, _)).forAll(x => x._1 == x._2)

  def tails: Stream[Stream[A]] =
    cons(this, unfold[Stream[A], Stream[A]](this) {
      case Empty => None
      case Cons(_, t) => Some((t(), t()))
    })

  /*
   * not O(n)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    cons(this.foldRight(z)(f), unfold[B, Stream[A]](this) {
      case Empty => None
      case Cons(h, t) => Some((t().foldRight(z)(f), t()))
    })
  }
  */

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight[Stream[B]](Stream(z)) { (x, acc) =>
      acc match {
        case Cons(h, t) => cons(f(x, h()), acc)
      }
    }

  def toList: List[A] = {
    def loop(xs: Stream[A]): List[A] =
      xs match {
        case Empty => Nil
        case Cons(h, t) => h() +: t().toList
      }

    loop(this)
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val ns: Stream[A] = cons(a, ns)
    ns
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  val fibs: Stream[Int] = {
    def aux(prev: Int, cur: Int): Stream[Int] = {
      cons(prev, aux(cur, prev + cur))
    }

    aux(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  val fibs2: Stream[Int] = unfold(0, 1)(a => Some((a._1, (a._2, a._1 + a._2))))

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def constant2[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  val ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).tails.toList.map(_.toList))
    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}