package fpinscala
package applicative

import monads.Functor
import state._
import State._

//import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions
import language.postfixOps
import language.reflectiveCalls

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
    }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply[A, B => C](unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply[A, B => C => D](unit(f.curried))(fa))(fb))(fc)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, x) => f(x))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply[A, B](unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]())) {
      case ((k, fv), fm) => map2(fv, fm)((v, m) => m.updated(k, v))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  self =>

  override def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
    }
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma flatMap f


    def unit[A](a: => A): Either[E, A] = Right(a)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] =
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

      override def flatMap[A, B](fna: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
      /*
      F.flatMap[N[A], N[B]](fna) { na => // F[N[B]]
        F.map(T.sequence[F, N[B]](N.map(na)(f)))(nna => N.flatMap(nna)(x => x))
      }
      */
        F.flatMap(fna)(na => F.map(T.traverse(na)(f))(N.join))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

    def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
          case (fa1@Failure(_, _), _) => fa1
          case (_, fa2@Failure(_, _)) => fa2
        }

      override def unit[A](a: => A): Validation[E, A] = Success(a)
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    /*
    val G = Applicative.optionApplicative
    traverse[Option, A, B](fa)(a => Some(f(a)))(G).get
    */
    type Id[T] = T

    val idMonad = new Monad[Id] {
      def unit[A](a: => A): A = a

      override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)
    }

    traverse[Id, A, B](fa)(f)(idMonad)
  }

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
  //mapAccum(fa, ())((a, s) => (a, s))._1
    mapAccum(fa, toList(fa).reverse)((a, s) => (s.head, s.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((a, s), f(s, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_] : Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        self.traverse(fa)(ga => G.traverse(ga)(f))
    }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      fa.foldRight(G.unit(List[B]()))((a, gbs) => G.map2(f(a), gbs)(_ :: _))
    }

  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
      fa match {
        case Some(v) => G.map(f(v))(Some(_))
        case _ => G.unit(None)
      }
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val G = implicitly[Applicative[G]]
      fa match {
        case Tree(head, tail) =>
          //G.map2(f(a), listTraverse.sequence(tailList.map(traverse(_)(f))))(Tree(_, _))
          G.map2(f(head), listTraverse.traverse(tail)(h => traverse(h)(f)))(Tree(_, _))
      }
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
/*
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
*/

object Main {
  def main(args: Array[String]): Unit = {

    /*
    case class Iteration[A](a: A, f: A => A, n: Int) {
      def foldMap[B](g: A => B)(M: Monoid[B]): B = {
        def iterate(n: Int, b: B, c: A): B =
          if (n <= 0) b else iterate(n - 1, M.op(b, g(c)), f(c))
        iterate(n, M.zero, a)
      }
    }

    val m = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    val a = Iteration[Int](1, x => x + 1, 10).foldMap(identity)(m)
    println(a)

    import Traverse._
    val f = List(1, 2, 3)
    val aa = listTraverse.traverseS(f)((a: Int) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1
    */

    import Traverse._

    println(listTraverse.reverse(List(1, 2, 3)))

    //println(listTraverse.map(List(1, 2, 3)) (x => x + 1))

    //println(listTraverse.reverse(List(1, 2, 3)) ++ listTraverse.reverse(List(4, 5, 6)))
    //println(listTraverse.reverse(List(1, 2, 3) ++ List(4, 5, 6)))

    case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
      def flatMap[B](f: A => M[Option[B]]): OptionT[M, B] =
        OptionT(M.flatMap(value) {
          case None => M.unit(None)
          case Some(a) => f(a)
        })
    }

    implicit val listMonad: Monad[List] = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
    }

    OptionT[List, Int](List(Some(1))).flatMap(n => List(Some(n)))

    //println(OptionT[List, Int](List.fill(10)(Some(1))).flatMap(n => OptionT(List.fill(n)(Some(1)))).value)

  }
}
