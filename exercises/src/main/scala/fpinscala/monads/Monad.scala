package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

import language.{higherKinds, reflectiveCalls}


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    (lma foldRight unit(List.empty[A])) {
      map2(_, _)(_ :: _)
    }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    (la foldRight unit(List.empty[B])) ((a, m) => map2(f(a), m)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  /*
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(List[A]())
    case h :: t => flatMap(f(h)) { b =>
      if (b) map(filterM(t)(f))(mt => h :: mt)
      else filterM(t)(f)
    }
  }
  */

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List[A]()))((h, mt) => flatMap(f(h)) { b =>
      if (b) map(mt)(h :: _)
      else mt
    })

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  def __compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(b => g(b)))

}

case class Reader[R, A](run: R => A) {
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
    Reader(r => {
      f(run(r)).run(r)
    })

  def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))

  //def map[B](f: A => B) = flatMap(x => Reader(r => f(x)))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)

    def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f

    def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

  /*
  def stateMonad[A]: Monad[State] = new Monad[State] {
    def unit[S](a: => A): State[S, A] = ???

    def flatMap[S, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ???
  }
  */

  /*
  class StateMonad[S] {
    type StateT[A] = State[S, A]

    val monad = new Monad[StateT] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
    }
  }

  val stateMonad2 = new StateMonad[Int].monad
  */

  def stateMonad[S] = new Monad[({type F[A] = State[S, A]})#F] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)

    def unit[A](a: => A): Id[A] = Id(a)
  }

  def readerMonad[R] = ???

}


case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = Id(f(value).value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(r => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      st flatMap f
  }
}

