package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /*
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    i match {
      case Int.MinValue => (0, r)
      case v => (math.abs(v), r)
    }
  }
  */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /*
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    ((i.toDouble + Int.MaxValue + 1) / (2 * Int.MaxValue.toDouble + 1), r)
  }
  */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (n, rng2) => (n % 2 == 0, rng2) }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft[(List[Int], RNG)]((Nil, rng)) { (acc, _) =>
      val (i, r) = acc._2.nextInt
      (i :: acc._1, r)
    }
  }

  def double2(rng: RNG): Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (x1, r1) = ra(rng)
      val (x2, r2) = rb(r1)
      (f(x1, x2), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case f +: Nil =>
      rng => {
        val (x, r) = f(rng)
        (List(x), r)
      }
    case f +: t =>
      rng => {
        val (xs, r) = sequence(t)(rng)
        val (x, r2) = f(r)
        (x +: xs, r2)
      }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ +: _))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, r) = f(rng)
      g(x)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapByFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(x => map(rb)(y => f(x, y)))

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (a2, s3) = sb.run(s2)
      (f(a, a2), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a1, s1) = run(s)
      f(a1).run(s1)
    })

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s1: S): State[S, Unit] = State(_ => ((), s1))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()


  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ +: _))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    /*
    val coinAction = State[Machine, (Int, Int)] {
      case Machine(true, ca, co) if ca > 0 => ((co + 1, ca), Machine(locked = false, ca, co + 1))
      case s@Machine(false, ca, co) => ((co, ca), s)
      case s@Machine(_, ca, co) if ca < 1 => ((co, ca), s)
    }

    val turnAction = State[Machine, (Int, Int)] {
      case Machine(false, ca, co) => ((co, ca - 1), Machine(locked = true, ca - 1, co))
      case s@Machine(true, ca, co) => ((co, ca), s)
    }


    inputs.map {
      case Coin => coinAction
      case Turn => turnAction
    }.reduceLeft { (x, y) =>
      x flatMap (_ => y)
    }
    */

    def update(i: Input)(m: Machine): Machine = (i, m) match {
      case (_, Machine(_, ca, _)) if ca < 1 => m
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Coin, Machine(true, ca, co)) if ca > 0 => Machine(false, ca, co + 1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
    }

    for {
      _ <- sequence(inputs.map((modify[Machine] _).compose(update)))
      m <- get
    } yield (m.coins, m.candies)

  }


  def main(args: Array[String]): Unit = {
    val randInt = State[RNG, Int](s => s.nextInt)
    println((for {
      x <- State(RNG.nonNegativeInt)
      y <- State(RNG.nonNegativeInt)
    } yield(x, y)).run(RNG.Simple(123)))
    //println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)) run Machine(locked = true, 5, 10))
  }
}
