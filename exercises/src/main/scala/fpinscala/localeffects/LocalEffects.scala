package fpinscala.localeffects

import fpinscala.monads._

import scala.collection.mutable.HashMap
import scala.language.postfixOps

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray

    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }

    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S, A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
  //xs.keys.foldLeft(ST[S, Unit](())) { (acc, k) =>
  //  acc flatMap { _ =>
  //    write(k, xs(k))
  //  }
  //}
    xs.foldRight(ST[S, Unit](())) { case ((k, v), st) => st flatMap (_ => write(k, v)) }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
}

sealed trait STMap[S, K, V] {
  protected def table: HashMap[K, V]

  def size: ST[S, Int] = ST(table.size)

  // Get the value under a key
  def apply(k: K): ST[S, V] = ST(table(k))

  // Get the value under a key, or None if the key does not exist
  def get(k: K): ST[S, Option[V]] = ST(table.get(k))

  // Add a value under a key
  def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)

  // Remove a key
  def -=(k: K): ST[S, Unit] = ST(table -= k)
}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val table = HashMap.empty[K, V]
  })

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val table = (HashMap.newBuilder[K, V] ++= m).result
  })
}

object Immutable {
  def noop[S] = ST[S, Unit](())

  def foreach[S, A](l: Stream[A])(f: A => ST[S, Unit]): ST[S, Unit] = l match {
    case h #:: t => f(h) flatMap (_ => foreach(t)(f))
    case _ => noop
  }

  /*
  def partition[S](arr: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    pivotVal <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    jr <- STRef[S, Int](l)
    _ <- foreach[S, Int](l until r toStream)(i => for {
      iv <- arr.read(i)
      _ <- if (iv < pivotVal) for {
        j <- jr.read
        _ <- arr.swap(i, j)
        _ <- jr.write(j + 1)
      } yield () else noop[S]
    } yield ())
    j <- jr.read
    _ <- arr.swap(j, r)
  } yield j
  */
  def partition[S](arr: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    pivotVal <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    jr <- STRef[S, Int](l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      iv <- arr.read(i)
      _ <- if (iv < pivotVal) for {
        j <- jr.read
        _ <- arr.swap(i, j)
        _ <- jr.write(j + 1)
      } yield () else noop[S]
    } yield ())
    j <- jr.read
    _ <- arr.swap(j, r)
  } yield j

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield () else noop[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
}

import scala.collection.mutable.HashMap

object Main {
  def main(args: Array[String]): Unit = {
    def p[T] = new RunnableST[STRef[_, Int]] {
      def apply[S] = for {
        r1 <- STRef(1)
      } yield r1
    }

    val stref = ST.runST(p)

    //val p2 = new RunnableST[STRef[_, Int]] {
    //  def apply[S] = for {
    //    r1 <- stref
    //  } yield r1
    //}
    //val stref2 = ST.runST(p2)

    //println(Immutable.quicksort(List(5, 2, 3)))
  }
}
