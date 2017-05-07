package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(a.sample)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State[RNG, Int](RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
  }
  def union[A](a: Gen[A], b: Gen[A]): Gen[A] = boolean.flatMap(x => if (x) a else b)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Prob = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double).flatMap { d =>
      if (d < g1Prob) g1._1.sample
      else g2._1.sample
    })
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = ???
  /*
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State(s => {
    val (a, s2) = sample.run(s)
    f(a).sample.run(s2)
  }))
  */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x => f(x).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))
}

/*
trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}
*/

trait SGen[+A] {

}

