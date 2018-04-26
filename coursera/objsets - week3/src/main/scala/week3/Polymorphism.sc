package week3

import objsets.{Empty, NonEmpty, TweetSet}

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
/**
  *  making the param T fails because we can pass
  * 2 unrelated subtypes to the function, resulting
  * in error
  *  so make it accept a super type of covariant type T
  *  This passes variance checks because
  *  - Covariant type params may appear in lower bounds
  *  of method type params
  *  - Contravariant type params may appear in upper bounds
  *  of method type params
  */
  def prepend[U >: T](elem: U) = new Cons[U](elem, this)
}

// putting val inside the constructor means that the
// class has a definition of head which is initialized with
// the constructor itself
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

//def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
//
//def nth[T](n: Int, xs: List[T]): T = {
//  xs match {
//    case xs: Nil[T] => throw new IndexOutOfBoundsException
//    case xs: List[T] if (n == 0) => xs.head
//    case _ => nth(n - 1, xs.tail)
//  }
//}
//
//val l = new Cons[Int](1, new Cons(2, new Cons(3, new Nil[Int])))
//nth(1, l)


object Polymorphism {
  /**
    * after making List take a covariant arg type,
    * everything type checks perfectly because:
    * Nothing is a subtype of String
    * List[Nothing] (Nil) is a subtype of List[String]
    * /therefore, this assignment is possible
    */

  def fn(xs: List[NonEmpty], x: Empty): List[TweetSet] = xs prepend x
  val x: List[String] = Nil
}