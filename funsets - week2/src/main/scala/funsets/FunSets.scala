package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  /*
    of all the language that they could have used, they chose to make
    it as ambiguous as possible.
    Set is just a placeholder for the function type Int => Boolean
    It has nothing to do with the Set Collection Class
    So, anywhere that requires a Set return type, we need to return a
    function that takes Int and gives a Boolean.

    These sets are not characterized by their elements, but by their
    function
    So, in colloquial terms, Set(1,2,3) is defined as
    (x => x == 1 || x == 2 || x == 3)

    Now this makes a lot of sense
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    def single(oneElem: Int) : Boolean = oneElem == elem
    single
    // typechecks correctly, because single : Int => Boolean
    // which means single : Set
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    x: Int => s(x) || t(x)
  }
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    x: Int => s(x) && t(x)
  }
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    x: Int => s(x) && !t(x)
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    x => s(x) && p(x)
  }


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 10

  def positive (bound: Int): Set = {
    x: Int => x > 0 && x <= bound
  }
  def even: Set = (x => x % 2 == 0)
  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      // if integer is in s but doesn't satisfy p, then return false
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = ???

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = ???

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
