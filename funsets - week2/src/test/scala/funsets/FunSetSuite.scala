package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  def threeElemSet (a: Int, b: Int, c: Int): Set = {
    x: Int => x == a || x == b || x == c
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union of 3 elem sets") {
    val s1 = threeElemSet(1, 2, 3)
    val s2 = threeElemSet(3, 4, 5)
    val un = union(s1, s2)
    assert(contains(un, 2), "element from first set")
    assert(contains(un, 3), "element from both sets")
    assert(contains(un, 5), "element from 2nd set")
  }

  test("intersection contains only the common ones") {
    new TestSets {
      val s = union(s1, s2)
      val int1 = intersect(s1, s2)
      val int2 = intersect(s, s1)
      assert(contains(int1, 1) === false, "intersection of distinct sets should be false")
      assert(contains(int1, 2) === false, "intersection of distinct sets should be false")
      assert(contains(int2, 1) === true, "intersection of common elements should be true")
    }
  }

  test("intersection of 3 elem sets") {
    val s1 = threeElemSet(1, 2, 3)
    val s2 = threeElemSet(3, 4, 5)
    val intersection = intersect(s1, s2)
    assert(contains(intersection, 2) === false, "element from first set")
    assert(contains(intersection, 3), "element from both sets")
    assert(contains(intersection, 5) === false, "element from 2nd set")
  }

  test("diff contains elements only from 1 set") {
    val s1 = threeElemSet(1, 2, 3)
    val s2 = threeElemSet(3, 4, 5)
    val d = diff(s1, s2)
    assert(contains(d, 2), "element from first set")
    assert(contains(d, 3) === false, "element from both sets")
    assert(contains(d, 5) === false, "element from 2nd set")
  }

  test("filter function") {
    val s1 = positive(5)
    val pred = multipleOfN(2)
    val filtered = filter(s1, pred)
    assert(contains(filtered, 1) === false)
    assert(contains(filtered, 2) === true)
    assert(contains(filtered, 3) === false)
    assert(contains(filtered, 4) === true)
    assert(contains(filtered, 5) === false)
  }

  test("forall") {
    val evens = multipleOfN(2)
    val threes = multipleOfN(3)
    val sixes = intersect(evens, threes)
    assert(forall(sixes, x => x % 6 == 0), "set of evens and multiples of 3 are multiples of 6")
  }

  test("exists") {
    val sixes = multipleOfN(6)
    assert(exists(sixes, multipleOfN(2)), "there exists a multiple of 6 which is also a multiple of 2")
    assert(exists(sixes, multipleOfN(3)), "there exists a multiple of 6 which is also a multiple of 3")
  }

  test("map function") {
    val threes = multipleOfN(3)
    val plusOnes = map(threes, x => x + 1)
    printSet(plusOnes)
  }
}
