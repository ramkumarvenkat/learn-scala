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

  test("singletonSet(1) does not contain 2") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(!contains(s1, 2), "Not a Singleton")
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

  test("union does not contain elements not present in any of the sets") {
    new TestSets {
      val s = union(s1, s2)
      assert(!contains(s, 4), "Union 4")
    }
  }

  test("intersection contains any elements of each set") {
    new TestSets {
      val u = union(s1, s2)
      val s = intersect(u, s1)
      assert(contains(s, 1), "Intersection 1")
    }
  }

  test("intersection does not contain elements present in only one of the sets") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersection 1")
    }
  }

  test("intersection does not contain elements not present in any of the sets") {
    new TestSets {
      val s = union(s1, s2)
      assert(!contains(s, 4), "Intersection 4")
    }
  }

  test("diff contains all elements of first set not present in second set") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val u2_3 = union(s2, s3)
      val s = diff(u1_2, u2_3)
      assert(contains(s, 1), "Diff 1")
    }
  }

  test("diff does not contain elements present in both the sets") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val u2_3 = union(s2, s3)
      val s = diff(u1_2, u2_3)
      assert(!contains(s, 2), "Diff 2")
    }
  }

  test("diff does not contain elements not present in any of the sets") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val u2_3 = union(s2, s3)
      val s = diff(u1_2, u2_3)
      assert(!contains(s, 4), "Diff 4")
    }
  }

  test("filter contains elements satisfied by predicate") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = filter(u1_2, _ %2 == 0)
      assert(contains(s, 2), "Filter 2")
    }
  }

  test("filter does not contain elements not satisfied by predicate") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = filter(u1_2, _ %2 == 0)
      assert(!contains(s, 1), "Filter 1")
    }
  }

  test("forall returns true when the set satisfies the predicate") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = union(u1_2, s3)
      assert(forall(s, _ < 10), "Forall less than 10")
    }
  }

  test("forall returns false when the set does not satisfy the predicate") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = union(u1_2, s3)
      assert(!forall(s, _ > 10), "Forall greater than 10")
    }
  }

  test("exists returns true when the set satisfies the predicate") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = union(u1_2, s3)
      assert(exists(s, _ <= 1), "Exists less than 1")
    }
  }

  test("exists returns false when the set does not satisfy the predicate") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = union(u1_2, s3)
      assert(!exists(s, _ > 10), "Exists greater than 10")
    }
  }

  test("map contains the transformed element") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = map(u1_2, x => x * 2)
      assert(contains(s, 4), "Map 4")
    }
  }

  test("map does not contain the original element") {
    new TestSets {
      val u1_2 = union(s1, s2)
      val s = map(u1_2, x => x * 2)
      assert(contains(s, 4), "Map 2")
    }
  }
}
