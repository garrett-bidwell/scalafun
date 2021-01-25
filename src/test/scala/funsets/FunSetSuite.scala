package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersection contains only the elements in both sets`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val u = union(union(s4, s5), s3)
      assert(contains(intersect(s, u), 3), "Intersect test")
      assert(!contains(intersect(s, u), 2), "Intersect negative test")
    }
  }

  @Test def `diff works yo`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val u = union(union(s4, s5), s3)
      assert(contains(diff(s, u), 1), "Diff test")
      assert(contains(filter(s,x=>x%2==0),2),"Filter test")
      assert(forall(s,x=>x>=0)==true,"ForAll test")
      assert(exists(s,x=>x%2==0)==true,"Exist")
      val m = map(s, x => x*x)
      printSet(m)
    }
  }

  @Test def `filter works yo`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val u = union(union(s4, s5), s3)
      assert(contains(filter(s, x => x%2 == 0), 2), "Filter test")
      assert(contains(filter(u, x => x%2 == 1), 5), "Filter test 2")
      assert(!contains(filter(s, x => x%2 != 0), 2), "Filter negative test")
    }
  }

  @Test def `forall works yo`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val u = union(union(s4, s5), s3)
      assert(forall(s, x => x >= 0) == true, "forall test")
      assert(forall(u, x => x >= 0) == true, "forall test 2")
      assert(forall(s, x => x <= 0) == false, "forall negative test")
    }
  }

  @Test def `exists works yo`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val u = union(union(s4, s5), s3)
      assert(exists(s, x => x%2 == 0) == true, "exists test")
      assert(exists(u, x => x%2 == 1) == true, "exists test 2")
      assert(exists(s, x => x <= 0) == false, "exists negative test")
      val m = map(s, x => x*x)
      printSet(m)
    }
  }

  @Test def `map works yo`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val m = map(s, x => x*x)
      printSet(m)
      assert(contains(m, 9), "map test with contains")
      assert(exists(m, x => x%2 == 0) == true, "map test with exists")
      assert(!contains(m, 3), "map negative test")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
