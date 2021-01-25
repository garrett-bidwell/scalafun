package patmat

import org.junit._
import org.junit.Assert.{ assertEquals, assertTrue}

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val bcd = Fork(Leaf('B',3), Fork(Leaf('C',1), Leaf('D',1), List('C','D'), 2), List('B','C','D'), 5)
    val efgh = Fork(Fork(Leaf('E',1), Leaf('F',1), List('E','F'), 2), Fork(Leaf('G',1), Leaf('H',1), List('G','H'), 2), List('E','F','G','H'), 4)
    val bcdefgh = Fork(bcd, efgh, List('B','C','D','E','F','G','H'), 9)
    val t3 = Fork(Leaf('A',8), bcdefgh, List('A','B','C','D','E','F','G','H'), 17)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `times of a list`: Unit = {
    val timesResult = times(List('a', 'b', 'a', 'a', 'b'))
    assertTrue(timesResult.contains('a', 3) && timesResult.contains('b', 2))
  }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `combine a 2-leaf list`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)), combine(leaflist))
  }

  @Test def `decode french secret`: Unit = {
    println(decode(frenchCode, secret))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `convert a code tree to a code table and code the bits for a character`: Unit =
    new TestTrees {
      val codeTable = convert(t3)
      assertEquals(List(1,0,1,1), codeBits(codeTable)('D'))
      assertEquals(List(1,1,1,0), codeBits(codeTable)('G'))
    }

  @Test def `quickEncode some text`: Unit =
    new TestTrees {
      assertEquals(List(1,0,1,0,1,1,0,0), quickEncode(t3)(List('C','E')))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
