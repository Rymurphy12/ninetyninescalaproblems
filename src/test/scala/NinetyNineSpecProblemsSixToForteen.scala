import org.scalatest.FlatSpec


class NinetyNineSpec extends FlatSpec{
  "A list of numbers: List(1,2,3,4,5)" should "return false" in {
    assert(!P06.isPalindrome(List(1,2,3,4,5)))
  }

  "A list of numbers: List(1,2,3,4,5,6)" should "return false" in {
    assert(!P06.isPalindrome(List(1,2,3,4,5,6)))
  }

  "A list of Nil" should "return true" in {
    assert(P06.isPalindrome(Nil))
  }

  "A list of 1: List(1)" should "return true" in {
    assert(P06.isPalindrome(List(1)))
  }

  "A list of 2: List(1,2)" should "return false" in {
    assert(!P06.isPalindrome(List(1,2)))
  }

  "A list of 2: List(1,1)" should "return true" in {
    assert(P06.isPalindrome(List(1,1)))
  }

  "A list of numbers: List(1,2,3,2,1)" should "return true" in {
    assert(P06.isPalindrome(List(1,2,3,2,1)))
  }

  "A list of numbers: List(1,2,3,3,2,1)" should "return true" in {
    assert(P06.isPalindrome(List(1,2,3,3,2,1)))
  }

  "A list of Any: List(List(1,1), 2, List(3, List(5, 8)))" should "return List(1, 1, 2, 3, 5, 8)" in {
    assert(P07.flatten(List(List(1,1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }
  "Flattening a list of Nil" should "return Nil" in {
    assert(P07.flatten(Nil) == Nil)
  }

  "A list of Symbols: List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)" should "return List('a,'b,'c,'a,'d,'e)" in {
    assert(P08.compress(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)) == List('a,'b, 'c, 'a, 'd, 'e))
  }

  "Compressing a list of Nil" should "return Nil" in {
    assert(P08.compress(Nil) == Nil)
  }

  "Compressing a list of 1 number: List(1)" should "return List(1)" in {
    assert(P08.compress(List(1)) == List(1))
  }

  "Compressing a list of 2 numbers: List(1,1)" should "return List(1)" in {
    assert(P08.compress(List(1,1))== List(1))
  }

  "Compressing a list of 2 numbers: List(1,2)" should "return List(1,2)" in {
    assert(P08.compress(List(1,2)) == List(1,2))
  }

  "Packing lists of Symbols: List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)"  should "return List(List('a,'a,'a,'a),List('b), List('c,'c), List('d), List('a, 'a), List('e, 'e, 'e, 'e)) " in {
    assert(P09.pack(
      List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)) ==
      List(List('a,'a,'a,'a),List('b),List('c,'c),List('a,'a),List('d),List('e,'e,'e,'e)))
  }

  "Packing a list of Nil" should "return Nil" in {
    assert(P09.pack(Nil) == Nil)
  }

  "Packing a list of 2 numbers: List(1,1)" should "return List(List(1,1))" in {
    assert(P09.pack(List(1,1)) == List(List(1,1)))
  }

  "Packing a list of 1 number: List(1)" should "return a List(List(1))" in {
    assert(P09.pack(List(1))== List(List(1)))
  }

  "Encoding a list of Symbols: List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" should "return List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)" in {
    assert(P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "Encoding a list of Nil" should "throw a NoSuchElementException" in {
    assertThrows[NoSuchElementException] {
      P10.encode(Nil)
    }
  }

  "Using a modified encoding of a list of Symbols: List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) " should "return List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))" in {
    assert(P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  "Using a modified encoding on a list of Nil" should "throw a NoSuchElementException" in {
    assertThrows[NoSuchElementException] {
      P11.encodeModified(Nil)
    }
  }

  "Deccoding a List of encoded Symbols: List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))" should "return List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" in {
    assert(P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "Decoding a list of encoded Symbols: List((1, 'a))" should "return List('a)" in {
    assert(P12.decode(List((1, 'a))) == List('a))
  }

  "Decoding a list of encoded Symbols: List(2, 'a)" should "return List('a, 'a)" in {
    assert(P12.decode(List((2, 'a))) ==  List('a, 'a))
  }

  "Decoding a list of Nil" should "throw a NoSuchElementException" in {
    assertThrows[NoSuchElementException] {
      P12.decode(Nil)
    }
  }

  "A direct encoding of Symbols: List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) " should "return List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))" in {
    assert(P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "A direct encoding of a List of Nil" should "throw a NoSuchElementException" in {
    assertThrows[NoSuchElementException] {
      P13.encodeDirect(Nil)
    }
  }

  "Duplicating a list of Symbols: List('a, 'b, 'c, 'c, 'd)" should "return List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)" in {
    assert(P14.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
}
