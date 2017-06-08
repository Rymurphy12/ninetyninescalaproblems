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

  "Packing lists of Symbols: List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)"  should "List(List('a,'a,'a,'a),List('b), List('c,'c), List('d), List('a, 'a), List('e, 'e, 'e, 'e)) " in {
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

}
