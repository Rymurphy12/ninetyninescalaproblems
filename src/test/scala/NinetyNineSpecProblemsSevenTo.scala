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
  "A list of Nil" should "return Nil" in {
    assert(P07.flatten(Nil) == Nil)
  }
}
