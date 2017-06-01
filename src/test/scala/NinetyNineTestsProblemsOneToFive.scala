import org.scalatest.FlatSpec

class NinetyNineSpecOneToFive extends FlatSpec {

  "A list of numbers: List(1,2,3,4,5)" should "return 5" in {
    assert(P01.last(List(1,2,3,4,5)) == 5)
  }

  "A list of one number: List(1)" should "return 1" in {
    assert(P01.last(List(1)) == 1)
  }

  it should "throw a NoSuchElementException" in {
    assertThrows[NoSuchElementException]{
      P01.last(Nil)
    }
  }

  "A list of numbers: List(1,2,3,4,5)" should "return 4" in {
    assert(P02.penultimate(List(1,2,3,4,5)) == 4)
  }

  "A list of two numbers: List(1,2)" should "return 1" in {
    assert(P02.penultimate(List(1,2)) == 1)
  }

  it should "throw a NoSuchElementException when given a List(1)" in {
    assertThrows[NoSuchElementException]{
      P02.penultimate(List(1))
    }
  }

  it should "throw a NoSuchElementException when given a Nil" in {
    assertThrows[NoSuchElementException]{
      P02.penultimate(Nil)
    }
  }

  "A list of numbers: List(1,2,3,4,5)" should "return the item in index postion 2" in {
    assert(P03.nth(2,List(1,2,3,4,5)) == 3)
  }

  "A list of numbers: List(1,2,3,4,5)" should "throw an IndexOutOfBounds Exception for index position 5" in {
    assertThrows[IndexOutOfBoundsException]{
      P03.nth(5, List(1,2,3,4,5))
    }
  }

  "A list of numbers: List(1,2,3,4,5)" should "throw an IndexOutOfBounds Exception for index position -1" in {
    assertThrows[IndexOutOfBoundsException]{
      P03.nth(-1, List(1,2,3,4,5))
    }
  }
  "A list of numbers: List(1,2,3,4,5)" should "return a result of 5" in {
    assert(P04.length(List(1,2,3,4,5)) == 5)
  }

  "A list of one number: List(1)" should "return a result of 1" in {
    assert(P04.length(List(1)) == 1)
  }

  "A list of Nil" should "return a 0" in {
    assert(P04.length(Nil) == 0)
  }

  "A list of numbers: List(1,2,3,4,5)" should "return a list of numbers: List(5,4,3,2,1)" in {
    assert(P05.reverse(List(1,2,3,4,5)) == List(5,4,3,2,1))
  }

  "A list of one number: List(1)" should "return a list of 1: List(1)" in {
    assert(P05.reverse(List(1)) == List(1))
  }

  "A list of Nil" should "return a Nil" in {
    assert(P05.reverse(Nil) == Nil)
  }
}
