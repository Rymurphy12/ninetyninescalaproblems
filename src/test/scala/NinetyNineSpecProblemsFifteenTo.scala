import org.scalatest.FlatSpec

class NinetyNineSpecFifteenTo extends FlatSpec {

  "A list of symbols: List('a, 'b, 'c, 'c, 'd) duplicated 3 times" should "return List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)" in {
    assert(P15.duplicateN(3,  List('a, 'b, 'c, 'c, 'd)) ==  List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))

  }

  "A list of symbols: List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) where every third element is dropped " should "return List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)" in {
    assert(P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "A list of symbols: List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) that splits the list after the third element " should "return (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))" in {
    assert(P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}



