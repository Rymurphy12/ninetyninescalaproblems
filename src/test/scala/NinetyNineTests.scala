import org.scalatest.FlatSpec

class NinetyNineSpec extends FlatSpec {

  "A List of one through five" should "return 5" in {
    assert(NinetyNine.last(List(1,2,3,4,5)) == 5)
  }

  
}
