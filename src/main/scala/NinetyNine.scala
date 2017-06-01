object NinetyNine {

  //Problem 1
  def last[A](xs: List[A]):A = xs match {
    case Nil => sys.error("Empty list inputed")
    case h :: Nil => h
    case h :: t =>  last(t)
  }

  //Problem 2
  def penultimate[A](xs: List[A]):A = xs match {
    case Nil => sys.error("Empty list inputed")
    case h :: t :: Nil => h
    case h :: t => penultimate(t)
  }

}
