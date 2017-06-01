object P01{
  //Problem 1
  def last[A](xs: List[A]):A = xs match {
    case h :: Nil => h
    case h :: t =>  last(t)
    case Nil => throw new NoSuchElementException
  }
}

object P02 {
  //Problem 2
  def penultimate[A](xs: List[A]):A = xs match {
    case h :: t :: Nil => h
    case h :: t => penultimate(t)
    case _ => throw new NoSuchElementException
  }
}

object P03 {
  //Problem 3
  def nth[A](x: Int, xs: List[A]):A = (x, xs) match {
      case (0, h :: _) => h
      case (n, h :: t) => nth(n -1, t)
      case (_, Nil) => throw new IndexOutOfBoundsException
    }
}

object P04 {
  //Problem 4
  def length[A](xs: List[A]): Int = {
    def go(acc: Int, xs: List[A]): Int = xs match {
      case Nil => acc
      case h :: t  => go(acc + 1, t)
    }
    go(0, xs)
  }
}

object P05 {
  //Problem 5
  def reverse[A](xs: List[A]): List[A] = {
    def go(acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => acc
      case h :: t => go(h :: acc, t)
    }
    go(Nil, xs)
  }
}
