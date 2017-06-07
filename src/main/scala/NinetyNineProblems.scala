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

object P06 {
  //Problem 6
  def isPalindrome[A](xs: List[A]): Boolean = xs match {
    case Nil => true
    case h :: Nil => true
    case h :: t => if (h == t.last) isPalindrome(t.init) else false
  }
}

object P07 {
  //Problem 7
  def flatten(xs: List[Any]): List[Any] = {
    if (xs == Nil) Nil
    else (xs.head, xs.tail) match {
    case (h: List[_], t) => if (t == Nil) flatten(h) ::: Nil else flatten(h) ::: flatten(t)
    case (h, t) => if (t == Nil) List(h) else h :: flatten(t)
    }
  }
}
