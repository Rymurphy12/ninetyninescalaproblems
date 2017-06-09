object NinetyNineProblems {
  def main(args: Array[String]): Unit = {
    println(P09.pack(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)))
  }
}

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
      case (h: List[_], Nil) => flatten(h) ::: Nil
      case (h, Nil) => List(h)
      case (h: List[_], t) => flatten(h) ::: flatten(t)
      case (h, t) =>  h :: flatten(t)
    }
  }
}

object P08 {
  //Problem 8
  def compress[A](xs: List[A]): List[A] = {
    def go(acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => acc.reverse
      case h :: Nil => (h :: acc).reverse
      case h :: t => if (h == t.head) go(h :: acc, t dropWhile(_ == h)) else go( h :: acc, t)
   }
    go(Nil, xs)
  }
}

object P09 {
  //Problem 9
  def pack[A](xs: List[A]): List[List[A]] = {
    def go(acc: List[List[A]], xs: List[A]): List[List[A]] = xs match {
      case Nil => acc.reverse
      case h :: t => {
        val (newList, oldList) = buildSublist(Nil, xs)
        go(newList :: acc, oldList)
      }
    }
    go(Nil, xs)
  }
  def  buildSublist[A](acc: List[A], xs: List[A]): (List[A], List[A]) = xs match {
    case Nil => (acc.reverse, xs)
    case h :: t => if (t != Nil && h == t.head) buildSublist(h :: acc, t) else ((h :: acc).reverse, t)
  }
}

object P10 {
  //Problem 10
  def encode[A](xs: List[A]): List[(Int, A)] = {
    if (xs == Nil) throw new NoSuchElementException
    val packedList = P09.pack(xs)
    packedList map (x => (x.length, x.head))
  }
}

object P11 {
  //Problem 11
  def encodeModified[A](xs: List[A]): List[Any] = {
    if (xs == Nil) throw new NoSuchElementException
    val packedList = P09.pack(xs)
    packedList map {x => if(x.length == 1) x.head else (x.length, x.head)}

  }
}

object P12 {
  //Problem 12
  def decode[A](xs: List[(Int, A)]): List[A] = {
    if(xs == Nil) throw new NoSuchElementException
    def go(acc: List[A], encodedList : List[(Int, A)]): List[A] = encodedList match {
      case Nil => acc.reverse
      case h :: t => go(decodeTuple(Nil, h) ::: acc, t)
    }

    def decodeTuple(acc: List[A], tuple: (Int, A)): List[A] = tuple match {
      case (0, _) => acc
      case (n, a) => decodeTuple(a :: acc, (n-1, a))
    }
    go(Nil, xs)
  }
}

object P13 {
  //Problem 13
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = {
    if (xs == Nil) throw new NoSuchElementException
    def go(acc: List[(Int, A)], unencodedList: List[A]): List[(Int, A)] = unencodedList match {
      case Nil => acc.reverse
      case _ => { val (subList, remainingList) = P09.buildSublist(Nil, unencodedList)
                  go((subList.length, subList.head) :: acc, remainingList)
      }
    }
    go(Nil, xs)
  }
}

object P14 {
  //Problem 14
  def duplicate[A](xs: List[A]): List[A] = {
    xs flatMap (x => List(x, x))
  }
}
