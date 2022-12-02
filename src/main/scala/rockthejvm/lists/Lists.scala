package rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  // Supertype S - to avoid of "Covariant type T occurs in contravariant position in type T of value elem"
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  def apply(index: Int): Nothing = throw new NoSuchElementException()

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil
}

// we can implement fields using "override val" for method parameters
case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringRec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result // empty list
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"  // single head
      else toStringRec(remaining.tail, s"$result${remaining.head}, ") // head + tail
    }
    "[" + toStringRec(this, "") + "]"
  }

  // inner circle like "for currentIndex = 0 in Range(0, index)"
  // where outer method just give the limit for the circle and should yield the result
  override def apply(index: Int): T = {
    @tailrec
    def applyRec(remaining: RList[T], currentIndex: Int): T = currentIndex match {
      case _ if index < 0 => throw new NoSuchElementException()
      case _ if currentIndex == index => remaining.head
      case _ => applyRec(remaining.tail, currentIndex + 1)
    }
    applyRec(this, 0)
  }

  override def length: Int = {
    @tailrec
    def lenghtRec(remaining: RList[T], accumulator: Int): Int = remaining match {
      case _ if remaining.isEmpty => accumulator
      case _ => lenghtRec(remaining.tail, accumulator + 1)
    }
    // Complexity is O(N)
    lenghtRec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseRec(remainig: RList[T], accumulator: RList[T]): RList[T] = remainig match {
      case _ if remainig.isEmpty => accumulator
      case _ => reverseRec(remainig.tail, remainig.head :: accumulator)
    }
    // Complexity is O(N)
    reverseRec(this, RNil)
  }
}
object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromRec(remaining: Iterable[T], accumulator: RList[T]): RList[T] = remaining match {
      case _ if remaining.isEmpty => accumulator
      case _ => fromRec(remaining.tail, remaining.head :: accumulator)
    }
    // Complexity potentially O(2N) = O(N)
    fromRec(iterable, RNil).reverse
  }
}


object Lists extends App {
  val aSmallList: RList[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: RNil
  val aLargeList: RList[Int] = RList.from(1 to 10000)

  println(aSmallList.apply(2))  // 3
  println(aSmallList.length)  // 10
  println(aSmallList.reverse)  // [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  println()  // [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  println(aLargeList.apply(8735)) // 8736
  println(aLargeList.length) // 10000
  println(aLargeList.reverse) // [10000, 9999, 9998, 9997, 9996, 9995, 9994, 9993, 9992, 9991, 9990, 9989, 9988, 9987, 9986, 9985, ...]
}
