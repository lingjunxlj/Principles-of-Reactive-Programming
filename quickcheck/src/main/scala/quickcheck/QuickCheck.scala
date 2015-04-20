package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import Gen._

abstract class QuickCheckHeap extends Properties("Heap") with HeapListHelper {

  property("min") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min1") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    var h = insert(a, empty)
    h = insert(b, h);

    findMin(h) == min
  }

  property("del") = forAll { a: Int =>
    var h = insert(a, empty)
    h = deleteMin(h)
    isEmpty(h) == true
  }


  property("gen") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("gen1") = forAll { (h: H) =>
    var flag = true
    var h1 = h
    var i = 0
    val list = toList(h1)
    while (flag && !isEmpty(h1)) {
      val minh = findMin(h1)
      val minl = list(i)

      if (minh != minl) {
        flag = false
        println("minh is " + minh + "; minl is " + minl)
      }
      i = i + 1

      h1 = deleteMin(h1)

    }

    if(i != list.size)
      flag = false

    flag
  }

  property("gen2") = forAll { (h1: H, h2: H) =>
    val min1 = toList(h1).head
    val min2 = toList(h2).head
    val min = if (min1 < min2) min1 else min2
    val h = meld(h1, h2)
    findMin(h) == min && (findMin(h) == min1 || findMin(h) == min2)

  }

  property("gen3") = forAll { (h1: H, h2: H) =>
    var flag = true
    val h = meld(h1, h2)

    var hh1 = h1
    var hh2 = h2

    val min1 = findMin(hh1)
    hh1 = deleteMin(hh1)
    hh2 = insert(min1, hh2)

    val hh = meld(hh1, hh2)

    if (findMin(h) != findMin(hh)) {
      flag = false
    } else {
      val l1 = toList(h1)
      val l2 = toList(h2)
      var l3 = l1 ++ l2
      l3 = l3.sortWith(_.compareTo(_) < 0)

      val l4 = toList(h)

      if (l3.size <= 0 || l3.size != l4.size) {
        flag = false
      } else {
        for (i <- 0 to (l3.size - 1)) {
          if (!l3(i).equals(l4(i))) {
            flag = false
          }
        }
      }
    }
    flag
  }

  property("gen4") = forAll { (list: List[A]) =>
    val h = fromList(list)
    val listh = toList(h)
    if (list.size < 0 || list.size != listh.size) {
      false
    } else {
      for (i <- 0 to (list.size - 1)) {
        if (!list(i).equals(listh(i))) {
          false
        }
      }
    }
    true
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    var hh1 = h1
    var hh2 = h2

    val min1 = findMin(hh1)
    hh1 = deleteMin(hh1)
    hh2 = insert(min1, hh2)

    val hh = meld(hh1, hh2)

    if (findMin(h) != findMin(hh))
      false
    else
      true
  }

  lazy val genHeap: Gen[H] = for {
    i <- choose(-10000, 10000)
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  lazy val genList: Gen[List[Int]] = for {
    k <- choose(-1000, 1000)
    v <- choose(-1000, 1000)
    l <- oneOf(const(List.empty), genList)
  } yield l.updated(k, v)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // print the content of a heap
  def printHeap(h: H) {
    if (!isEmpty(h)) {
      println(findMin(h) + " ")
      printHeap(deleteMin(h))
    }
  }

  //
  //  // dummy logging property
  //  property("log") = forAll { (h: H) =>
  //    println("print heap: ")
  //    printHeap(h)
  //    true
  //  }

}

trait HeapListHelper extends IntHeap {
  def toList(h: H): List[A] = h match {
    case h if isEmpty(h) => Nil
    case h => findMin(h) :: toList(deleteMin(h))
  }

  def fromList(l: List[A]) = (l foldRight empty)(insert(_, _))
}
