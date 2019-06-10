package quickcheck

import org.scalacheck.Arbitrary.{apply => _, _}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      num <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(num, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //  If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("minOfTwo") = forAll { (a: Int, b: Int) => findMin(insert(a, insert(b, empty))) == (if (a < b) a else b) }

  //  If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("deleteMin") = forAll { num: Int => isEmpty(deleteMin(insert(num, empty))) }

  //  Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sortedSeq") = forAll { h: H =>
    @tailrec
    def sub(list: List[Int], b: H): Seq[Int] =
      if (isEmpty(b)) list
      else sub(findMin(b) :: list, deleteMin(b))

    val orderedSeq = sub(Nil, h)
    orderedSeq == orderedSeq.sorted
  }

  //  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld") = forAll { (h: H, i: H) => findMin(meld(h, i)) == findMin(h) || findMin(meld(h, i)) == findMin(i) }

}