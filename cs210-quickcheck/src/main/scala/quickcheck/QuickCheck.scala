package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for 
      j <- arbitrary[Int]
      i <- insert(j, empty)
      h <- oneOf(const(empty), genHeap)
    yield
      meld(h,i)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = if (a > b) then b else a 
    findMin(h) == min
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("del1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sort1") = forAll { (h: H) => 
    def sortAux(h: H): List[Int] = 
      if(isEmpty(h)) then Nil else findMin(h) :: sortAux(deleteMin(h))
    
    def checkOrder(l: List[Int]): Boolean = l match
      case Nil => true
      case x :: Nil => true
      case x :: xs => x <= xs.head && checkOrder(xs)
    checkOrder(sortAux(h)) == true
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min3") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    if(isEmpty(h1) && isEmpty(h2)) then isEmpty(h) 
    else if (isEmpty(h1)) then findMin(h2) == findMin(h)
    else if (isEmpty(h2)) then findMin(h1) == findMin(h)
    else findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
  }

  //deleting min works correctly
  property("meld1") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    def aux(h: H, h1: H, h2: H): Boolean =
      if(isEmpty(h)) then isEmpty(h1) && isEmpty(h2)
      else if (isEmpty(h1)) then aux(deleteMin(h),h1, deleteMin(h2))
      else if (isEmpty(h2)) then aux(deleteMin(h),h2, deleteMin(h1))

      else 
        val min = findMin(h) 
        if(min == findMin(h1)) then aux(deleteMin(h),h2, deleteMin(h1))
        else if(min == findMin(h2)) then aux(deleteMin(h), deleteMin(h2), h1)
        else false
    aux(h,h1,h2) == true
  }

  //merging works correctly
  property("meld2") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    def aux(h: H, h1: H, h2: H): Boolean =
      if(isEmpty(h)) then isEmpty(h1) && isEmpty(h2)
      else if (isEmpty(h1)) then aux(deleteMin(h),h1, deleteMin(h2))
      else if (isEmpty(h2)) then aux(deleteMin(h),h2, deleteMin(h1))

      else 
        val min = findMin(h) 
        if(min == findMin(h1)) then aux(deleteMin(h),h2, deleteMin(h1))
        else aux(deleteMin(h), deleteMin(h2), h1)
    aux(h,h1,h2) == true
  }

  //merging double check
  property("meld3") = forAll { (l: List[Int]) =>
      def add(l: List[Int], acc: H): H = l match
        case Nil => acc
        case x :: xs => add(xs, insert(x, acc))

      def sortAux(h: H): List[Int] = 
      if(isEmpty(h)) then Nil else findMin(h) :: sortAux(deleteMin(h))
    
      sortAux(add(l, empty)).toSet.equals(l.toSet) 
    }