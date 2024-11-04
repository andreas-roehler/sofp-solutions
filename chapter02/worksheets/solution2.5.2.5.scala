/** 
  Exercise 2.5.2.5
  Same task as in Exercise 2.5.2.4 but using a set of sets.

  Instead of just three sets a, b, c, we are given a value of type
  Set[Set[Int]].

  The required type signature and a sample test:

  def prodSet(si: Set[Set[Int]]): Set[Set[Int]] = ???

  scala> prodSet(Set(Set(1, 2), Set(3), Set(4, 5), Set(6)))
  res0: Set[Set[Int]] = Set(Set(1,3,4,6),Set(1,3,5,6),Set(2,3,4,6),Set(2,3,5,6))

  Hint: use foldLeft and flatMap.
  */

def prodSetIntern(b: Set[Set[Int]] = Set(Set.empty), symbols: List[Int] = List.empty): Set[Set[Int]] = {
  val c =  b.toList
  if (c.tail.isEmpty) (c.head).map{ x => (x +: symbols).toSet }
  else
    c.head.flatMap{ x => prodSetIntern(c.tail.toSet, (x +: symbols)) }
}


def prodSet(a: Set[Set[Int]] = Set(Set.empty)): Set[Set[Int]] = {
  prodSetIntern(a)
}

val result = prodSet(Set(Set(1, 2), Set(3), Set(4, 5), Set(6)))
val expected: Set[Set[Int]] = Set(Set(1,3,4,6),Set(1,3,5,6),Set(2,3,4,6),Set(2,3,5,6))
assert(result == expected)

// scala> :load solution2.5.2.5.scala
// :load solution2.5.2.5.scala
// def prodSetIntern(b: Set[Set[Int]], symbols: List[Int]): Set[Set[Int]]
// def prodSet(a: Set[Set[Int]]): Set[Set[Int]]
// val result: Set[Set[Int]] = Set(Set(6, 4, 3, 1), Set(6, 5, 3, 1), Set(6, 4, 3, 2), Set(6, 5, 3, 2))
// val expected: Set[Set[Int]] = Set(Set(1, 3, 4, 6), Set(1, 3, 5, 6), Set(2, 3, 4, 6), Set(2, 3, 5, 6))
