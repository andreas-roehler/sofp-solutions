/** 
  Exercise 2.5.2.15
  For a given sequence xs:Seq[Double], find a subsequence that
  has the largest sum of values.

  The sequence xs is not sorted, and its values may be positive or
  negative.

  The required type signature and a sample test:

  def maxsub(xs: Seq[Double]): Seq[Double] = ???

  scala> maxsub(Seq(1.0, -1.5, 2.0, 3.0, -0.5, 2.0, 1.0, -10.0, 2.0))

  res0: Seq[Double] = List(2.0, 3.0, -0.5, 2.0, 1.0)

  Hint: use dynamic programming and foldLeft.
 */
            
def maxsub(xs: Seq[Double], res: Seq[Double] = Seq.empty, max: Double = Double.NegativeInfinity): Seq[Double] = { 
  if (xs.isEmpty) res.reverse
  else {
    var resIntern: Seq[Double] = Seq.empty
    var newRes: Seq[Double] = Seq.empty
    var maxIntern = max
    var newIntern: Seq[Double] = Seq.empty 
    for (i <- xs) {
      newIntern = i +: resIntern
      resIntern = newIntern
      if (maxIntern < newIntern.sum) {newRes = newIntern
                                      maxIntern = newRes.sum}
    }
    if (maxIntern <= max) maxsub(xs.tail, res, max)
    else maxsub(xs.tail, newRes, maxIntern)
  }
}

val result =  maxsub(a)
val resultSum = maxsub(a).sum
val expected =  List(2.0, 3.0, -0.5, 2.0, 1.0)
val expectedSum =  List(2.0, 3.0, -0.5, 2.0, 1.0).sum
assert(result == expected) 
