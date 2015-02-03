package com.stanford.algo.week1

object InversionsCounter {

  def countInversions(inversionCount: Int, xs: List[Int]): (Int, List[Int]) = {
    val n = xs.length / 2
    if (n == 0) (0,xs)
    else {
      def mergeAndCount(xs: List[Int], ys: List[Int]): (Int, List[Int]) =
        (xs, ys) match {
          case(Nil, ys) => (inversionCount, ys)
          case(xs, Nil) => (inversionCount, xs)
          case(x :: xs1, y :: ys1) =>
            if (x < y) x::merge(xs1, ys)
            else {
              inversionCount = inversionCount +1
              y :: merge(xs, ys1)
            }
        }
      val (left, right) = xs splitAt(n)
      mergeAndCount(countInversions(left), countInversions(right))
    }
  }

}
