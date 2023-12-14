/** author: Andreas Röhler */

/** 
  Exercise 2.1.7.2
Find all triples i, j, k where 0 ≤ i,j,k ≤ 9 and i + 4 ∗ j + 9 * k > i ∗ j * k.
Hint: use flatMap and filter.
  */

val result = (0 to 9).flatMap(x => (0 to 9).flatMap(y => (0 to 9).map { z => (x, y, z) } )).filter{ case (i, j, k) => (i +  4 * k + 9 * j) > (i * j * k) }

val expected = Vector((0,0,1), (0,0,2), (0,0,3), (0,0,4), (0,0,5), (0,0,6), (0,0,7), (0,0,8), (0,0,9), (0,1,0), (0,1,1), (0,1,2), (0,1,3), (0,1,4), (0,1,5), (0,1,6), (0,1,7), (0,1,8), (0,1,9), (0,2,0), (0,2,1), (0,2,2), (0,2,3), (0,2,4), (0,2,5), (0,2,6), (0,2,7), (0,2,8), (0,2,9), (0,3,0), (0,3,1), (0,3,2), (0,3,3), (0,3,4), (0,3,5), (0,3,6), (0,3,7), (0,3,8), (0,3,9), (0,4,0), (0,4,1), (0,4,2), (0,4,3), (0,4,4), (0,4,5), (0,4,6), (0,4,7), (0,4,8), (0,4,9), (0,5,0), (0,5,1), (0,5,2), (0,5,3), (0,5,4), (0,5,5), (0,5,6), (0,5,7), (0,5,8), (0,5,9), (0,6,0), (0,6,1), (0,6,2), (0,6,3), (0,6,4), (0,6,5), (0,6,6), (0,6,7), (0,6,8), (0,6,9), (0,7,0), (0,7,1), (0,7,2), (0,7,3), (0,7,4), (0,7,5), (0,7,6), (0,7,7), (0,7,8), (0,7,9), (0,8,0), (0,8,1), (0,8,2), (0,8,3), (0,8,4), (0,8,5), (0,8,6), (0,8,7), (0,8,8), (0,8,9), (0,9,0), (0,9,1), (0,9,2), (0,9,3), (0,9,4), (0,9,5), (0,9,6), (0,9,7), (0,9,8), (0,9,9), (1,0,0), (1,0,1), (1,0,2), (1,0,3), (1,0,4), (1,0,5), (1,0,6), (1,0,7), (1,0,8), (1,0,9), (1,1,0), (1,1,1), (1,1,2), (1,1,3), (1,1,4), (1,1,5), (1,1,6), (1,1,7), (1,1,8), (1,1,9), (1,2,0), (1,2,1), (1,2,2), (1,2,3), (1,2,4), (1,2,5), (1,2,6), (1,2,7), (1,2,8), (1,2,9), (1,3,0), (1,3,1), (1,3,2), (1,3,3), (1,3,4), (1,3,5), (1,3,6), (1,3,7), (1,3,8), (1,3,9), (1,4,0), (1,4,1), (1,4,2), (1,4,3), (1,4,4), (1,4,5), (1,4,6), (1,4,7), (1,4,8), (1,4,9), (1,5,0), (1,5,1), (1,5,2), (1,5,3), (1,5,4), (1,5,5), (1,5,6), (1,5,7), (1,5,8), (1,5,9), (1,6,0), (1,6,1), (1,6,2), (1,6,3), (1,6,4), (1,6,5), (1,6,6), (1,6,7), (1,6,8), (1,6,9), (1,7,0), (1,7,1), (1,7,2), (1,7,3), (1,7,4), (1,7,5), (1,7,6), (1,7,7), (1,7,8), (1,7,9), (1,8,0), (1,8,1), (1,8,2), (1,8,3), (1,8,4), (1,8,5), (1,8,6), (1,8,7), (1,8,8), (1,8,9), (1,9,0), (1,9,1), (1,9,2), (1,9,3), (1,9,4), (1,9,5), (1,9,6), (1,9,7), (1,9,8), (1,9,9), (2,0,0), (2,0,1), (2,0,2), (2,0,3), (2,0,4), (2,0,5), (2,0,6), (2,0,7), (2,0,8), (2,0,9), (2,1,0), (2,1,1), (2,1,2), (2,1,3), (2,1,4), (2,1,5), (2,1,6), (2,1,7), (2,1,8), (2,1,9), (2,2,0), (2,2,1), (2,2,2), (2,2,3), (2,2,4), (2,2,5), (2,2,6), (2,2,7), (2,2,8), (2,2,9), (2,3,0), (2,3,1), (2,3,2), (2,3,3), (2,3,4), (2,3,5), (2,3,6), (2,3,7), (2,3,8), (2,3,9), (2,4,0), (2,4,1), (2,4,2), (2,4,3), (2,4,4), (2,4,5), (2,4,6), (2,4,7), (2,4,8), (2,4,9), (2,5,0), (2,5,1), (2,5,2), (2,5,3), (2,5,4), (2,5,5), (2,5,6), (2,5,7), (2,6,0), (2,6,1), (2,6,2), (2,6,3), (2,6,4), (2,6,5), (2,6,6), (2,7,0), (2,7,1), (2,7,2), (2,7,3), (2,7,4), (2,7,5), (2,7,6), (2,8,0), (2,8,1), (2,8,2), (2,8,3), (2,8,4), (2,8,5), (2,8,6), (2,9,0), (2,9,1), (2,9,2), (2,9,3), (2,9,4), (2,9,5), (3,0,0), (3,0,1), (3,0,2), (3,0,3), (3,0,4), (3,0,5), (3,0,6), (3,0,7), (3,0,8), (3,0,9), (3,1,0), (3,1,1), (3,1,2), (3,1,3), (3,1,4), (3,1,5), (3,1,6), (3,1,7), (3,1,8), (3,1,9), (3,2,0), (3,2,1), (3,2,2), (3,2,3), (3,2,4), (3,2,5), (3,2,6), (3,2,7), (3,2,8), (3,2,9), (3,3,0), (3,3,1), (3,3,2), (3,3,3), (3,3,4), (3,3,5), (3,4,0), (3,4,1), (3,4,2), (3,4,3), (3,4,4), (3,5,0), (3,5,1), (3,5,2), (3,5,3), (3,5,4), (3,6,0), (3,6,1), (3,6,2), (3,6,3), (3,6,4), (3,7,0), (3,7,1), (3,7,2), (3,7,3), (3,8,0), (3,8,1), (3,8,2), (3,8,3), (3,9,0), (3,9,1), (3,9,2), (3,9,3), (4,0,0), (4,0,1), (4,0,2), (4,0,3), (4,0,4), (4,0,5), (4,0,6), (4,0,7), (4,0,8), (4,0,9), (4,1,0), (4,1,1), (4,1,2), (4,1,3), (4,1,4), (4,1,5), (4,1,6), (4,1,7), (4,1,8), (4,1,9), (4,2,0), (4,2,1), (4,2,2), (4,2,3), (4,2,4), (4,2,5), (4,3,0), (4,3,1), (4,3,2), (4,3,3), (4,4,0), (4,4,1), (4,4,2), (4,4,3), (4,5,0), (4,5,1), (4,5,2), (4,5,3), (4,6,0), (4,6,1), (4,6,2), (4,7,0), (4,7,1), (4,7,2), (4,8,0), (4,8,1), (4,8,2), (4,9,0), (4,9,1), (4,9,2), (5,0,0), (5,0,1), (5,0,2), (5,0,3), (5,0,4), (5,0,5), (5,0,6), (5,0,7), (5,0,8), (5,0,9), (5,1,0), (5,1,1), (5,1,2), (5,1,3), (5,1,4), (5,1,5), (5,1,6), (5,1,7), (5,1,8), (5,1,9), (5,2,0), (5,2,1), (5,2,2), (5,2,3), (5,3,0), (5,3,1), (5,3,2), (5,4,0), (5,4,1), (5,4,2), (5,5,0), (5,5,1), (5,5,2), (5,6,0), (5,6,1), (5,6,2), (5,7,0), (5,7,1), (5,7,2), (5,8,0), (5,8,1), (5,8,2), (5,9,0), (5,9,1), (5,9,2), (6,0,0), (6,0,1), (6,0,2), (6,0,3), (6,0,4), (6,0,5), (6,0,6), (6,0,7), (6,0,8), (6,0,9), (6,1,0), (6,1,1), (6,1,2), (6,1,3), (6,1,4), (6,1,5), (6,1,6), (6,1,7), (6,2,0), (6,2,1), (6,2,2), (6,3,0), (6,3,1), (6,3,2), (6,4,0), (6,4,1), (6,4,2), (6,5,0), (6,5,1), (6,6,0), (6,6,1), (6,7,0), (6,7,1), (6,8,0), (6,8,1), (6,9,0), (6,9,1), (7,0,0), (7,0,1), (7,0,2), (7,0,3), (7,0,4), (7,0,5), (7,0,6), (7,0,7), (7,0,8), (7,0,9), (7,1,0), (7,1,1), (7,1,2), (7,1,3), (7,1,4), (7,1,5), (7,2,0), (7,2,1), (7,2,2), (7,3,0), (7,3,1), (7,4,0), (7,4,1), (7,5,0), (7,5,1), (7,6,0), (7,6,1), (7,7,0), (7,7,1), (7,8,0), (7,8,1), (7,9,0), (7,9,1), (8,0,0), (8,0,1), (8,0,2), (8,0,3), (8,0,4), (8,0,5), (8,0,6), (8,0,7), (8,0,8), (8,0,9), (8,1,0), (8,1,1), (8,1,2), (8,1,3), (8,1,4), (8,2,0), (8,2,1), (8,2,2), (8,3,0), (8,3,1), (8,4,0), (8,4,1), (8,5,0), (8,5,1), (8,6,0), (8,6,1), (8,7,0), (8,7,1), (8,8,0), (8,8,1), (8,9,0), (8,9,1), (9,0,0), (9,0,1), (9,0,2), (9,0,3), (9,0,4), (9,0,5), (9,0,6), (9,0,7), (9,0,8), (9,0,9), (9,1,0), (9,1,1), (9,1,2), (9,1,3), (9,2,0), (9,2,1), (9,3,0), (9,3,1), (9,4,0), (9,4,1), (9,5,0), (9,5,1), (9,6,0), (9,6,1), (9,7,0), (9,7,1), (9,8,0), (9,8,1), (9,9,0), (9,9,1)).toIndexedSeq

// That function is unused for now
case class Solution2_1_7_2_AR_check(i: Int, k: Int, j: Int) {
  val ergebnis: Boolean = (i +  4 * k + 9 * j) > (i * j * k)
  printf("\nergebnis: %s ", ergebnis)
}

// Check the value of expected
val check_solution2_1_7_2_AR = expected.map({case (i, j, k) => ((i +  4 * k + 9 * j), (i * j * k)) }).filter{ case (x, y) => x <  y }

printf("\ncheck_solution2_1_7_2_AR: this should be empty: %s", check_solution2_1_7_2_AR)

assert(result == expected) 
