val range = 'A' to 'M'
def mm(c: Char, i: Int) = " " * i + c
range.zipWithIndex.head._1

import org.scalacheck.Prop.{BooleanOperators, forAll}

val p1 = forAll { n: Int =>
  (n % 2 == 0) ==> (2 * (n / 2) == n)
  (n % 2 == 1) ==> ((n + 1) % 2 == 0)
}

val p2 = forAll {(s: String) => s.toLowerCase.toUpperCase == s.toUpperCase}

val p3 = p1 && p2
//p3.check

val vv = List(1)
vv.reduce((e1, e2) => e1 + e2)






List("aa", "b").reduce((a, b) => a + b)

List('A', '-').mkString("")

