import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.Range.Inclusive

val add = (a: Int, b: Int) => a + b

for (i <- 1 to 3; j <- 1 to 3) yield add(i, j)

val palindromeGen: Gen[String] = for {
  base <- arbitrary[String]
  middle <- Gen.option(arbitrary[Char])
} yield base + middle.getOrElse("") + base.reverse


arbitrary[String].sample

import org.scalacheck.Prop.forAll
val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ::: l2).size }


class C(i: Int) {
  val oneTo: Inclusive =  1 to i
  def product() : Int = {
    oneTo.filter(i => i % 2 == 0).product
  }
}

object C {
  def apply(i: Int) = {
    println(s"Creating C($i)")
    new C(i).product()
  }
}

//C(10)


val cProd = for {
    i <- Gen.choose(10, 1000)
} yield i

cProd.sample

val range = 'A' to 'M'
val halfRows = range.length

range.zipWithIndex

range.foldLeft(0)((ac, c) => diamondRow(c, ac, halfRows)

  val leftSpaces = " " * (halfRows - ac - 1)
  val innerSpaces = " " * (ac + ac - 1)
  if (ac == 0)
    println(leftSpaces + c)
  else
  println(leftSpaces + c + innerSpaces + c)
  ac+1})


def foo(c: Char, i: Int, rows: Int) = {

}

def diamondRow(c: Char, i: Int, rows: Int) = {
  val leftSpaces = " " * (rows - i - 1)
  val innerSpaces = " " * (2 * i - 1)
  if (i == 0)
    leftSpaces + c
  else
    leftSpaces + c + innerSpaces + c
}

/*
0  1  2  3, 4, 5
0, 1, 3, 5, 7  9


 */

/*
forAll[A1, P](f: (A1) ⇒ P)(implicit p: (P) ⇒ Prop, a1: Arbitrary[A1], s1: Shrink[A1], pp1: (A1) ⇒ Pretty): Prop
Converts a function into a universally quantified property
 */