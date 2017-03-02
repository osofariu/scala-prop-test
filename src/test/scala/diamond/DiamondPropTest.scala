package diamond

import org.scalacheck.{Gen, Properties, Prop}

class DiamondPropTest extends Properties("diamond properties") {

  val validCharGen: Gen[Char] = Gen.alphaUpperChar
  property("diamond has odd number of rows") =
    Prop.forAll(Gen.alphaUpperChar) { c: Char => Diamond(c).rows.length % 2 == 1 }

  def assertSpacesAtHead(pair: (String, Int)) : Boolean = {
    val spacesAtBeginning = " " * pair._2
    pair._1.startsWith(spacesAtBeginning) && pair._1(pair._2) != ' '
  }

  property("diamond has N-1, N-2,.. 0, then 1, 2, .. N-1 spaces before first Char where N is size of Diamond") =
    Prop.forAll(Gen.alphaUpperChar) { c =>
      val d = Diamond(c)
      val rowSpaces = for (pair <- d.rows.zipWithIndex) yield (pair._1, Math.abs((d.rows.length / 2) - pair._2))
      rowSpaces.count(assertSpacesAtHead) == rowSpaces.length
    }
}
