package diamond

import org.scalacheck.{Gen, Properties, Prop}

class DiamondPropTest extends Properties("diamond properties") {

  property("diamond has odd number of rows") =
    Prop.forAll(Gen.alphaUpperChar) { c: Char => Diamond(c).rows.length % 2 == 1 }


  property("every Diamond has A in first and last row") = {
    Prop.forAll(Gen.alphaUpperChar) { c =>
      val rows = Diamond(c).rows
      rows.head.trim == "A" && rows.last.trim == "A"
    }
  }

  // If we make property above fail by expecting something other than 'A' when scalacheck tries to
  // shrink the input it tries Char.MIN_VALUE which is outside the range of Gen.alphaUpperChar.
  //
  // This means the result with shrinking will be meaningless because you get an exception,
  // which has nothing to do with why the property failed in the fist place.

  property("diamond has N-1, N-2,.. 0, then 1, 2, .. N-1 spaces before first Char where N is size of Diamond") = {

    def expectedBeginningSpaces(pair: (String, Int)): Boolean = {
      pair._1.startsWith(" " * pair._2) && pair._1(pair._2) != ' '
    }

    Prop.forAll(Gen.alphaUpperChar) { c =>
      val rows = Diamond(c).rows
      val rowAndSpaces = for (pair <- rows.zipWithIndex) yield (pair._1, Math.abs((rows.length / 2) - pair._2))
      rowAndSpaces.forall(expectedBeginningSpaces)
    }
  }

  property("letters A, B, etc. appear twice sequentially in each row, except for A") = {
    Prop.forAll(Gen.alphaUpperChar) { c =>
      val rows = Diamond(c).rows
      val onlyChars = rows.map(_.replaceAll("\\s", ""))
      val uniqueChars = onlyChars.foldLeft[String]("")((acc: String, chars: String) => {
        if (chars.length == 2 && chars(0) == chars(1)) acc + chars.head else acc + chars
      })
      val expectedCharsSeen = ('A' to c) union ('A' to c).reverse.tail
      expectedCharsSeen.mkString("") == uniqueChars
    }
  }
}
