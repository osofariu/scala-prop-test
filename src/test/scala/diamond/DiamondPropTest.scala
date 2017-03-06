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

  property("diamond has respectively: N-1, N-2,.. 0, 1, .. N-1 spaces before first Letter where N is # distinct chars in Diamond") = {

    def expectBeginningSpaces(pair: (String, Int)): Boolean = {
      pair match {
        case (row, beginningSpaces) => row.startsWith(" " * beginningSpaces) && row(beginningSpaces) != ' '
      }
    }

    Prop.forAll(Gen.alphaUpperChar) { c =>
      val rows = Diamond(c).rows
      val rowAndSpaces = for (pair <- rows.zipWithIndex) yield (pair._1, Math.abs((rows.length / 2) - pair._2))
      rowAndSpaces.forall(expectBeginningSpaces)
    }
  }

  property("letters in diamond appear twice sequentially in each row, except for A") = {
    def squishDuplicateLetter(chars: String) = {
      if (chars.length == 2 && chars.head == chars.tail.head) chars.head else chars
    }

    Prop.forAll(Gen.alphaUpperChar) { c =>
      val onlyLetters = Diamond(c).rows.map(_.replaceAll("\\s", ""))
      val actualUniqueLetters = onlyLetters.foldLeft("")((acc: String, chars: String) => {
        acc + squishDuplicateLetter(chars)
      })
      val expectedUniqueLetters = (('A' to c) union ('A' to c).reverse.tail).mkString("")

      expectedUniqueLetters == actualUniqueLetters
    }
  }

  property("Spaces between letters in Diamond are: 1, 3, .. N/2.. N/2-1, .. 1 except for first and last rows") = {
    Prop.forAll(Gen.alphaUpperChar) { c =>
      val rows = Diamond(c).rows
      val actualSpacesCounts = rows.map(_.trim).map(_.replaceAll("[A-Z]", "")).map(_.length)

      val indexesUp = (0 to 0) ++ (1 until rows.length by 2)
      val expectedSpacesCounts = indexesUp union indexesUp.reverse.tail

      expectedSpacesCounts == actualSpacesCounts
    }
  }
}
