package diamond

import org.scalatest.{Matchers, path}

class DiamondTest extends path.FunSpec with Matchers {

  describe("diamond of valid letters") {

    it("diamond of A is A") {
      Diamond('A').rows should === (List("A"))
    }

    it("diamond of B has three rows with expected spacing") {
      Diamond('B').rows should === (List(" A", "B B", " A"))
    }

    it("diamond of C has five rows with expected spacing") {
      Diamond('C').rows should === (List("  A", " B B", "C   C", " B B", "  A"))
    }
  }

  describe("invalid characters") {

    it("diamond of '0' throws exception") {
      intercept[IllegalArgumentException] {
        Diamond('0').rows
      }
    }
  }
}
