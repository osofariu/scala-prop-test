package cannary

import org.scalatest.{Matchers, path}

class BasicUnitTest extends path.FunSpec with Matchers {

  describe("String test") {

    it("startsWith on a concatenated string always starts with the first one") {
      val str1 = "hello"
      val str2 = "world"
      (str1 + str2).startsWith(str1) shouldBe true
    }
  }
}
