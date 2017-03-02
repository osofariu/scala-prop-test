package cannary

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class BasicCheckTest extends Properties("String test") {

  property("starts with") = forAll {(a: String, b: String) => (a + b).startsWith(a) }
  property("ends with") = forAll {(a: String, b: String) => (a + b).endsWith(b)}
}
