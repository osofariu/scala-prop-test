package diamond

case class Diamond(c: Char) {
  def rows : List[String] = c match {
    case char if char >= 'A' && char <= 'Z' =>
      val charRange = 'A' to char
      val firstHalfRange = charRange.zipWithIndex.map {
        case (c: Char, index: Int) => makeRow(c, index, charRange.length)
      }
      val secondHalfRange = firstHalfRange.reverse.tail
      firstHalfRange union secondHalfRange toList
    case other => throw new IllegalArgumentException("Invalid character=(" + other + ")")
  }

  def makeRow(char: Char, index: Int, rangeLen: Int) : String = {
    val before_spacing = " " * (rangeLen - index - 1)
    val middle_spacing = " " * (index * 2 - 1)

    if (index == 0)
      before_spacing + char
    else
      before_spacing + char + middle_spacing + char
  }

  def show() : Unit = {
    this.rows.foreach(row => println(row))
  }
}
