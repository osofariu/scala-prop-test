package diamond

case class Diamond(c: Char) {
  def rows : IndexedSeq[String] = c match {
    case 'A' => IndexedSeq("A")
    case char if char > 'A' && char <= 'Z' =>
      val charRange = 'A' to char
      val firstHalfRange = charRange.zipWithIndex.map((pair) => makeRow(pair._1, pair._2, charRange.length))
      val secondHalfRange = firstHalfRange.reverse.tail
      firstHalfRange union secondHalfRange
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
