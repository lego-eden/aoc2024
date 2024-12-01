trait Day:
  var useExample = false

  def partOne: Long = partOne(if useExample then example else input)
  def partTwo: Long = partTwo(if useExample then example else input)

  def partOne(lines: IndexedSeq[String]): Long
  def partTwo(lines: IndexedSeq[String]): Long

  lazy val input: IndexedSeq[String] =
    os.read.lines(os.pwd/toString/"input.txt")
  
  lazy val example: IndexedSeq[String] =
    os.read.lines(os.pwd/toString/"example.txt")

  override def toString: String = super.toString.takeWhile(_ != '$')
  
end Day

@main def main(): Unit =
  val day = day1
  day.useExample = false

  println(s"Part one: ${day.partOne}")
  println(s"Part two: ${day.partTwo}")