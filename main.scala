trait Day:
  var useExample = false

  def partOne: Long | BigInt = partOne(if useExample then example else input)
  def partTwo: Long | BigInt = partTwo(if useExample then example else input)

  def partOne(lines: IndexedSeq[String]): Long | BigInt
  def partTwo(lines: IndexedSeq[String]): Long | BigInt

  lazy val input: IndexedSeq[String] =
    os.read.lines(os.pwd / toString / "input.txt")

  lazy val example: IndexedSeq[String] =
    os.read.lines(os.pwd / toString / "example.txt")

  override def toString: String = super.toString.takeWhile(_ != '$')

end Day

@main def main(): Unit =
  val day = day9
  day.useExample = false

  println(s"Part one: ${day.partOne}")
  println(s"Part two: ${day.partTwo}")
