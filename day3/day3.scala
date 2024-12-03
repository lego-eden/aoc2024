object day3 extends Day:

  def mulSum(s: String): Long =
    val regex = raw"mul\((\d+),(\d+)\)".r
    regex.findAllMatchIn(s)
      .map(m => m.group(1).toLong * m.group(2).toLong)
      .sum

  override def partOne(lines: IndexedSeq[String]): Long =
    mulSum(lines.mkString("\n"))

  override def partTwo(lines: IndexedSeq[String]): Long =
    val mulRegex = raw"mul\((\d+),(\d+)\)".r
    lines.mkString("\n").split(raw"don't\(\)").zipWithIndex.map:
      case (s, 0) => mulSum(s)
      case (s"${_}do()$tail", _) => mulSum(tail)
      case _ => 0
    .sum
