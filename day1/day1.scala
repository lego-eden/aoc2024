object day1 extends Day:

  def parseInput(lines: IndexedSeq[String]): Vector[Vector[Long]] =
    lines.foldLeft(Vector(Vector.empty[Long], Vector.empty[Long])): (acc, s) =>
      val (nLeft, nRight) =
        (s.takeWhile(_ != ' ').toLong, s.dropWhile(_ != ' ').strip.toLong)
      Vector(acc(0) :+ nLeft, acc(1) :+ nRight)

  def partOne(lines: IndexedSeq[String]): Long =
    val Vector(left, right) = parseInput(lines).map(_.sorted)
    left.zip(right).map((l, r) => (l - r).abs).sum

  def partTwo(lines: IndexedSeq[String]): Long =
    val Vector(left, right) = parseInput(lines)
    val register = right.foldLeft(Map.empty[Long, Int]): (acc, n) =>
      acc + (n -> (acc.getOrElse(n, 0) + 1))

    left.map(n => n * register.getOrElse(n, 0)).sum
