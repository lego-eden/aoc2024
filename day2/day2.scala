object day2 extends Day:

  def parseInput(lines: IndexedSeq[String]): IndexedSeq[Vector[Long]] =
    lines.map(l => l.split(" ").map(_.toLong).toVector)

  def isCorrect(report: Vector[Long]): Boolean =
    val initSign = ((report(0) - report(1)).sign)
    report.sliding(2).forall(levels =>
      val Vector(l1, l2) = levels
      val delta = l1 - l2
      delta.abs >= 1 && delta.abs <= 3 && delta.sign == initSign
    )

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines).filter(isCorrect).size

  override def partTwo(lines: IndexedSeq[String]): Long = 
    parseInput(lines).filter(report =>
      (report +: report.indices.map(report.remove)).exists(isCorrect)
    ).size

