object day2 extends Day:

  def parseInput(lines: IndexedSeq[String]): IndexedSeq[Vector[Long]] =
    lines.map(l => l.split(" ").map(_.toLong).toVector)

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines).filterNot(report =>
      val initSign = (report(0) - report(1)).sign
      report.sliding(2).exists(levels =>
        val Vector(l1, l2) = levels
        val delta = l1 - l2
        delta.abs < 1 || delta.abs > 3 || delta.sign != initSign
      )
    ).size

  override def partTwo(lines: IndexedSeq[String]): Long = 
    parseInput(lines).filter(report =>
      (report +: report.indices.map(report.remove)).exists(rep =>
        val initSign = (rep(0) - rep(1)).sign
        rep.sliding(2).forall(levels =>
          val Vector(l1, l2) = levels
          val delta = l1 - l2
          delta.abs >= 1 && delta.abs <= 3 && delta.sign == initSign
        )
      )
    ).size

