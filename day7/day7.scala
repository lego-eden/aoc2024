import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

object day7 extends Day:

  def parseInput(lines: IndexedSeq[String]): IndexedSeq[(Long, Vector[Long])] =
    lines.map(
      _.splitFirst(": ")
        .mapElem(0)(_.toLong)
        .mapElem(1): tail =>
          tail.split(' ').map(_.toLong).toVector
    )

  type Op = (Long, Long) => Long

  val part1Ops: Vector[Op] = Vector(_ + _, _ * _)
  val part2Ops: Vector[Op] =
    part1Ops :+ ((i1, i2) => s"$i1$i2".toLong)

  def isValid(ops: Vector[Op])(expected: Long, line: Vector[Long]): Boolean =
    def isValid(line: Vector[Long], acc: Long): Boolean =
      if expected == acc && line.isEmpty then true
      else if line.isEmpty || acc > expected then false
      else
        val helper: Op => Boolean =
          op => isValid(line.tail, op(acc, line.head))
        ops.exists(helper)

    isValid(line.tail, line.head)

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines)
      .filter((expected, line) => isValid(part1Ops)(expected, line))
      .map(_(0))
      .sum

  override def partTwo(lines: IndexedSeq[String]): Long =
    parseInput(lines).par
      .filter((expected, line) => isValid(part2Ops)(expected, line))
      .map(_(0))
      .sum
