import scala.util.boundary

object day7 extends Day:

  def parseInput(lines: IndexedSeq[String]): IndexedSeq[(BigInt, Vector[BigInt])] =
    lines.map(_.splitFirst(": ")
      .mapElem(0)(BigInt.apply)
      .mapElem(1): tail =>
        tail.split(' ').map(BigInt.apply).toVector
    )

  type Op = (BigInt, BigInt) => BigInt
  val part1Ops: Vector[Op] = Vector(_ + _, _ * _)
  val part2Ops: Vector[Op] =
    part1Ops :+ ((i1, i2) => BigInt(s"$i1$i2"))

  def isValid(ops: Vector[Op])(expected: BigInt, line: Vector[BigInt]): Boolean =
    def isValid(line: Vector[BigInt], acc: BigInt): Boolean =
      if expected == acc && line.isEmpty then true
      else if line.isEmpty then false
      else
        val helper: Op => Boolean =
          f => isValid(line.tail, f(acc, line.head))
        ops.exists(helper)

    isValid(line.tail, line.head)

  override def partOne(lines: IndexedSeq[String]): BigInt =
    parseInput(lines).filter((expected, line) => isValid(part1Ops)(expected, line))
      .map(_(0))
      .sum

  override def partTwo(lines: IndexedSeq[String]): BigInt =
    parseInput(lines).filter((expected, line) => isValid(part2Ops)(expected, line))
      .map(_(0))
      .sum