object day13 extends Day:

  type Pos = (Long, Long)

  case class ClawMachine(da: Pos, db: Pos, prize: Pos):

    lazy val minimumTokens: Option[Long] =
      val b = (prize(1) * da(0) - prize(0) * da(1)) / (db(1) * da(0) - db(0) * da(1))
      val a = (prize(0) - b * db(0)) / da(0)

      if da * a + db * b == prize then Some(a * 3 + b)
      else None
      
  end ClawMachine

  def parseInput(lines: IndexedSeq[String]): IndexedSeq[ClawMachine] =
    lines.splitAll("").map(clawMachine => 
      val da = clawMachine(0) match
        case s"Button A: X+$dx, Y+$dy" => (dx.toLong, dy.toLong)
      val db = clawMachine(1) match
        case s"Button B: X+$dx, Y+$dy" => (dx.toLong, dy.toLong)
      val prize = clawMachine(2) match
        case s"Prize: X=$x, Y=$y" => (x.toLong, y.toLong)
      ClawMachine(da, db, prize)
    )
  
  def parseInput2(lines: IndexedSeq[String]): IndexedSeq[ClawMachine] =
    parseInput(lines).map(machine => machine.copy(prize = machine.prize + ((1L, 1L) * 10000000000000L)))

  def solve(machines: IndexedSeq[ClawMachine]): Long =
    machines.map(_.minimumTokens).collect:
      case Some(tokens) => tokens
    .sum

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines) |> solve

  override def partTwo(lines: IndexedSeq[String]): Long =
    parseInput2(lines) |> solve
