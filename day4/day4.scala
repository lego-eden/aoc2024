import scala.util.{Try, Success, Failure}

object day4 extends Day:

  val directions =
    for
      hdir <- -1 to 1
      vdir <- -1 to 1
      if (hdir, vdir) != (0, 0)
    yield (hdir, vdir)

  def locations(row: Int, col: Int): IndexedSeq[IndexedSeq[(Int, Int)]] =
    directions.map(dir =>
      val (drow, dcol) = dir
      for i <- 0 to 3 yield (row + drow * i, col + dcol * i)
    )

  def isMas(s: String): Boolean = s.strip == "MAS" || s.strip == "SAM"
  def xCoords(row: Int, col: Int): Vector[IndexedSeq[(Int, Int)]] =
    Vector((-1 to 1).zip(-1 to 1), (-1 to 1).zip(1 to -1 by -1))
      .map(offsets => offsets.map(offset => (row + offset(0), col + offset(1))))

  override def partOne(lines: IndexedSeq[String]): Long =
    var count = 0
    for
      row <- lines.indices
      col <- lines(row).indices
      maybeXmas <- locations(row, col)
      str <- Try:
        maybeXmas
          .map:
            case (r, c) => lines(r)(c)
          .mkString
      if str == "XMAS"
    do count += 1

    count
  end partOne

  override def partTwo(lines: IndexedSeq[String]): Long =
    lines.indices
      .map: row =>
        lines(row).indices
          .filter: col =>
            xCoords(row, col).forall: coords =>
              Try:
                coords
                  .map:
                    case (r, c) => lines(r)(c)
                  .mkString |> isMas
              .getOrElse(false)
          .size
      .sum
  end partTwo
