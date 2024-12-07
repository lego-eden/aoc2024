import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

object day6 extends Day:
  enum Tile:
    case Empty, Obstacle
    case Visited(dirs: Set[(Int, Int)])

  case class Grid(grid: Vector[Vector[Tile]], guardPos: (Int, Int), guardDir: (Int, Int), uniqueTiles: Int = 0):
    
    lazy val nextGuardPos = guardPos.zip(guardDir).map(_ + _)
    
    lazy val next: Grid =
      val (nextRow, nextCol) = nextGuardPos
      grid(nextRow)(nextCol) match
        case Tile.Empty =>
          setTile(nextRow, nextCol, Tile.Visited(Set(guardDir))).copy(uniqueTiles = uniqueTiles + 1)
            .copy(guardPos = (nextRow, nextCol))
        case Tile.Obstacle =>
          val (hdir, vdir) = guardDir
          setGuardDir(vdir, -hdir).next
        case Tile.Visited(dirs) =>
          setTile(nextRow, nextCol, Tile.Visited(dirs + guardDir))
            .copy(guardPos = (nextRow, nextCol))

    def setTile(row: Int, col: Int, tile: Tile): Grid =
      copy(grid = grid.updated(row, grid(row).updated(col, tile)))

    def setGuardDir(dir: (Int, Int)): Grid =
      copy(guardDir = dir)
    
    @tailrec
    final def walk: Grid =
      Try(next) match
        case Failure(_) => this
        case Success(newGrid) => newGrid.walk

    @tailrec
    final def doesLoop: Boolean =
      Try(next) match
        case Failure(_) => false
        case Success(newGrid) =>
          val (nextRow, nextCol) = nextGuardPos
          grid(nextRow)(nextCol) match
            case Tile.Visited(dirs) if dirs(guardDir) => true
            case _ => newGrid.doesLoop

    lazy val placeObstacle: Option[Grid] =
      val (nextRow, nextCol) = nextGuardPos
      grid.lift(nextRow).flatMap(_.lift(nextCol)) match
        case Some(Tile.Empty) => Some(setTile(nextRow, nextCol, Tile.Obstacle))
        case _ => None
  end Grid

  def parseInput(lines: IndexedSeq[String]): Grid =
    var guardPos = (0, 0)
    Grid(
      lines.zipWithIndex.map: line =>
        val row = line(1)
        line(0).zipWithIndex.map:
          case ('.', _) => Tile.Empty
          case ('#', _) => Tile.Obstacle
          case ('^', col) =>
            guardPos = (row, col)
            Tile.Visited(Set[(Int, Int)]((-1, 0)))
        .toVector
      .toVector,
      guardPos,
      (-1, 0),
      1
    )

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines).walk.grid.map: line =>
      line.count(_.isInstanceOf[Tile.Visited])
    .sum

  override def partTwo(lines: IndexedSeq[String]): Long =
    var grid = parseInput(lines)
    var possiblePlacements = 0
    while Try{ grid = grid.next }.isSuccess do
      grid.placeObstacle.foreach: g =>
        if g.doesLoop then possiblePlacements += 1

    possiblePlacements
