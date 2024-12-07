import scala.annotation.tailrec
import scala.collection.mutable.Set as MutSet

object day6 extends Day:
  type Dir = (Int, Int)
  type Pos = (Int, Int)
  extension (pos: Pos)
    infix def +(other: Dir) = pos.zip(other).map(_ + _)

  case class Grid(obs: Set[Pos], isDefinedAt: Pos => Boolean):
    def block(pos: Pos): Grid =
      copy(obs + pos)

  case class Guard(pos: Pos, dir: Dir)(using givenGrid: Grid):
    val grid = givenGrid
    lazy val nextPos: Pos = pos + dir
    lazy val next: Guard = copy(nextPos)

    lazy val walk: Set[Guard] =
      @tailrec
      def walk(guard: Guard, path: Set[Guard]): Set[Guard] =
        if guard.grid.isDefinedAt(nextPos) then
          val (vdir, hdir) = guard.dir
          if grid.obs(nextPos) then
            walk(guard.copy(dir = (hdir, -vdir)), path)
          else
            walk(guard.next, path + guard)
        else
          Set(guard)
      
      walk(this, Set.empty)
    
    lazy val doesLoop: Boolean = ???

  def parseInput(lines: IndexedSeq[String]): (Grid, Guard) =
    val obstacles = MutSet.empty[(Int, Int)]
    var guardPos = (0, 0)
    lines.zipWithIndex.map: (line, row) =>
      line.zipWithIndex.map:
        case ('^', col) =>
          guardPos = (row, col)
        case ('#', col) =>
          obstacles.add((row, col))
        case _ =>

    given grid: Grid =
      Grid(
        obstacles.toSet,
        (row: Int, col: Int) => lines.isDefinedAt(row) && lines(0).isDefinedAt(col)
      )

    (grid, Guard(guardPos, (-1, 0)))

  override def partOne(lines: IndexedSeq[String]): Long =
    val (grid, guard) = parseInput(lines)
    guard.walk.map(_.pos).size

  override def partTwo(lines: IndexedSeq[String]): Long =
    val (grid, guard) = parseInput(lines)
    given Grid = grid
    0