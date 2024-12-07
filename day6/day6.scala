import scala.annotation.tailrec
import scala.collection.mutable.Set as MutSet
import scala.collection.parallel.CollectionConverters.ImmutableSetIsParallelizable

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
    lazy val next: Guard = 
      if grid.obs(nextPos) then
        val (vdir, hdir) = dir
        copy(dir = (hdir, -vdir))
      else
        copy(nextPos)

    lazy val walk: Set[Guard] =
      @tailrec
      def walk(guard: Guard, path: Set[Guard]): Set[Guard] =
        if guard.grid.isDefinedAt(guard.nextPos) then
          walk(guard.next, path + guard)
        else
          path + guard
      
      walk(this, Set.empty)
    
    lazy val doesLoop: Boolean = 
      @tailrec
      def doesLoop(guard: Guard, path: Set[Guard]): Boolean =
        if !guard.grid.isDefinedAt(guard.nextPos) then
          false
        else
          if path(guard) then true
          else doesLoop(guard.next, path + guard)
      
      doesLoop(this, Set.empty)
        
  end Guard

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
        (row, col) => lines.isDefinedAt(row) && lines(0).isDefinedAt(col)
      )

    (grid, Guard(guardPos, (-1, 0)))

  override def partOne(lines: IndexedSeq[String]): Long =
    val (grid, guard) = parseInput(lines)
    guard.walk.map(_.pos).size

  override def partTwo(lines: IndexedSeq[String]): Long =
    val (grid, guard) = parseInput(lines)
    guard.walk.par
      .map(g => grid.block(g.pos))
      .filter(g =>
        Guard(guard.pos, guard.dir)(using g).doesLoop
      )
      .size
