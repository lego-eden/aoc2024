import scala.collection.mutable.Buffer
import scala.collection.mutable.Map as MutMap
import scala.util.Try
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object day10 extends Day:

  type Point = (Int, Int)
  type Grid = Array[Array[Int]]

  extension (g: Grid)
    def apply(point: (Int, Int)): Option[Int] =
      g.lift(point(0)).flatMap(_.lift(point(1)))

  extension (p: (Int, Int))
    def +(other: (Int, Int)): (Int, Int) =
      (p(0) + other(0), p(1) + other(1))

  extension (grid: Grid)
    def connected9s(point: Point): Vector[Point] =
      if grid(point).map(_ == 9).getOrElse(false) then Vector(point)
      else
        Vector((-1, 0), (0, 1), (1, 0), (0, -1))
          .map(_ + point)
          // .par
          .filter(
            grid(_)
              .flatMap(i => grid(point).map(j => i == j + 1))
              .getOrElse(false)
          )
          // .tapEach(p => println(s"$point -> $p"))
          .flatMap(p => connected9s(p))
    end connected9s

  def parseInput(lines: IndexedSeq[String]): (Grid, Vector[Point]) =
    var trailHeads = Buffer.empty[Point]
    (
      lines.zipWithIndex.map: (line, row) =>
        line.zipWithIndex.map: (c, col) =>
          val digit = Try(c.asDigit).getOrElse(-1)
          if digit == 0 then trailHeads += ((row, col))
          digit
        .toArray
      .toArray,
      trailHeads.toVector
    )

  override def partOne(lines: IndexedSeq[String]): Long =
    val (grid, trailHeads) = parseInput(lines)
    trailHeads.map(grid.connected9s).map(_.distinct.size).sum

  override def partTwo(lines: IndexedSeq[String]): Long =
    val (grid, trailHeads) = parseInput(lines)
    trailHeads.map(grid.connected9s).map(_.size).sum
