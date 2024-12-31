import java.lang.Math.floorMod

object day14 extends Day:

  type Vec = (Int, Int)

  extension (vec: Vec)
    infix def mod(other: Vec): Vec =
      (floorMod(vec(0), other(0)), floorMod(vec(1), other(1)))

  def room = if useExample then (11, 7) else (101, 103)

  case class Robot(pos: Vec, vel: Vec):
    def walk(seconds: Int): Robot =
      val newPos = (pos + vel * seconds) mod room
      copy(newPos)

    def quadrant: Option[Vec] =
      val center = room / 2
      pos.zip(center).map(_.compare(_)) match
        case (0, _) | (_, 0) => None
        case region => Some(region)

  def parseInput(lines: IndexedSeq[String]): IndexedSeq[Robot] =
    lines.map:
      case s"p=$px,$py v=$vx,$vy" => Robot((px.toInt, py.toInt), (vx.toInt, vy.toInt))

  def countQuadrants(robots: IndexedSeq[Robot]): Int =
    robots.groupBy(_.quadrant).collect:
      case (Some(_), robots) => robots
    .map(_.size)
    .product

  def isChristmasTree(robots: IndexedSeq[Robot]): Boolean =
    robots.exists(robot =>
      (1 to 9).map(offset => robot.pos + (offset, 0))
        .forall(pos => robots.exists(rb => rb.pos == pos))
    )

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines).map(_.walk(100)) |> countQuadrants

  override def partTwo(lines: IndexedSeq[String]): Long =
    val robots = parseInput(lines)
    LazyList.from(0).find(i =>
      robots.map(_.walk(i)) |> isChristmasTree
    ).get