import scala.collection.mutable.{HashMap as MutMap, HashSet as MutSet}

object day12 extends Day:

  type Region = Set[Plot]
  type Pos = (Int, Int)
  enum Dir:
    case Up, Down, Left, Right
    lazy val dir: (Int, Int) = this match
      case Up    => (-1, 0)
      case Down  => (1, 0)
      case Left  => (0, -1)
      case Right => (0, 1)

    lazy val adjacent: Set[Dir] = this match
      case Up | Down    => Set(Left, Right)
      case Left | Right => Set(Up, Down)

  case class Plot(pos: Pos, sides: Set[Dir]):
    lazy val perimiter = sides.size
    def adjacent(dir: Dir): Set[Pos] =
      if !sides(dir) then Set.empty
      else dir.adjacent.map(_.dir + pos)

  private val knownRegions = MutMap.empty[Pos, Region]

  extension (region: Region)
    def apply(pos: Pos): Option[Plot] = region.find(_.pos == pos)
    def area: Long = region.size
    def perimiter: Long = region.toVector.map(_.perimiter).sum
    def sides: Long =
      def side(pos: Pos, dir: Dir, seen: Set[Pos]): Set[Pos] =
        region(pos)
          .map(plot =>
            if plot.sides(dir) then
              // plot.adjacent(dir).flatMap(p => side(p, dir, seen + pos)) + plot.pos
              plot
                .adjacent(dir)
                .filterNot(seen.apply)
                .flatMap(p => side(p, dir, seen + p))
            else seen
          )
          .getOrElse(seen)

      region
        .flatMap(plt =>
          plt.sides.map(dir => dir -> side(plt.pos, dir, Set(plt.pos)))
        )
        .size
    end sides

  extension (lines: IndexedSeq[String])
    def apply(pos: Pos): Option[Char] =
      lines.lift(pos(0)).flatMap(_.lift(pos(1)))

    def perimiter(pos: Pos): Long =
      lines.region(pos).find(_.pos == pos).get.perimiter

    def region(pos: Pos): Region =
      lazy val seen = MutSet(pos)

      def region(c: Char, pos: Pos): Region =
        val sides =
          Dir.values
            .filter(side =>
              val p = pos + side.dir
              lines(p).map(_ == c).getOrElse(false)
            )
            .toSet
        val neighbors = sides.map(_.dir + pos)

        val unseenNeighbors =
          neighbors.filterNot(seen.apply)

        seen += pos

        if unseenNeighbors.isEmpty then
          Set(Plot(pos, Dir.values.toSet -- sides))
        else
          Set(Plot(pos, Dir.values.toSet -- sides))
            ++ unseenNeighbors.flatMap(p => region(c, p))
      end region

      val result = lines(pos)
        .map(c => knownRegions.getOrElse(pos, region(c, pos)))
        // .map(c => region(c, pos, Set(pos)))
        .getOrElse(Set.empty)

      knownRegions.addAll(result.map(_.pos -> result))
      result
    end region

    def plot(pos: Pos): Plot =
      lines.region(pos).find(_.pos == pos).get

  def solve(lines: IndexedSeq[String], lenFunc: Region => Long): Long =
    val positions =
      for
        row <- lines.indices
        col <- lines(row).indices
      yield (row, col)

    positions
      .map(lines.region)
      .distinct
      .map(region => region.area * lenFunc(region))
      .sum
  end solve

  override def partOne(lines: IndexedSeq[String]): Long =
    solve(lines, _.perimiter)

  override def partTwo(lines: IndexedSeq[String]): Long =
    solve(lines, _.sides)
