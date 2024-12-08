import scala.collection.mutable.Set as MutSet
import scala.collection.mutable.Map as MutMap

object day8 extends Day:

  type Pos = (Int, Int)
  type BoundsCheck = Pos => Boolean
  type NodeGen = (Pos, Pos, BoundsCheck) => Set[Pos]

  extension (pos: Pos)
    def -(other: Pos): Pos =
      pos.zip(other).map(_ - _)

    def +(other: Pos): Pos =
      pos.zip(other).map(_ + _)

  def parseInput(
      lines: IndexedSeq[String]
  ): (Map[Char, Set[Pos]], BoundsCheck) =
    val result: MutMap[Char, MutSet[Pos]] = MutMap.empty
    lines.zipWithIndex.foreach: (line, row) =>
      line.zipWithIndex.foreach:
        case ('.', col) =>
        case (c, col) =>
          result
            .getOrElseUpdate(c, MutSet((row, col)))
            .add((row, col))
    (
      result.map((k, v) => (k, v.toSet)).toMap,
      (row, col) => lines.isDefinedAt(row) && lines(0).isDefinedAt(col)
    )
  end parseInput

  def antennaNodes1(
      antenna1: Pos,
      antenna2: Pos,
      boundsCheck: BoundsCheck
  ): Set[Pos] =
    val result = MutSet.empty[Pos]

    val pos1 = antenna2 + ((antenna1 - antenna2) * 2)
    val pos2 = antenna1 + ((antenna2 - antenna1) * 2)
    if boundsCheck(pos1) then result += pos1
    if boundsCheck(pos2) then result += pos2

    result.toSet

  def antennaNodes2(
      antenna1: Pos,
      antenna2: Pos,
      boundsCheck: BoundsCheck
  ): Set[Pos] =
    val result = MutSet.empty[Pos]
    Vector((antenna1, antenna2), (antenna2, antenna1)).foreach: (a1, a2) =>
      val delta = a1 - a2
      var n = 0
      var pos1 = a2
      var pos2 = a1
      while
        pos1 = pos1 + delta
        pos2 = pos2 - delta
        boundsCheck(pos1) || boundsCheck(pos2)
      do
        if boundsCheck(pos1) then result += pos1
        if boundsCheck(pos2) then result += pos2

    result.toSet
  end antennaNodes2

  def antiNodes(antennas: Map[Char, Set[Pos]], boundsCheck: BoundsCheck)(using
      nodeGen: NodeGen
  ): Set[Pos] =
    val result = MutSet.empty[Pos]

    for
      antennaGroup <- antennas.values
      antenna1 <- antennaGroup
      antenna2 <- antennaGroup - antenna1
    do result ++= nodeGen(antenna1, antenna2, boundsCheck)

    result.toSet

  override def partOne(lines: IndexedSeq[String]): Long =
    given NodeGen = antennaNodes1
    (parseInput(lines) |> antiNodes).size

  override def partTwo(lines: IndexedSeq[String]): Long =
    given NodeGen = antennaNodes2
    (parseInput(lines) |> antiNodes).size
