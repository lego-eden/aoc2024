object day15 extends Day:

  type Pos = (Int, Int)

  def charToMove(char: Char): (Int, Int) = char match
    case '^' => (-1, 0)
    case 'v' => (1, 0)
    case '<' => (0, -1)
    case '>' => (0, 1)

  case class Room(pos: Pos, walls: Set[Pos], boxes: Set[Pos]):
    @tailrec
    def move(moves: IndexedSeq[(Int, Int)]): Room =
      if moves.isEmpty ||  then this

    def inFront(dir: (Int, Int)): Vector[Pos] =
      ???

  def parseInput(lines: IndexedSeq[String]): (Room, IndexedSeq[(Int, Int)]) =
    val (roomStr, movesStr) = lines.splitFirst("")

    val positions = roomStr.indices.flatMap(row =>
      roomStr(row).indices.map(col => (row, col))
    ).groupBy((row, col) =>
      roomStr(row)(col)
    )

    val moves = movesStr.mkString.map(charToMove)

    (Room(positions('@').head, positions('#').toSet, positions('O').toSet), moves)

  end parseInput

  override def partOne(lines: IndexedSeq[String]): Long = ???

  override def partTwo(lines: IndexedSeq[String]): Long = ???
