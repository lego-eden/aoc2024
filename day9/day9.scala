import scala.annotation.targetName
object day9 extends Day:
  import Block.*
  enum Block(val length: Int):
    case File(id: Long, len: Int) extends Block(len)
    case Space(len: Int) extends Block(len)

    def toVector: Vector[Option[Long]] = this match
      case File(id, length) => Some(id).repeat(length)
      case Space(length) => None.repeat(length)

  def parseInput(lines: IndexedSeq[String]): Vector[Option[Long]] =
    var id = -1L
    lines.head.grouped(2).map(_.toVector).flatMap:
      case Vector(file, free) =>
        id += 1
        Some(id).repeat(file.asDigit) ++ None.repeat(free.asDigit)
      case Vector(file) => 
        id += 1
        Some(id).repeat(file.asDigit)
      case _ => ???
    .toVector

  extension (xs: Vector[Option[Long]])
    def compact: Vector[Option[Long]] =
      val dotIndices = xs.indices.filter(xs(_).isEmpty).iterator
      val numIndices = xs.indices.reverse.filter(xs(_).isDefined).iterator
      var seq = xs
      var dotIndex, numIndex = -1
      while
        dotIndex = dotIndices.next()
        numIndex = numIndices.next()
        dotIndex < numIndex
      do
        seq = seq.swap(numIndex, dotIndex)

      seq
    end compact

    def checksum: Long =
      xs.zipWithIndex
        .collect:
          case tup@(Some(n), i) => (n, i)
        .map(_ * _)
        .sum
  end extension

  def parseBlockInput(lines: IndexedSeq[String]): Vector[Block] =
    var id = -1L
    lines.head.grouped(2).map(_.toVector).flatMap:
      case Vector(file, free) =>
        id += 1
        Vector(File(id, file.asDigit), Space(free.asDigit))
      case Vector(file) => 
        id += 1
        Vector(File(id, file.asDigit))
      case _ => ???
    .toVector    

  extension (xs: Vector[Block])
    @targetName("blockCompact")
    def compact: Vector[Block] =
      val maxId = 
        xs.reverse.collectFirst:
          case File(id, len) => id
          case Space(len) => -1
        .get

      (maxId to 0 by -1).foldLeft(xs): (acc, id) =>
        acc.moveLeft(id)

    def moveLeft(id: Long): Vector[Block] =
      val (spaces, files) = xs.zipWithIndex.partitionMap:
        case (space@Space(len), i) => Left((space, i))
        case (file@File(id, len), i) => Right((file, i))
      
      val (file, fileIdx) = files.reverse.findLast((file, i) => file.id == id).get
      
      spaces.find((space, _) => space.len >= file.len) match
        case Some((space, spaceIdx)) if spaceIdx < fileIdx =>
          val remainingSpace =
            if space.len == file.len then
              Nil
            else
              Vector(Space(space.len - file.len))
          xs.updated(fileIdx, Space(file.len))
            .patch(spaceIdx, Vector(file) :++ remainingSpace, 1)
        case _ => xs
      
    end moveLeft

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines).compact.checksum

  override def partTwo(lines: IndexedSeq[String]): Long =
    parseBlockInput(lines)
      .compact
      .flatMap(_.toVector)
      .checksum
