import scala.collection.mutable.HashMap as MutMap

object day11 extends Day:

  case class Stone(num: Long):
    override lazy val toString: String = num.toString
    lazy val digitCount = toString.size

    def blink: Vector[Stone] =
      if num == 0 then Vector(Stone(1))
      else if digitCount.isEven then
        toString
          .splitAt(digitCount / 2)
          .map(s => Stone(s))
          .toList
          .toVector
      else Vector(Stone(num * 2024))

    val blinkN: Int => Long =
      def blinkN(n: Int): Long =
        if n <= 0 then 1
        else blink.map(_.blinkN(n - 1)).sum

      n => Stone.blinkMemo.getOrElseUpdate((this, n), blinkN(n))

  object Stone:
    private val blinkMemo = MutMap.empty[(Stone, Int), Long]
    def apply(s: String): Stone = new Stone(s.toLong)

  extension (xs: Vector[Stone])
    def blink(n: Int): Long =
      xs.map(_.blinkN(n)).sum

  def parseInput(lines: IndexedSeq[String]): Vector[Stone] =
    lines.head.split(" ").map(s => Stone(s)).toVector

  override def partOne(lines: IndexedSeq[String]): Long =
    parseInput(lines).blink(25)

  override def partTwo(lines: IndexedSeq[String]): Long =
    parseInput(lines).blink(75)
