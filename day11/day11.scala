import scala.collection.mutable.HashMap as MutMap

object day11 extends Day:

  case class Stone(num: BigInt):
    override lazy val toString: String = num.toString
    lazy val digitCount = toString.size  

    def blink: Vector[Stone] =
      if num == 0 then Vector(Stone(1))
      else if digitCount.isEven then
        toString.splitAt(digitCount / 2)
          .map(s => Stone(BigInt(s)))
          .toList
          .toVector
      else
        Vector(Stone(num * 2024))

    val blinkN: Int => BigInt =
      def blinkN(n: Int): BigInt =
        if n <= 0 then
          1
        else 
          blink.map(_.blinkN(n - 1)).sum

      n => Stone.blinkMemo.getOrElseUpdate((this, n), blinkN(n))
  
  object Stone:
    private val blinkMemo = MutMap.empty[(Stone, Int), BigInt]
    def apply(s: String): Stone = new Stone(BigInt(s))

  extension (xs: Vector[Stone])
    def blink(n: Int): BigInt =
      xs.map(_.blinkN(n)).sum

  def parseInput(lines: IndexedSeq[String]): Vector[Stone] =
    lines.head.split(" ").map(s => Stone(s)).toVector
  
  override def partOne(lines: IndexedSeq[String]): BigInt =
    parseInput(lines).blink(25)

  override def partTwo(lines: IndexedSeq[String]): BigInt =
    parseInput(lines).blink(75)
