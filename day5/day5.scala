import scala.compiletime.ops.double
object day5 extends Day:

  def parseInput(
      lines: IndexedSeq[String]
  ): (Set[(Long, Long)], IndexedSeq[Vector[Long]]) =
    val (strRules, strUpdates) = lines.splitFirst("")
    val rules = strRules.map(
      _.splitFirst('|')
        .map[[_] =>> Long]([t] => s => s.asInstanceOf[String].toLong)
    )
    val updates = strUpdates.map(_.split(",").map(_.toLong).toVector)
    (rules.toSet, updates)

  def correctlyOrdered(rules: Set[(Long, Long)])(pages: Vector[Long]): Boolean =
    val pageSet = pages.toSet
    val activeRules = rules.filter:
      case (before, after) => pageSet(before) && pageSet(after)

    pages.indices.forall: i =>
      val page = pages(i)
      val pageRules = activeRules.filter:
        case (before, after) => before == page || after == page

      val (beforeSet, afterSet) = pageRules.partitionMap:
        case (before, after) if page == after => Left(before)
        case (_, after)                       => Right(after)

      val before = pages.take(i).toSet
      val after = pages.drop(i + 1).toSet

      val beforeOk = (beforeSet & after).isEmpty
      val afterOk = (afterSet & before).isEmpty

      beforeOk && afterOk

  def order(rules: Set[(Long, Long)])(pages: Vector[Long]): Vector[Long] =
    val pageSet = pages.toSet
    val beforeSets: Map[Long, Set[Long]] =
      val activeRules =
        rules
          .filter((before, after) => pageSet(before) && pageSet(after))
      pages
        .map(page =>
          val pageRules = activeRules.filter: (before, after) =>
            before == page || after == page
          val beforeSet = pageRules
            .filter((before, after) => page == after)
            .map((before, _) => before)
          page -> beforeSet
        )
        .toMap
    end beforeSets

    val customOrdering: Ordering[Long] =
      Ordering.fromLessThan((i1, i2) => beforeSets(i2)(i1))

    pages.sorted(using customOrdering)

  override def partOne(lines: IndexedSeq[String]): Long =
    val (rules, updates) = parseInput(lines)

    updates
      .filter(correctlyOrdered(rules))
      .map(xs => xs(xs.size / 2))
      .sum

  override def partTwo(lines: IndexedSeq[String]): Long =
    val (rules, updates) = parseInput(lines)

    updates
      .filterNot(correctlyOrdered(rules))
      .map(order(rules))
      .map(xs => xs(xs.size / 2))
      .sum
