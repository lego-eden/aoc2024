import scala.reflect.ClassTag
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*
import scala.compiletime.constValue

extension [A](a: A)
  def tap[U](f: A => U): A =
    f(a)
    a

  def log(): A = a.tap(println)

  infix def |>[B](f: A => B): B = f(a)

extension [A, CC[x] <: scala.collection.SeqOps[x, CC, CC[x]]](xs: CC[A])
  def remove(index: Int): CC[A] =
    xs.patch(index, Nil, 1)

  def splitFirst(elem: A): (CC[A], CC[A]) =
    val (init, tail) = xs.splitAt(xs.indexOf(elem))
    (init, tail.tail)

extension [A, CC[x] <: scala.collection.IterableOps[A, CC, CC[A]]](xs: CC[A])
  def headAndTail: (A, CC[A]) = (xs.head, xs.tail)

extension (s: String)
  def remove(index: Int): String =
    s.patch(index, Nil, 1)

  def splitFirst(elem: Char): (String, String) =
    val (init, tail) = s.splitAt(s.indexOf(elem))
    (init, tail.tail)

  def splitFirst(reg: String): (String, String) =
    reg.r.findFirstMatchIn(s) match
      case Some(regMatch) =>
        (s.take(regMatch.start), s.drop(regMatch.end))
      case None => ("", s)

  def headAndTail: (Char, String) = (s.head, s.tail)

extension [A: ClassTag](arr: Array[A])
  def remove(index: Int): Array[A] =
    arr.patch(index, Nil, 1)

  def splitFirst(elem: A): (Array[A], Array[A]) =
    val (init, tail) = arr.splitAt(arr.indexOf(elem))
    (init, tail.tail)

  def headAndTail: (A, Array[A]) = (arr.head, arr.tail)

type Invariant[T]

type Equal[A, B] <: Boolean = Invariant[A] match
  case Invariant[B] => true
  case _            => false

type HasSingleType[Tup <: Tuple] <: Boolean = Tup match
  case EmptyTuple => true
  case x *: xs    => Equal[Tuple.Head[Tup], x] && HasSingleType[xs]

type MapElem[Tup <: Tuple, Idx <: Int, New] = (Tup, Idx) match
  case (x *: xs, 0) => New *: xs
  case (x *: xs, _) => x *: MapElem[xs, Idx - 1, New]

type ZipWithIndex[Tup <: Tuple] = ZipWithIndexHelper[Tup, 0]

private type ZipWithIndexHelper[Tup <: Tuple, Idx] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case x *: xs    => (x, Idx) *: ZipWithIndexHelper[xs, S[Idx]]

extension (t: Tuple)(using (HasSingleType[t.type] =:= true))
  def map[B](f: Tuple.Head[t.type] => B): Tuple.Map[t.type, [_] =>> B] =
    t.map[[_] =>> B]([_] => elem => f(elem.asInstanceOf[Tuple.Head[t.type]]))

extension (t: Tuple)(using
    singleType: HasSingleType[t.type] =:= true,
    num: Numeric[Tuple.Head[t.type]]
)
  def *(scalar: Tuple.Head[t.type]): t.type =
    t.map(num.times(_, scalar)).asInstanceOf[t.type]

extension (t: Tuple)
  def mapElem[B](
      i: Int
  )(
      f: Tuple.Elem[t.type, i.type] => B
  ): MapElem[t.type, i.type, B] =
    type FTypeConstructor[T] = T match
      case (_, i.type) => B
      case (x, _)      => x

    t.zipWithIndex
      .map[FTypeConstructor](
        [typ] =>
          pair =>
            pair match
              case (elem, `i`) =>
                f(elem.asInstanceOf[Tuple.Elem[t.type, i.type]])
                  .asInstanceOf[FTypeConstructor[typ]]
              case (elem, _) => elem.asInstanceOf[FTypeConstructor[typ]]
      )
      .asInstanceOf[MapElem[t.type, i.type, B]]

  def zipWithIndex: ZipWithIndex[t.type] =
    def zipWithIndex(tup: Tuple, i: Int): ZipWithIndex[tup.type] =
      (
        (tup, i) match
          case (EmptyTuple, _) => EmptyTuple
          case (x *: xs, _)    => (x, i) *: zipWithIndex(xs, i + 1)
      ).asInstanceOf[ZipWithIndex[tup.type]]

    zipWithIndex(t, 0)
