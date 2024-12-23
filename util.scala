import scala.reflect.ClassTag
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

extension [A](a: A)
  inline def tap[U](f: A => U): A =
    f(a)
    a

  inline def log(): A = a.tap(println)

  inline infix def |>[B](f: A => B): B = f(a)

extension [A](a: => A) def repeat(n: Int): Vector[A] = Vector.fill(n)(a)

extension [A: Integral as num](i: A)
  def isEven: Boolean = i % num.fromInt(2) == 0
  def isOdd: Boolean = !i.isEven
  @tailrec
  infix def gcd(j: A): A =
    if j == 0 then i
    else j gcd i % j

extension [A, CC[x] <: scala.collection.SeqOps[x, CC, CC[x]]](xs: CC[A])
  def remove(index: Int): CC[A] =
    xs.patch(index, Nil, 1)

  def splitFirst(elem: A): (CC[A], CC[A]) =
    val (init, tail) = xs.splitAt(xs.indexOf(elem))
    (init, tail.tail)

  def swap(i1: Int, i2: Int): CC[A] =
    val tmp = xs(i1)
    xs.updated(i1, xs(i2)).updated(i2, tmp)

  def splitAll(elem: A): Vector[CC[A]] =
    lazy val init = xs.takeWhile(_ != elem)
    lazy val tail = xs.dropWhile(_ != elem)

    if xs.isEmpty then Vector(xs)
    else if tail.isEmpty then Vector(init)
    else
      init +: tail.tail.splitAll(elem)

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
      case None => (s, "")

  def headAndTail: (Char, String) = (s.head, s.tail)

  def swap(i1: Int, i2: Int): String =
    val tmp = s(i1)
    s.updated(i1, s(i2)).updated(i2, tmp)

extension [A: ClassTag](arr: Array[A])
  def remove(index: Int): Array[A] =
    arr.patch(index, Nil, 1)

  def splitFirst(elem: A): (Array[A], Array[A]) =
    val (init, tail) = arr.splitAt(arr.indexOf(elem))
    (init, tail.tail)

  def headAndTail: (A, Array[A]) = (arr.head, arr.tail)

extension [T: Integral as num](p: (T, T))
  def +(other: (T, T)): (T, T) =
    (p(0) + other(0), p(1) + other(1))
  
  def -(other: (T, T)): (T, T) =
    (p(0) - other(0), p(1) - other(1))

  def *(scalar: T): (T, T) =
    (p(0) * scalar, p(1) * scalar)

  def %(scalar: T): (T, T) =
    (p(0) % scalar, p(1) % scalar)

  def %(other: (T, T)): (T, T) =
    (p(0) % other(0), p(1) % other(1))
  
  infix def gcd(other: (T, T)): (T, T) =
    (p(0) gcd other(0), p(1) gcd other(1))

final class Invariant[T]

type Equal[A, B] <: Boolean = Invariant[A] match
  case Invariant[B] => true
  case _            => false

type HasSingleType[Tup <: Tuple] =
  HasSingleTypeHelper[Tup, Tuple.Head[Tup]]

private type HasSingleTypeHelper[Tup <: Tuple, T] <: Boolean = Tup match
  case EmptyTuple => true
  case x *: xs    => Equal[T, x] && HasSingleTypeHelper[xs, T]

type MapElem[Tup <: Tuple, Idx <: Int, New] = (Tup, Idx) match
  case (x *: xs, 0) => New *: xs
  case (x *: xs, _) => x *: MapElem[xs, Idx - 1, New]

type ZipWithIndex[Tup <: Tuple] = ZipWithIndexHelper[Tup, 0]

private type ZipWithIndexHelper[Tup <: Tuple, Idx] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case x *: xs    => (x, Idx) *: ZipWithIndexHelper[xs, S[Idx]]

type Fill[Size <: Int, Elem] <: Tuple = Size match
  case 0 => EmptyTuple
  case _ => Elem *: Fill[Size - 1, Elem]

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

end extension
