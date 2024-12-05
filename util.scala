import scala.reflect.ClassTag

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

  def headAndTail: (Char, String) = (s.head, s.tail)

extension [A: ClassTag](arr: Array[A])
  def remove(index: Int): Array[A] =
    arr.patch(index, Nil, 1)

  def splitFirst(elem: A): (Array[A], Array[A]) =
    val (init, tail) = arr.splitAt(arr.indexOf(elem))
    (init, tail.tail)

  def headAndTail: (A, Array[A]) = (arr.head, arr.tail)
