extension [A](a: A)
  def tap[U](f: A => U): A =
    f(a)
    a

  def log(): A = a.tap(println)

extension [A, CC[x] <: scala.collection.SeqOps[x, CC, CC[x]]](xs: CC[A])
  def remove(index: Int): CC[A] =
    xs.patch(index, Nil, 1)

extension (s: String)
  def remove(index: Int): String =
    s.patch(index, Nil, 1)