extension [A](a: A)
  def tap[U](f: A => U): A =
    f(a)
    a

  def log(): A = a.tap(println)