trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

// putting val inside the constructor means that the
// class has a definition of head which is initialized with
// the constructor itself
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def nth[T](n: Int, xs: List[T]): T = {
  xs match {
    case xs: Nil[T] => throw new IndexOutOfBoundsException
    case xs: List[T] if (n == 0) => xs.head
    case _ => nth(n - 1, xs.tail)
  }
}

val l = new Cons[Int](1, new Cons(2, new Cons(3, new Nil[Int])))
nth(1, l)