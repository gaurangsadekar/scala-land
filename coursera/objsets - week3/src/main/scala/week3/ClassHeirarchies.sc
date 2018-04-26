package week3
// abstract classes can't be instantiated, methods have no body
// traits are a lot like interfaces in Java
// they also allow methods and vals to be defined, on top of
// interface like functionality.
trait IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

/*
this was originally a class, but since we need
only one empty set, it can be implemented as a
Singleton object
Syntax note =>
'object' is Scala for Singleton
(only one instance of the object is declared and referenced)
there is no 'new' KeyWord
objects are vals, so evaluated at declaration
'class' is Scala for prototype
(new instance of class is created every time the new keyword is used)
 */
object Empty extends IntSet {
  def contains(x: Int) = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < this.elem)
      left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem)
      new NonEmpty(elem, left incl x, right)
    else if (x > elem)
      new NonEmpty(elem, left, right incl x)
    else this
  }

  override def toString = "{" + left + elem + right + "}"
}

/*
testing here
 */
object ClassHeirarchies {
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
}
