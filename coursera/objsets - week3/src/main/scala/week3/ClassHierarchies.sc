abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

/* since empty is essentially the null class,
   it doesn't need to make new instances of empty classes every time
   so its best to use it as an object
   that gives a new NonEmpty of addition of a single element
 */
object Empty extends IntSet  {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  def union(other: IntSet) = other
}

/*
  implemented as a binary tree
  recursive definitions of everything
 */
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) left incl x
    else if (x > elem) right incl x
    else this
  }

  override def toString = "{" + left + elem + right + "}"

  def union(other: IntSet) = ((left union right) union other) incl elem
}

val s1 = Empty incl 3
s2 =

