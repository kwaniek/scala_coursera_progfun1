abstract class IntSet {
  def include(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def contains(x: Int): Boolean = false

  override def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def include(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left.include(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.include(x))
    else this
  }

  override def union(other: IntSet): IntSet = {
    (left union right union other) include elem
  }

  override def contains(x: Int): Boolean = {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }

  override def toString: String = "[" + left.toString + elem + right.toString + "]"
}

val a = Empty
val b = new NonEmpty(3, Empty, Empty)
val c = a.include(4)
val d = b.include(4)