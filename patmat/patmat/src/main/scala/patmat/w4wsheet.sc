trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: T = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")
}

//list(1,2) = List.apply(1,2)
def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))


trait Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Var(x: Var) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + " + " + show(r)
  case Var(x) => x.toString
}

show(Sum(Number(1), Number(44)))
