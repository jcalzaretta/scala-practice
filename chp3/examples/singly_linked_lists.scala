sealed trait List[+A] //Creates new data type, sealed means all implementations contained in this file

// constructors.  empty list or nonempty.  Cons == construct
case object Nil extends List[Nothing]

// +A means List is polymorphic and contain any data types.  + means covariant, idk what this is.
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    // pattern => result
    case Nil => 1.0
    // _ is a wildcard, like *.  It could be any variable name, but _ is convention.  It also indicates
    // that we don't care about the contents, like golang
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //EXERCISE 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  //EXERCISE 3.3
  def setHead(list: List[A], value: A): List[A] = list match {
    case Cons(h, t) =>  (value, Cons(h, Cons t))
  }

  //EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  //EXERCISE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => 1
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

}
