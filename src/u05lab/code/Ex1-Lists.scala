package u05lab.code

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex.Match // silence warnings

sealed trait List[A] {
  def head: Option[A]

  def tail: Option[List[A]]

  def append(list: List[A]): List[A]

  def foreach(consumer: (A) => Unit): Unit

  def get(pos: Int): Option[A]

  def filter(predicate: (A) => Boolean): List[A]

  def map[B](fun: (A) => B): List[B]

  def size: Int

  def toSeq: Seq[A]

  def foldLeft[B](acc: B)(f: (B,A)=>B): B

  def foldRight[B](acc: B)(f: (A,B)=>B): B

  def flatMap[B](f: A => List[B]): List[B]

  def reverse(): List[A]

  def zipRight: List[(A,Int)]

  def partition(pred: A => Boolean): (List[A],List[A])

  def span(pred: A => Boolean): (List[A],List[A])

  def reduce(op: (A,A)=>A): A

  def takeRight(n: Int): List[A]

  def collect[B](pf: PartialFunction[A, B]): List[B]

  // right-associative construction: 10 :: 20 :: 30 :: Nil()
  def ::(head: A): List[A] = Cons(head,this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
  extends ListImplementation[A]

case class Nil[A]()
  extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
  def unapply[A](l: List[A]): Option[(A,List[A])] = l match {
    case Cons(h,t) => Some((h,t))
    case _ => None
  }
}

// List algorithms
trait ListImplementation[A] extends List[A] {
  override def head: Option[A] = this match {
    case h :: t => Some(h)
    case _ => None
  }

  override def tail: Option[List[A]] = this match {
    case h :: t => Some(t)
    case _ => None
  }

  override def append(list: List[A]): List[A] = this match {
    case h :: t => h :: (t append list)
    case _ => list
  }

  override def foreach(consumer: (A) => Unit): Unit = this match {
    case h :: t => {
      consumer(h); t foreach consumer
    }
    case _ => None
  }

  override def get(pos: Int): Option[A] = this match {
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t get (pos - 1)
    case _ => None
  }

  override def filter(predicate: (A) => Boolean): List[A] = this match {
    case h :: t if (predicate(h)) => h :: (t filter predicate)
    case _ :: t => (t filter predicate)
    case _ => Nil()
  }

  override def map[B](fun: (A) => B): List[B] = this match {
    case h :: t => fun(h) :: (t map fun)
    case _ => Nil()
  }

  override def size: Int = this match {
    case h :: t => 1 + t.size
    case _ => 0
  }

  override def toSeq: Seq[A] = this match {
    case h :: t => h +: t.toSeq // using method '+:' in Seq..
    case _ => Seq()
  }

  override def foldLeft[B](acc: B)(f: (B, A) => B): B = this match {
    case Cons(h, t) => t.foldLeft(f(acc, h))(f)
    case Nil() => acc
  }

  override def foldRight[B](acc: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(acc)((acc, elem) => f(elem, acc))

  override def reverse(): List[A] =
    this.foldLeft(Nil[A]().asInstanceOf[List[A]])((acc, elem) => Cons(elem, acc))

  override def flatMap[B](f: A => List[B]): List[B] = this match {
    case Cons(h, t) => f(h).append(t.flatMap(f))
    case Nil() => Nil()
  }

  override def zipRight: List[(A, Int)] = {
    @tailrec
    def _zipRight(l: List[A], k: Int = 0, res: List[(A, Int)] = List.nil): List[(A, Int)] = l match {
      case h :: t => _zipRight(t, k + 1, (h, k) :: res)
      case _ => res
    }

    _zipRight(this).reverse()

    /*var k = -1
    this.map(elem => {k += 1; (elem, k)})*/
  }

  override def partition(pred: A => Boolean): (List[A], List[A]) = {
    @tailrec
    def _partition(l: List[A], pTrue: List[A] = List.nil, pFalse: List[A] = List.nil): (List[A], List[A]) = l match {
      case h :: t if pred(h) => _partition(t, h :: pTrue, pFalse)
      case h :: t => _partition(t, pTrue, h :: pFalse)
      case _ => (pTrue.reverse(), pFalse.reverse())
    }

    _partition(this)
  }

  override def span(pred: A => Boolean): (List[A], List[A]) = {
    @tailrec
    def _span(l: List[A], pTrue: List[A] = List.nil): (List[A], List[A]) = l match {
      case h :: t if pred(h) => _span(t, h :: pTrue)
      case h :: t => (pTrue.reverse(), h :: t)
    }

    _span(this)
  }

  /**
    *
    * @throws UnsupportedOperationException if the list is empty
    */
  override def reduce(op: (A, A) => A): A = ???

  // List(12,23,32,2, nil)
  override def takeRight(n: Int): List[A] = this match {
    case h :: t if n > this.size => throw new IllegalArgumentException
    case h :: t if this.size - n > 1 => t.takeRight(n)
    case h :: t => t
    case Nil() => Nil()
  }

  override def collect[B](pf: PartialFunction[A, B]): List[B] = this match {
    case h :: t if pf.isDefinedAt(h) => Cons(pf(h), t.collect(pf))
    case h :: t => t.collect(pf)
    case _ => Nil()
  }
}

// Factories
object List {

  // Smart constructors
  def nil[A]: List[A] = Nil()
  def cons[A](h: A, t: List[A]): List[A] = Cons(h,t)

  def apply[A](elems: A*): List[A] = {
    var list: List[A] = Nil()
    for (i <- elems.length-1 to 0 by -1) list = elems(i) :: list
    list
  }

  def of[A](elem: A, n: Int): List[A] =
    if (n==0) Nil() else elem :: of(elem,n-1)
}

object ListsTest extends App {

  import List._  // Working with the above lists
  println(List(10,20,30,40))
  val l = 10 :: 20 :: 30 :: 40 :: Nil() // same as above
  println(l.head) // 10
  println(l.tail) // 20,30,40
  println(l append l) // 10,20,30,40,10,20,30,40
  println(l append l toSeq) // as a list: 10,20,30,40,10,20,30,40
  println(l get 2) // 30
  println(of("a",10)) // a,a,a,..,a
  println(l filter (_<=20) map ("a"+_) ) // a10, a20

  assert(List(1,2,3) == List(1,2,3))

  println(scala.collection.immutable.List(10,20,30,40).partition(_>15))
  println(scala.collection.immutable.List(10,20,30,40).span(_>15))

  // Ex. 1: zipRight
  println(l.zipRight.toSeq) // List((10,0), (20,1), (30,2), (40,3))

  // Ex. 2: partition
  println(l.partition(_>15)) // ( Cons(20,Cons(30,Cons(40,Nil()))), Cons(10,Nil()) )

  // Ex. 3: span
  println(l.span(_>15)) // ( Nil(), Cons(10,Cons(20,Cons(30,Cons(40,Nil())))) )
  println(l.span(_<15)) // ( Cons(10,Nil()), Cons(20,Cons(30,Cons(40,Nil()))) )

  // Ex. 4: reduce
//  println(l.reduce(_+_)) // 100
//  try {
//    List[Int]().reduce(_+_);
//    assert(false)
//  } catch {
//    case _:UnsupportedOperationException =>
//  }

  // Ex. 5: takeRight
  println(l.takeRight(2)) // Cons(30,Cons(40,Nil()))

  // Ex. 6: collect
  println(l.collect { case x if x<15 || x>35 => x-1 }) // Cons(9, Cons(39, Nil()))
}