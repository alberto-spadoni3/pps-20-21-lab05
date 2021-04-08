package u05lab.code

import scala.annotation.tailrec

object Ex4 {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
        var result: List[A] = List.nil
        @tailrec
        def _sequence(l: List[Option[A]]): Option[List[A]] = l match {
            case h :: t if h.isEmpty => None
            case h :: t => result = h.get :: result; _sequence(t)
            case _ => Option(result.reverse())
        }
        _sequence(a)
    }
}

object Tests extends App {
    import Ex4._
    val list: List[Option[Int]] = List(Some(1),Some(2),Some(3))
    println(sequence(list)) // Some(Cons(1,Cons(2,Cons(3,Nil()))))

    val listWithNone = list.append(List(Some(12), Some(67), None, Some(99)))
    println(sequence(listWithNone)) // None
}