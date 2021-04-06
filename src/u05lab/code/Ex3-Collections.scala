package u05lab.code

import java.util.concurrent.TimeUnit
import scala.collection.immutable._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}


object CollectionsTest extends App {
  import scala.collection._

  /* Linear sequences: List, ListBuffer */
  val list = immutable.List(0,1,2)
  val list2 = immutable.List.range(2, 10)
  println(12 :: list)
  println(list ::: list2)
  println(list2 :+ 10)
  println()

  val listBuffer = ListBuffer("ciao")
  println(listBuffer += "a tutti")
  println(listBuffer += "ragazzi")
  println(listBuffer -= "ciao")
  println(listBuffer(1))
  println(listBuffer.head)
  println()

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val vector = Vector(10, 20)
  println(vector.getClass)
  println(vector.reverse)
  println(vector(1))
  println(vector.head)
  println()

  val array = Array(3.1415, 2, 443.23)
  println(array.getClass)
  println(array.length) // 3
  println(array.contains(3.1415)) // true
  println()

  val arrayBuffer = ArrayBuffer(36)
  println(arrayBuffer += 21 += 100 += 2500)
  println(arrayBuffer.length) // 4
  println(arrayBuffer ++= list += (-3,-4,-5,-6))
  println(arrayBuffer --= list -= 2500)
  println()

  /* Sets */
  val set: Set[String] = Set("a", "b")
  println(set + "c" + "d...")
  println(set.contains("a")) // true
  println(set & Set("b", "c")) // Set(b)
  println(set & Set("d...")) // Set()

  val mutableSet = mutable.Set("a", "b")
  println(mutableSet += ("c", "d...")) // Set(c, a, d..., b)
  println(mutableSet & mutable.Set("b", "c")) // Set(c, b)
  println(mutableSet & mutable.Set("d...")) // Set(d...)
  println()

  /* Maps */
  val map = Map(0 -> "zero", 10 -> "ten", 20 -> "twenty")
  println(map)
  println(map.keys)
  println(map.values)
  println(map.keySet)
  println(map(20))
  println(map map {case (key, value) => (key, "["+value+"]")})
  println()

  val mutableMap = mutable.Map(1 -> "one")
  println(mutableMap += (2 -> "two") += (3 -> "three")) // Map(2 -> two, 1 -> one, 3 -> three)
  println(mutableMap - 1)
  println()

  /* Comparison */
  // I'm gonna compare immutable IndexedSeq with immutable LinearSeq
  import PerformanceTests._
  accessingLastElement()
  addingElements()
  searchingElement()
  updateElement()
}

object PerformanceTests {
  import PerformanceUtils._
  val NUM_OF_REPETITIONS: Byte = 5
  val LIST_SIZE = 1000000
  val random = new Random()
  var indexedSeq: IndexedSeq[Int] = IndexedSeq.range(1, LIST_SIZE)
  var linearSeq: LinearSeq[Int] = LinearSeq.range(1, LIST_SIZE)

  def accessingLastElement(): Unit = {
    println("Accessing last element".toUpperCase)
    loop({
      measure("IndexedSeq") { indexedSeq.last }
      measure("LinearSeq") { linearSeq.last }
    }, iterations = 3)
  }

  def addingElements(): Unit = {
    println("Adding elements".toUpperCase)
    loop({
      val elemToAdd = 1 to random.nextInt(LIST_SIZE)
      println("Adding " + elemToAdd.size + " elements...")
      measure("IndexedSeq"){indexedSeq ++= elemToAdd}
      measure("LinearSeq"){linearSeq ++= elemToAdd}
    })
  }

  def searchingElement(): Unit = {
    println("Searching elements".toUpperCase)
    loop({
      val elemToSearch = random.nextInt(LIST_SIZE - 1)
      println("Searching elem -> " + elemToSearch)
      measure("IndexedSeq"){indexedSeq.contains(elemToSearch)}
      measure("LinearSeq"){linearSeq.contains(elemToSearch)}
    })
  }

  def updateElement(): Unit = {
    println("Updating one element".toUpperCase)
    loop({
      val elemToUpdate = random.nextInt(1000)
      val index = random.nextInt(LIST_SIZE)
      println(s"Updating elem at index $index to value $elemToUpdate")
      measure("IndexedSeq"){indexedSeq.updated(index, elemToUpdate)}
      measure("LinearSeq"){linearSeq.updated(index, elemToUpdate)}
    })
  }

  def loop(action: => Unit, iterations: Int = NUM_OF_REPETITIONS): Unit = {
    println("#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#")
    for (i <- 1 to iterations) {
      println("Iteration n. " + i)
      action
    }
    println("#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#\n")
  }
}