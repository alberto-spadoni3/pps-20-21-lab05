package u05lab

import u05lab.code._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class SomeTest {
  @Test
  def testZipRight() {
    val list = List("a", "b", "c")
    val expected = List(("a", 0), ("b", 1), ("c", 2))
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(expected, list.zipRight)
  }

  @Test
  def testPartition(): Unit = {
    val list = List(10, 2, 6, 23, 9, 44, 0, 144)
    assertEquals((List(2,6,9,0), List(10,23,44,144)), list.partition(_<10))
    assertEquals((List.nil, List.nil), List.nil[Int].partition(_<10))
  }
}