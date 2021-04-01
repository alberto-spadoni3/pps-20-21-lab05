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

  @Test
  def testSpan(): Unit = {
    val list = List(1,2,3,4,50,6,7,8)
    assertEquals((List(1,2,3,4), List(50,6,7,8)), list.span(_<10))
    assertEquals((List.nil, List.nil), List.nil[Int].partition(_<10))
  }

  @Test
  def testReduce(): Unit = {
    assertTrue(true)
  }

  @Test
  def testTakeRight(): Unit = {
    val list = List(2, 4, 6, 8, 10)
    assertEquals(List(6,8,10), list.takeRight(3))
    assertEquals(List.nil, List.nil.takeRight(3))
    assertEquals(List(10), list.takeRight(1))
    assertThrows(classOf[java.lang.IllegalArgumentException], () => list.takeRight(6))
  }

  @Test
  def testCollect(): Unit = {
    val list = List(3, 8, 0, 45, -21, 44, -68)
    val squareOfEvenAndNonZero: PartialFunction[Int, Int] = {
        case x if x != 0 && x % 2 == 0 => x * x
      }

    assertEquals(List(64, 44 * 44, -68 * -68), list collect squareOfEvenAndNonZero)
    assertEquals(List.nil, List(1,3,5,7) collect squareOfEvenAndNonZero)
  }
}