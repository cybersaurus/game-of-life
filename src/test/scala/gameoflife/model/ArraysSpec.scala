package gameoflife.model

import Arrays.*
import Arrays.given

object ArraysSpec extends weaver.FunSuite with ArraysFixtures {

  test("getCellAt returns Some(1)") {
    expect.eql(Some(1), increasingCells3x4.getCellAt(0, 0))
  }

  test("getCellAt returns Some(8)") {
    expect.eql(Some(8), increasingCells3x4.getCellAt(1, 2))
  }

  test("getCellAt returns None") {
    expect.eql(None, increasingCells3x4.getCellAt(100, 100))
  }

  test("mapWithCoords applies supplied function to all elements") {
    val expected: Array[Array[Int]] =
      cellsWith(width = 3, height = 4, default = -1) {
        increasingInts(width = 3) andThen (_ * 2)
      }

    expect.eql(expected, increasingCells3x4.mapWithCoords { case (i, (_, _)) => i * 2 })
  }

  test("zipWithIndex adds coordinates to all elements") {
    val expected: Array[Array[(Int, (Int, Int))]] =
      cellsWith(width = 3, height = 4, default = (-1, -1 -> -1)) {
        increasingIntsWithCoords(width = 3)
      }

    expect.eql(expected, increasingCells3x4.zipWithCoords())
  }
}
