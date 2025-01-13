package gameoflife.model

import Arrays.*
import Arrays.given

object ArraysSpec extends weaver.FunSuite with ArrayGridFixtures {
  test("rotateColsLeft") {
    val expected: Array[Array[Int]] = Array(
      Array(2, 3, 1),
      Array(5, 6, 4),
      Array(8, 9, 7)
    )

    expect.eql(expected, int3x3Grid().cells.rotateColsLeft)
  }

  test("rotateColsRight") {
    val expected: Array[Array[Int]] = Array(
      Array(3, 1, 2),
      Array(6, 4, 5),
      Array(9, 7, 8)
    )

    expect.eql(expected, int3x3Grid().cells.rotateColsRight)
  }

  test("rotateRowsUp") {
    val expected: Array[Array[Int]] = Array(
      Array(4, 5, 6),
      Array(7, 8, 9),
      Array(1, 2, 3)
    )

    expect.eql(expected, int3x3Grid().cells.rotateRowsUp)
  }

  test("rotateRowsDown") {
    val expected: Array[Array[Int]] = Array(
      Array(7, 8, 9),
      Array(1, 2, 3),
      Array(4, 5, 6)
    )

    expect.eql(expected, int3x3Grid().cells.rotateRowsDown)
  }
}
