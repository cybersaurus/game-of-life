package gameoflife.model

import Arrays.given

object GridSpec extends weaver.FunSuite with GridFixtures {

  test("cellAt returns 123") {
    expect.eql(123, int3x4Grid().setAt(2, 3, 123).cellAt(2, 3))
  }

  test("getCellAt returns Some(123)") {
    expect.eql(Some(123), int3x4Grid().setAt(2, 3, 123).getCellAt(2, 3))
  }

  test("getCellAt returns None for invalid coords") {
    expect.eql(None, int3x4Grid().getCellAt(100, 100))
  }

  test("map applies supplied function to all elements") {
    val expectedGrid: Grid[Int] = increasingGrid(width = 3, height = 4, inc = 2)

    expect.eql(expectedGrid, int3x4Grid().map(_ * 2))
  }

  test("zipWithIndex adds grid coordinates to all elements") {
    val expectedGrid: Grid[(Int, (Int, Int))] = Grid
      .of(width = 3, height = 4, empty = (0, -1 -> -1))
      .setAt(x = 0, y = 0, cell = (1, 0 -> 0))
      .setAt(x = 1, y = 0, cell = (2, 1 -> 0))
      .setAt(x = 2, y = 0, cell = (3, 2 -> 0))
      .setAt(x = 0, y = 1, cell = (4, 0 -> 1))
      .setAt(x = 1, y = 1, cell = (5, 1 -> 1))
      .setAt(x = 2, y = 1, cell = (6, 2 -> 1))
      .setAt(x = 0, y = 2, cell = (7, 0 -> 2))
      .setAt(x = 1, y = 2, cell = (8, 1 -> 2))
      .setAt(x = 2, y = 2, cell = (9, 2 -> 2))
      .setAt(x = 0, y = 3, cell = (10, 0 -> 3))
      .setAt(x = 1, y = 3, cell = (11, 1 -> 3))
      .setAt(x = 2, y = 3, cell = (12, 2 -> 3))

    expect.eql(expectedGrid, int3x4Grid().zipWithIndex)
  }

  test("withOffsetIndex") {
    val expectedGrid: Grid[(Int, (Int, Int))] = Grid
      .of(width = 3, height = 4, empty = (0, -1 -> -1))
      .setAt(x = 0, y = 0, cell = (1, 10 -> 20))
      .setAt(x = 1, y = 0, cell = (2, 11 -> 20))
      .setAt(x = 2, y = 0, cell = (3, 12 -> 20))
      .setAt(x = 0, y = 1, cell = (4, 10 -> 21))
      .setAt(x = 1, y = 1, cell = (5, 11 -> 21))
      .setAt(x = 2, y = 1, cell = (6, 12 -> 21))
      .setAt(x = 0, y = 2, cell = (7, 10 -> 22))
      .setAt(x = 1, y = 2, cell = (8, 11 -> 22))
      .setAt(x = 2, y = 2, cell = (9, 12 -> 22))
      .setAt(x = 0, y = 3, cell = (10, 10 -> 23))
      .setAt(x = 1, y = 3, cell = (11, 11 -> 23))
      .setAt(x = 2, y = 3, cell = (12, 12 -> 23))

    expect.eql(expectedGrid.cells, Grid.withOffsetIndex(10, 20)(int3x4Grid().cells))
  }

  test("slice") {
    val expected: Array[Array[Int]] = Array(
      Array(5, 6),
      Array(8, 9)
    )

    expect.eql(expected, int3x4Grid().slice(xFrom = 1, xTo = 2, yFrom = 1, yTo = 2))
  }

  test("neighbours of middle/middle cell on 3x3 grid") {
    val expected: Array[Array[Int]] = Array(
      Array(1, 2, 3),
      Array(4, 6),
      Array(7, 8, 9)
    )

    expect.eql(expected, int3x3Grid().neighbours(1, 1))
  }

  test("neighbours of middle/bottom cell on 3x3 grid") {
    val expected: Array[Array[Int]] = Array(
      Array(4, 5, 6),
      Array(7, 9),
      Array(1, 2, 3)
    )

    expect.eql(expected, int3x3Grid().neighbours(1, 2))
  }

  test("neighbours of middle/top cell on 3x3 grid") {
    val expected: Array[Array[Int]] = Array(
      Array(7, 8, 9),
      Array(1, 3),
      Array(4, 5, 6)
    )

    expect.eql(expected, int3x3Grid().neighbours(1, 0))
  }

  test("neighbours of left/top cell on 3x3 grid") {
    val expected: Array[Array[Int]] = Array(
      Array(9, 7, 8),
      Array(3, 2),
      Array(6, 4, 5)
    )

    expect.eql(expected, int3x3Grid().neighbours(0, 0))
  }

  test("neighbours of right/bottom cell on 3x3 grid") {
    val expected: Array[Array[Int]] = Array(
      Array(5, 6, 4),
      Array(8, 7),
      Array(2, 3, 1)
    )

    expect.eql(expected, int3x3Grid().neighbours(2, 2))
  }

  test("neighbours of middle/middle cell on 5x5 grid") {
    val expected: Array[Array[Int]] = Array(
      Array(7, 8, 9),
      Array(12, 14),
      Array(17, 18, 19)
    )

    expect.eql(expected, int5x5Grid().neighbours(2, 2))
  }
}
