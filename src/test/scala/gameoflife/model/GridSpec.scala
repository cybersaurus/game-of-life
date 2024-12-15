package gameoflife.model

import cats.syntax.show.*

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

  test("neighbours") {
    val expected: Array[Array[Int]] = Array(
      Array(1, 2, 3),
      Array(4, 6),
      Array(7, 8, 9)
    )

    expect.eql(expected, int3x4Grid().neighbours(1, 1))
  }

  test("bigger neighbours") {
    val grid = int5x5Grid()

    val neighbours = grid.neighbours(1, 2)

    val slice: Array[Array[Int]] = grid.slice(xFrom = 0, xTo = 2, yFrom = 1, yTo = 3)

    val slicecWithIndex: Array[Array[(Int, (Int, Int))]] = Grid.withOffsetIndex(0, 1)(slice)

    val expected: Array[Array[Int]] = Array(
      Array(6, 7, 8),
      Array(11, 13),
      Array(16, 17, 18)
    )

    expect.eql(expected, neighbours)
  }

  List[(String, Grid[Int] => Int => Int, List[(Int, Int)])](
    ("upper", Grid.upper, List(0 -> 3, 1 -> 0, 2 -> 1, 3 -> 2)),
    ("lower", Grid.lower, List(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)),
    ("left", Grid.left, List(0 -> 2, 1 -> 0, 2 -> 1)),
    ("right", Grid.right, List(0 -> 1, 1 -> 2, 2 -> 0))
  ).foreach((opName, op, scenarios) =>
    scenarios.foreach((x, expected) =>
      test(s"$opName of $x should be $expected") {
        expect.same(expected, op(emptyGrid)(x))
      }
    )
  )
}
