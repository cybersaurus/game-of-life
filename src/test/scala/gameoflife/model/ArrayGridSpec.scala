package gameoflife.model

import gameoflife.model.shape.Shape

object ArrayGridSpec extends weaver.FunSuite with ArrayGridFixtures {

  test("getCellAt returns Some(123)") {
    expect.eql(Some(123), emptyInt3x4Grid { case (2, 3) => 123 }.getCellAt(2, 3))
  }

  test("getCellAt returns None for invalid coords") {
    expect.eql(None, emptyInt3x4Grid().getCellAt(100, 100))
  }

  test("map applies supplied function to all elements") {
    val expectedGrid: ArrayGrid[Int] = increasingGrid(width = 3, height = 4, inc = 2)

    expect.eql(expectedGrid, int3x4Grid().map { case (i, (_, _)) => i * 2 })
  }

  test("slice") {
    val expected: Array[Array[Int]] = Array(
      Array(5, 6),
      Array(8, 9)
    )

    expect.eql(expected, int3x4Grid().slice(xFrom = 1, xTo = 2, yFrom = 1, yTo = 2))
  }

  test("neighboursCount") {
    expect.eql(4, int3x4Grid().neighboursCount(1, 1)(cell => cell % 2 == 0))
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

  test("add empty shape") {
    val emptyGrid2x5: ArrayGrid[Int] = ArrayGrid.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
    val emptyGrid4x3: ArrayGrid[Int] = ArrayGrid.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)

    val emptyShape2x5: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
    val emptyShape4x3: Shape[Int] = Shape.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)

    val expected: ArrayGrid[Int] = ArrayGrid.of(width = 4, height = 5, fill = 0)(PartialFunction.empty)

    expect.eql(expected, emptyGrid2x5.add(emptyShape4x3, default = 0)) &&
    expect.eql(expected, emptyGrid4x3.add(emptyShape2x5, default = 0))
  }

  test("add shape to empty grid") {
    val empty10x10: ArrayGrid[Int] = ArrayGrid.of(width = 10, height = 10, fill = 0)(PartialFunction.empty)

    val shape: Shape[Int] = Shape.of(width = 5, height = 5, fill = 0) {
      case (1, 2) => 10
      case (2, 2) => 20
      case (3, 2) => 30
    }
    val expected = ArrayGrid.of(width = 10, height = 10, fill = 0) {
      case (1, 2) => 10
      case (2, 2) => 20
      case (3, 2) => 30
    }

    expect.eql(expected, empty10x10.add(shape, default = 0))
  }

  test("add shape with offset to empty grid") {
    val empty10x10: ArrayGrid[Int] = ArrayGrid.of(width = 10, height = 10, fill = 0)(PartialFunction.empty)

    val shape: Shape[Int] = Shape.of(width = 3, height = 3, fill = 0) {
      case (0, 1) => 10
      case (1, 1) => 20
      case (2, 1) => 30
    }
    val expected = ArrayGrid.of(width = 10, height = 10, fill = 0) {
      case (4, 6) => 10
      case (5, 6) => 20
      case (6, 6) => 30
    }

    expect.eql(expected, empty10x10.add(shape, default = 0, atX = 4, atY = 5))
  }
}
