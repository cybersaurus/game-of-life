package gameoflife.model

import Arrays.given

object GridSpec extends weaver.FunSuite {
  private val emptyGrid: Grid[Int] = emptyIntGrid()
  private def emptyIntGrid(): Grid[Int] = Grid.of(width = 3, height = 4, empty = 0)

  private def intGrid(): Grid[Int] = emptyIntGrid()
    .setAt(x = 0, y = 0, cell = 1)
    .setAt(x = 1, y = 0, cell = 2)
    .setAt(x = 2, y = 0, cell = 3)
    .setAt(x = 0, y = 1, cell = 4)
    .setAt(x = 1, y = 1, cell = 5)
    .setAt(x = 2, y = 1, cell = 6)
    .setAt(x = 0, y = 2, cell = 7)
    .setAt(x = 1, y = 2, cell = 8)
    .setAt(x = 2, y = 2, cell = 9)
    .setAt(x = 0, y = 3, cell = 10)
    .setAt(x = 1, y = 3, cell = 11)
    .setAt(x = 2, y = 3, cell = 12)

  test("cellAt returns 123") {
    val grid = intGrid() /*.clone()*/.setAt(2, 3, 123)

    expect.eql(123, grid.cellAt(2, 3))
  }

  test("getCellAt returns Some(123)") {
    val grid = intGrid() /*.clone()*/.setAt(2, 3, 123)

    expect.eql(Some(123), grid.getCellAt(2, 3))
  }

  test("getCellAt returns None for invalid coords") {
    expect.eql(None, intGrid().getCellAt(100, 100))
  }

  test("map applies supplied function to all elements") {
    val doubledGrid: Grid[Int] = intGrid() /*.clone()*/.map(_ * 2)

    val expectedGrid: Grid[Int] = emptyIntGrid()
      .setAt(x = 0, y = 0, cell = 2)
      .setAt(x = 1, y = 0, cell = 4)
      .setAt(x = 2, y = 0, cell = 6)
      .setAt(x = 0, y = 1, cell = 8)
      .setAt(x = 1, y = 1, cell = 10)
      .setAt(x = 2, y = 1, cell = 12)
      .setAt(x = 0, y = 2, cell = 14)
      .setAt(x = 1, y = 2, cell = 16)
      .setAt(x = 2, y = 2, cell = 18)
      .setAt(x = 0, y = 3, cell = 20)
      .setAt(x = 1, y = 3, cell = 22)
      .setAt(x = 2, y = 3, cell = 24)

    expect.eql(expectedGrid, doubledGrid)
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

    expect.eql(expectedGrid, intGrid().zipWithIndex)
  }

  test("slice") {
    val sliced: Array[Array[Int]] = intGrid().slice(xFrom = 1, xTo = 2, yFrom = 1, yTo = 2)

    val expected: Array[Array[Int]] = Array(
      Array(5, 6),
      Array(8, 9)
    )

    expect.same(expected, sliced)
  }

  test("neighbours") {
    val neighbours: Array[Array[Int]] = intGrid().neighbours(1, 1)

    val expected: Array[Array[Int]] = Array(
      Array(1, 2, 3),
      Array(4, 6),
      Array(7, 8, 9)
    )

    expect.same(expected, neighbours)
  }

  List[(Int, Int)](
    0 -> 3,
    1 -> 0,
    2 -> 1,
    3 -> 2
  ).foreach((x, expected) =>
    test(s"upper of $x should be $expected") {
      expect.same(expected, Grid.upper(emptyGrid)(x))
    }
  )

  List[(Int, Int)](
    0 -> 1,
    1 -> 2,
    2 -> 3,
    3 -> 0
  ).foreach((x, expected) =>
    test(s"lower of $x should be $expected") {
      expect.same(expected, Grid.lower(emptyGrid)(x))
    }
  )

  List[(Int, Int)](
    0 -> 2,
    1 -> 0,
    2 -> 1
  ).foreach((x, expected) =>
    test(s"left of $x should be $expected") {
      expect.same(expected, Grid.left(emptyGrid)(x))
    }
  )

  List[(Int, Int)](
    0 -> 1,
    1 -> 2,
    2 -> 0
  ).foreach((x, expected) =>
    test(s"right of $x should be $expected") {
      expect.same(expected, Grid.right(emptyGrid)(x))
    }
  )
}
