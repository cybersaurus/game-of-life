package gameoflife.model

import cats.Eq
import gameoflife.model.shape.Shape

protected abstract class GridSpec extends weaver.FunSuite {

  protected def gridEquals[A: Eq]: Eq[Grid[A]]

  protected given [A: Eq]: Eq[Grid[A]] = gridEquals

  protected val emptyGrid2x5: Grid[Int]
  protected val emptyGrid4x3: Grid[Int]
  protected val emptyGrid4x5: Grid[Int]
  protected val emptyGrid10x10: Grid[Int]

  protected val int3x3Grid: Grid[Int]
  protected val int3x4Grid: Grid[Int]
  protected val int5x5Grid: Grid[Int]

  protected def increasingGrid(width: Int, height: Int, inc: Int = 1, offset: Int = 0): Grid[Int]

  protected def makeGrid(width: Int, height: Int, default: Int, cells: Map[(Int, Int), Int]): Grid[Int]

  test("getCellAt returns Some(12)") {
    expect.eql(Some(12), int3x4Grid.getCellAt(2, 3))
  }

  test("getCellAt returns None for invalid coords") {
    expect.eql(None, int3x4Grid.getCellAt(100, 100))
  }

  test("map applies supplied function to all elements") {
    // TODO: Remove?
    import Empty.given_Empty_Int

    val expectedGrid: Grid[Int] = increasingGrid(width = 3, height = 4, inc = 2)

    expect.eql(expectedGrid, int3x4Grid.map { case (i, (_, _)) => i * 2 })
  }

  test("neighboursCount") {
    expect.eql(4, int3x4Grid.neighboursCount(1, 1)(cell => cell % 2 == 0))
  }

  test("neighbours of middle/middle cell on 3x3 grid") {
    val expected: Map[(Int, Int), Option[Int]] = Map(
      // format: off
      (0,0) -> 1, (1,0) -> 2, (2,0) -> 3,
      (0,1) -> 4,             (2,1) -> 6,
      (0,2) -> 7, (1,2) -> 8, (2,2) -> 9
      // format: on
    ).view.mapValues(Some(_)).toMap

    expect.eql(expected, int3x3Grid.neighbours(1, 1))
  }

  test("neighbours of middle/bottom cell on 3x3 grid") {
    val expected: Map[(Int, Int), Option[Int]] = Map(
      // format: off
      (0, 1) -> 4, (1, 1) -> 5, (2, 1) -> 6,
      (0, 2) -> 7,              (2, 2) -> 9,
      (0, 0) -> 1, (1, 0) -> 2, (2, 0) -> 3
      // format: on
    ).view.mapValues(Some(_)).toMap

    expect.eql(expected, int3x3Grid.neighbours(1, 2))
  }

  test("neighbours of middle/top cell on 3x3 grid") {
    val expected: Map[(Int, Int), Option[Int]] = Map(
      // format: off
      (0, 2) -> 7, (1, 2) -> 8, (2, 2) -> 9,
      (0, 0) -> 1,              (2, 0) -> 3,
      (0, 1) -> 4, (1, 1) -> 5, (2, 1) -> 6
      // format: on
    ).view.mapValues(Some(_)).toMap

    expect.eql(expected, int3x3Grid.neighbours(1, 0))
  }

  test("neighbours of left/top cell on 3x3 grid") {
    val expected: Map[(Int, Int), Option[Int]] = Map(
      // format: off
      (2, 2) -> 9, (0, 2) -> 7, (1, 2) -> 8,
      (2, 0) -> 3,              (1, 0) -> 2,
      (2, 1) -> 6, (0, 1) -> 4, (1, 1) -> 5,
      // format: on
    ).view.mapValues(Some(_)).toMap

    expect.eql(expected, int3x3Grid.neighbours(0, 0))
  }

  test("neighbours of right/bottom cell on 3x3 grid") {
    val expected: Map[(Int, Int), Option[Int]] = Map(
      // format: off
      (1, 1) -> 5, (2, 1) -> 6, (0, 1) -> 4,
      (1, 2) -> 8,              (0, 2) -> 7,
      (1, 0) -> 2, (2, 0) -> 3, (0, 0) -> 1,
      // format: on
    ).view.mapValues(Some(_)).toMap

    expect.eql(expected, int3x3Grid.neighbours(2, 2))
  }

  test("neighbours of middle/middle cell on 5x5 grid") {
    val expected: Map[(Int, Int), Option[Int]] = Map(
      // format: off
      (1, 1) ->  7, (2, 1) ->  8, (3, 1) ->  9,
      (1, 2) -> 12,               (3, 2) -> 14,
      (1, 3) -> 17, (2, 3) -> 18, (3, 3) -> 19,
      // format: on
    ).view.mapValues(Some(_)).toMap

    expect.eql(expected, int5x5Grid.neighbours(2, 2))
  }

  test("add empty shape creates a bigger grid") {
    val emptyShape2x5: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
    val emptyShape4x3: Shape[Int] = Shape.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)

    expect.eql(emptyGrid4x5, emptyGrid2x5.add(emptyShape4x3, default = 0)) &&
    expect.eql(emptyGrid4x5, emptyGrid4x3.add(emptyShape2x5, default = 0))
  }

  test("add shape to empty grid") {
    val shape: Shape[Int] = Shape.of(width = 5, height = 5, fill = 0) {
      case (1, 2) => 10
      case (2, 2) => 20
      case (3, 2) => 30
    }
    val expectedCells: Map[(Int, Int), Int] = Map(
      (1, 2) -> 10,
      (2, 2) -> 20,
      (3, 2) -> 30
    )
    val expected: Grid[Int] = makeGrid(width = 10, height = 10, default = 0, expectedCells)

    expect.eql(expected, emptyGrid10x10.add(shape, default = 0))
  }

  test("add shape with offset to empty grid") {
    val shape: Shape[Int] = Shape.of(width = 3, height = 3, fill = 0) {
      case (0, 1) => 10
      case (1, 1) => 20
      case (2, 1) => 30
    }
    val expectedCells: Map[(Int, Int), Int] = Map(
      (4, 6) -> 10,
      (5, 6) -> 20,
      (6, 6) -> 30
    )
    val expected: Grid[Int] = makeGrid(width = 10, height = 10, default = 0, expectedCells)

    expect.eql(expected, emptyGrid10x10.add(shape, default = 0, atX = 4, atY = 5))
  }

  List[(String, Int => Int, List[(Int, Int)])](
    ("upper", int3x4Grid.upper _, List(0 -> 3, 1 -> 0, 2 -> 1, 3 -> 2)),
    ("lower", int3x4Grid.lower _, List(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)),
    ("left", int3x4Grid.left _, List(0 -> 2, 1 -> 0, 2 -> 1)),
    ("right", int3x4Grid.right _, List(0 -> 1, 1 -> 2, 2 -> 0))
  ).foreach((opName, op, scenarios) =>
    scenarios.foreach((x, expected) =>
      test(s"$opName of $x should be $expected") {
        expect.same(expected, op(x))
      }
    )
  )
}
