package gameoflife.model.shape

import cats.syntax.show.toShow
import cats.Eq
import cats.Show
import gameoflife.model.Arrays.given
import gameoflife.model.Eqs.*

object ShapeSpec extends weaver.FunSuite {
  private given [A: Eq]: Eq[Shape[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  private given [A: Show]: Show[Shape[A]] = Show.show(_.cells.show)

  private def increasingInts(width: Int): PartialFunction[(Int, Int), Int] = (x, y) => (x + 1) + (y * width)

  private val increasingIntsShape: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(increasingInts(width = 2))

  test("getCellAt returns Some(1)") {
    expect.eql(Some(1), increasingIntsShape.getCellAt(0, 0))
  }

  test("getCellAt returns Some(6)") {
    expect.eql(Some(6), increasingIntsShape.getCellAt(1, 2))
  }

  test("getCellAt returns None") {
    expect.eql(None, increasingIntsShape.getCellAt(100, 100))
  }

  test("map applies supplied function to all elements") {
    val expectedShape: Shape[Int] =
      Shape.of(width = 2, height = 5, fill = 0)((x, y) => increasingInts(width = 2)(x, y) * 2)

    expect.eql(expectedShape, increasingIntsShape.map { case (i, (_, _)) => i * 2 })
  }

  test("zipWithIndex adds coordinates to all elements") {
    val expectedShape: Shape[(Int, (Int, Int))] =
      Shape.of(width = 2, height = 5, fill = (0, -1 -> -1)) {
        case (0, 0) => (1, 0 -> 0)
        case (1, 0) => (2, 1 -> 0)
        case (0, 1) => (3, 0 -> 1)
        case (1, 1) => (4, 1 -> 1)
        case (0, 2) => (5, 0 -> 2)
        case (1, 2) => (6, 1 -> 2)
        case (0, 3) => (7, 0 -> 3)
        case (1, 3) => (8, 1 -> 3)
        case (0, 4) => (9, 0 -> 4)
        case (1, 4) => (10, 1 -> 4)
      }

    expect.eql(expectedShape, increasingIntsShape.zipWithIndex)
  }

  test("combine empty shapes") {
    val empty2x5: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
    val empty4x3: Shape[Int] = Shape.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)

    val expected: Shape[Int] = Shape.of(width = 4, height = 5, fill = 0)(PartialFunction.empty)

    expect.eql(expected, Shape.combine(empty2x5, empty4x3, default = 0)) &&
    expect.eql(expected, Shape.combine(empty4x3, empty2x5, default = 0))
  }
}
