package gameoflife.model.shape

object ShapeSpec extends weaver.FunSuite with ShapeFixtures {

  test("getCellAt returns Some(1)") {
    expect.eql(Some(1), increasingInts3x4Shape.getCellAt(0, 0))
  }

  test("getCellAt returns Some(8)") {
    expect.eql(Some(8), increasingInts3x4Shape.getCellAt(1, 2))
  }

  test("getCellAt returns None") {
    expect.eql(None, increasingInts3x4Shape.getCellAt(100, 100))
  }

  test("map applies supplied function to all elements") {
    val expectedShape: Shape[Int] =
      Shape.of(increasingInts3x4Shape.width, increasingInts3x4Shape.height, fill = 0)(
        increasingInts(increasingInts3x4Shape.width) andThen (_ * 2)
      )

    expect.eql(expectedShape, increasingInts3x4Shape.map { case (i, (_, _)) => i * 2 })
  }

  test("combine empty shapes") {
    val empty2x5: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
    val empty4x3: Shape[Int] = Shape.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)

    val expected: Shape[Int] = Shape.of(width = 4, height = 5, fill = 0)(PartialFunction.empty)

    expect.eql(expected, Shape.combine(empty2x5, empty4x3, default = 0)) &&
    expect.eql(expected, Shape.combine(empty4x3, empty2x5, default = 0))
  }

  test("flipHorizontally") {
    val expectedShape: Shape[Int] =
      Shape.of(increasingInts3x4Shape.width, increasingInts3x4Shape.height, fill = 0)((x, y) =>
        (increasingInts3x4Shape.width - x) + (y * increasingInts3x4Shape.width)
      )

    expect.eql(expectedShape, increasingInts3x4Shape.flipHorizontally)
  }

  test("flipHorizontally twice") {
    expect.eql(increasingInts3x4Shape, increasingInts3x4Shape.flipHorizontally.flipHorizontally)
  }

  test("flipVertically") {
    val expectedShape: Shape[Int] =
      Shape.of(increasingInts3x4Shape.width, increasingInts3x4Shape.height, fill = 0)((x, y) =>
        (x + 1) + ((increasingInts3x4Shape.width - y) * increasingInts3x4Shape.width)
      )

    expect.eql(expectedShape, increasingInts3x4Shape.flipVertically)
  }

  test("flipVertically twice") {
    expect.eql(increasingInts3x4Shape, increasingInts3x4Shape.flipVertically.flipVertically)
  }
}
