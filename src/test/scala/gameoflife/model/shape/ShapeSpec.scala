package gameoflife.model.shape

object ShapeSpec extends weaver.FunSuite with ShapeFixtures {

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

  test("combine empty shapes") {
    val empty2x5: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
    val empty4x3: Shape[Int] = Shape.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)

    val expected: Shape[Int] = Shape.of(width = 4, height = 5, fill = 0)(PartialFunction.empty)

    expect.eql(expected, Shape.combine(empty2x5, empty4x3, default = 0)) &&
    expect.eql(expected, Shape.combine(empty4x3, empty2x5, default = 0))
  }
}
