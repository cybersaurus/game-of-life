package gameoflife.model.shape

import scala.reflect.ClassTag
import scala.util.chaining.*

case class Shape[A](cells: Array[Array[A]]) {
  val height: Int = cells.length
  val width: Int = cells(0).length

  def getCellAt(x: Int, y: Int): Option[A] = {
    def isDefined(x: Int, y: Int): Boolean =
      x >= 0 && y >= 0 && cells.length > y && cells(0).length > x

    def cellAt(x: Int, y: Int): A = cells(y)(x)

    Option.when(isDefined(x, y))(cellAt(x, y))
  }

  def map[B: ClassTag](f: (A, (Int, Int)) => B): Shape[B] =
    zipWithIndex.cells
      .map(row => row.map(f.tupled))
      .pipe(Shape.apply)

  private[shape] def zipWithIndex: Shape[(A, (Int, Int))] =
    Shape
      .withOffsetIndex(xOffset = 0, yOffset = 0)(cells)
      .pipe(Shape.apply)
}
object Shape {
  def fill[A: ClassTag](width: Int, height: Int, fill: => A) =
    Shape.of(width, height, fill)(PartialFunction.empty)

  def of[A: ClassTag](width: Int, height: Int, fill: => A)(insertAtCoords: PartialFunction[(Int, Int), A]) =
    Array
      .tabulate(height, width)((y, x) => insertAtCoords.applyOrElse((x, y), (_, _) => fill))
      .pipe(Shape.apply)

  def combine[A: ClassTag](lShape: Shape[A], rShape: Shape[A], default: A, atX: Int = 0, atY: Int = 0): Shape[A] =
    Shape.of(
      width = math.max(lShape.width, rShape.width),
      height = math.max(lShape.height, rShape.height),
      default
    ) { (x, y) =>
      (lShape.getCellAt(x, y), rShape.getCellAt(x - atX, y - atY)) match {
        case (None, Some(rightCell))           => rightCell
        case (Some(leftCell), None)            => leftCell
        case (Some(leftCell), Some(rightCell)) => if rightCell == default then leftCell else rightCell
        case (None, None)                      => default
      }
    }

  private def withOffsetIndex[A](xOffset: Int, yOffset: Int)(
      subcells: Array[Array[A]]
  ): Array[Array[(A, (Int, Int))]] =
    subcells.zipWithIndex
      .map((row, y) => row.zipWithIndex.map { (cell, x) => (cell, (x + xOffset) -> (y + yOffset)) })
}
