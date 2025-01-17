package gameoflife.model.shape

import gameoflife.model.Arrays.*

import scala.reflect.ClassTag
import scala.util.chaining.*

case class Shape[A: ClassTag](cells: Array[Array[A]]) {
  val height: Int = cells.length
  val width: Int = cells(0).length

  def getCellAt(x: Int, y: Int): Option[A] = cells.getCellAt(x, y)

  def map[B: ClassTag](f: (A, (Int, Int)) => B): Shape[B] = cells.mapWithCoords(f).pipe(Shape.apply)
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
}
