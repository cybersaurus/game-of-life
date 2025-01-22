package gameoflife.model

import cats.syntax.show.toShow
import cats.Show
import gameoflife.model.shape.Shape

import scala.reflect.ClassTag
import scala.util.chaining.*

import Arrays.*

/** Simple implementation of Grid, backed by a two-dimensional Array.
  *
  * @param cells
  *   the initial underlying two-dimensional array of values
  * @tparam A
  *   the type of value stored in the ArrayGrid
  */
final case class ArrayGrid[A: ClassTag: Empty] private (cells: Array[Array[A]]) extends Grid[A] {
  import Arrays.given

  override val height: Int = cells.length
  override val width: Int = cells(0).length

  override given gridShow[A: Show]: Show[Grid[A]] = Show.show(_.asInstanceOf[ArrayGrid[A]].cells.show)

  override def add(shape: Shape[A], default: A, atX: Int = 0, atY: Int = 0): ArrayGrid[A] =
    ArrayGrid.of(
      width = math.max(width, shape.width),
      height = math.max(height, shape.height),
      default
    ) { (x, y) =>
      (this.getCellAt(x, y), shape.getCellAt(x - atX, y - atY)) match {
        case (None, Some(otherCell))       => otherCell
        case (Some(cell), None)            => cell
        case (Some(cell), Some(otherCell)) => if otherCell == default then cell else otherCell
        case (None, None)                  => default
      }
    }

  override def getCellAt(x: Int, y: Int): Option[A] = cells.getCellAt(x, y)

  override def map[B: ClassTag: Empty](f: (A, (Int, Int)) => B): Grid[B] =
    cells
      .mapWithCoords(f)
      .pipe(ArrayGrid.apply)
}

object ArrayGrid {
  def fill[A: ClassTag: Empty](width: Int, height: Int, fill: => A) =
    ArrayGrid.of(width, height, fill)(PartialFunction.empty)

  def of[A: ClassTag: Empty](width: Int, height: Int, fill: => A)(insertAtCoords: PartialFunction[(Int, Int), A]) =
    Array
      .tabulate(height, width)((y, x) => insertAtCoords.applyOrElse((x, y), (_, _) => fill))
      .pipe(ArrayGrid.apply)
}
