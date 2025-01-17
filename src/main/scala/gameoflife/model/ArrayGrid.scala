package gameoflife.model

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
final case class ArrayGrid[A: ClassTag] private (cells: Array[Array[A]]) extends Grid[A] {

  override val height: Int = cells.length
  override val width: Int = cells(0).length

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

  override def map[B: ClassTag](f: (A, (Int, Int)) => B): Grid[B] =
    cells
      .mapWithCoords(f)
      .pipe(ArrayGrid.apply)

  override def reduce(
      reduceCellsToRow: (A, A) => A,
      reduceRowsToResult: (A, A) => A
  ): A =
    cells
      .map(row => row.reduce(reduceCellsToRow))
      .reduce(reduceRowsToResult)

  override def neighboursCount(x: Int, y: Int)(pred: A => Boolean): Int = neighbours(x, y).flatten.count(pred)

  private[model] def neighbours(x: Int, y: Int): Array[Array[A]] = {
    def rowsAround(y: Int): Array[Array[A]] =
      y match {
        case y if y == 0          => Array(cells(height - 1), cells(y), cells(y + 1))
        case y if y == height - 1 => Array(cells(y - 1), cells(y), cells(0))
        case y                    => cells.slice(y - 1, y + 2)
      }

    def columnsAround(x: Int)(rows: Array[Array[A]]): Array[Array[A]] =
      x match {
        case x if x == 0         => rows.map(row => Array(row(width - 1), row(x), row(x + 1)))
        case x if x == width - 1 => rows.map(row => Array(row(x - 1), row(x), row(0)))
        case x                   => rows.map(_.slice(x - 1, x + 2))
      }

    def collectCellsExceptMiddle(row: Array[(A, (Int, Int))]): Array[A] =
      row.collect { case (cell, (cx, cy)) if (cx, cy) != (1, 1) => cell }

    (rowsAround andThen columnsAround(x))(y)
      .pipe(_.zipWithCoords())
      .map(collectCellsExceptMiddle)
  }

  private[model] def slice(xFrom: Int, xTo: Int, yFrom: Int, yTo: Int): Array[Array[A]] =
    cells.slice(yFrom, yTo + 1).map(_.slice(xFrom, xTo + 1))
}

object ArrayGrid {
  import cats.syntax.show.toShow
  import cats.Show

  import Arrays.given

  given [A: Show]: Show[ArrayGrid[A]] = Show.show(_.cells.show)

  def fill[A: ClassTag](width: Int, height: Int, fill: => A) =
    ArrayGrid.of(width, height, fill)(PartialFunction.empty)

  def of[A: ClassTag](width: Int, height: Int, fill: => A)(insertAtCoords: PartialFunction[(Int, Int), A]) =
    Array
      .tabulate(height, width)((y, x) => insertAtCoords.applyOrElse((x, y), (_, _) => fill))
      .pipe(ArrayGrid.apply)

  extension [A: Show](grid: ArrayGrid[A])
    def debug(prefix: String): ArrayGrid[A] = grid.tap(_ => println(s"$prefix: [${grid.show}]"))
}
