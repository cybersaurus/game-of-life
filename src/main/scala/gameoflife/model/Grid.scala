package gameoflife.model

import scala.reflect.ClassTag
import scala.util.chaining.*

final case class Grid[A: ClassTag] private (cells: Array[Array[A]]) {

  val height: Int = cells.length
  val width: Int = cells(0).length

  override def clone(): Grid[A] = cells.clone().map(row => row.clone()).pipe(Grid.apply)

  def combine(otherGrid: Grid[A], default: A, atX: Int = 0, atY: Int = 0): Grid[A] =
    Grid.of(
      width = math.max(width, otherGrid.width),
      height = math.max(height, otherGrid.height),
      default
    ) { (x, y) =>
      (this.getCellAt(x, y), otherGrid.getCellAt(x - atX, y - atY)) match {
        case (None, Some(otherCell))       => otherCell
        case (Some(cell), None)            => cell
        case (Some(cell), Some(otherCell)) => if otherCell == default then cell else otherCell
        case (None, None)                  => default
      }
    }

  def getCellAt(x: Int, y: Int): Option[A] = Option.when(isDefined(x, y))(cellAt(x, y))

  private def cellAt(x: Int, y: Int): A = cells(y)(x)

  private def isDefined(x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && cells.length > y && cells(0).length > x

  def map[B: ClassTag](f: A => B): Grid[B] =
    cells
      .map(row => row.map(f))
      .pipe(Grid.apply)

  def zipWithIndex: Grid[(A, (Int, Int))] =
    Grid
      .withOffsetIndex(xOffset = 0, yOffset = 0)(cells)
      .pipe(Grid.apply)

  def reduce(
      reduceCellsToRow: (A, A) => A,
      reduceRowsToResult: (A, A) => A
  ): A =
    cells
      .map(row => row.reduce(reduceCellsToRow))
      .reduce(reduceRowsToResult)

  def neighbours(x: Int, y: Int): Array[Array[A]] = {
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
      .pipe(Grid.withOffsetIndex(0, 0))
      .map(collectCellsExceptMiddle)
  }

  private[model] def slice(xFrom: Int, xTo: Int, yFrom: Int, yTo: Int): Array[Array[A]] =
    cells.slice(yFrom, yTo + 1).map(_.slice(xFrom, xTo + 1))
}

object Grid {

  def fill[A: ClassTag](width: Int, height: Int, fill: => A) =
    Grid.of(width, height, fill)(PartialFunction.empty)

  def of[A: ClassTag](width: Int, height: Int, fill: => A)(insertAtCoords: PartialFunction[(Int, Int), A]) =
    Array
      .tabulate(height, width)((y, x) => insertAtCoords.applyOrElse((x, y), (_, _) => fill))
      .pipe(Grid.apply)

  private[model] def withOffsetIndex[A](xOffset: Int, yOffset: Int)(
      subcells: Array[Array[A]]
  ): Array[Array[(A, (Int, Int))]] =
    subcells.zipWithIndex
      .map((row, y) => row.zipWithIndex.map { (cell, x) => (cell, (x + xOffset) -> (y + yOffset)) })

  import cats.syntax.show.toShow
  import cats.Eq
  import cats.Show

  import Arrays.given
  import Eqs.*

  given [A: Eq]: Eq[Grid[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  given [A: Show]: Show[Grid[A]] =
    Show.show(_.cells.show)

  extension [A: Show](grid: Grid[A])
    def debug(prefix: String): Grid[A] = grid.tap(_ => println(s"$prefix: [${grid.show}]"))
}
