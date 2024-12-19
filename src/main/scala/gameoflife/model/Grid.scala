package gameoflife.model

import scala.reflect.ClassTag
import scala.util.chaining.*

final case class Grid[A: ClassTag] private (cells: Array[Array[A]]) {

  val height: Int = cells.length
  val width: Int = cells(0).length

  override def clone(): Grid[A] =
    cells.clone().map(row => row.clone()).pipe(Grid.apply)

  def getCellAt(x: Int, y: Int): Option[A] = scala.util.Try(cellAt(x, y)).toOption

  def cellAt(x: Int, y: Int): A = cells(y)(x)

  def setAt(x: Int, y: Int, cell: A): Grid[A] =
    this.tap(_ => cells(y).update(x, cell))

  def map[B: ClassTag](f: A => B): Grid[B] =
    cells
      .map(row => row.map(f))
      .pipe(Grid.apply)

  def zipWithIndex: Grid[(A, (Int, Int))] =
    Grid.withOffsetIndex(xOffset = 0, yOffset = 0)(cells).pipe(Grid.apply)

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

    (rowsAround andThen columnsAround(x))(y)
      .pipe(Grid.withOffsetIndex(0, 0))
      .map(row => row.filterNot { case (cell, (cx, cy)) => (cx, cy) == (1, 1) })
      .pipe(Grid.stripIndex)
  }

  private[model] def slice(xFrom: Int, xTo: Int, yFrom: Int, yTo: Int): Array[Array[A]] =
    cells.slice(yFrom, yTo + 1).map(_.slice(xFrom, xTo + 1))
}

object Grid {

  def of[A: ClassTag](width: Int, height: Int, empty: => A) =
    Array
      .fill(height, width)(empty)
      .pipe(Grid.apply)

  private[model] def withOffsetIndex[A](xOffset: Int, yOffset: Int)(
      subcells: Array[Array[A]]
  ): Array[Array[(A, (Int, Int))]] =
    subcells.zipWithIndex
      .map((row, y) => row.zipWithIndex.map { (cell, x) => (cell, (x + xOffset) -> (y + yOffset)) })

  private def stripIndex[A: ClassTag](cellsWithIndex: Array[Array[(A, (Int, Int))]]): Array[Array[A]] =
    cellsWithIndex.map(row => row.map((cell, _) => cell))

  import cats.syntax.show.*
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

  extension [A: Show](grid: Grid[A]) {
    def debug(prefix: String) = {
      println(s"$prefix: [${grid.show}]")
      grid
    }
  }
}
