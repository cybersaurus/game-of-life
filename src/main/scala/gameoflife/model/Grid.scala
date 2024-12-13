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
    cellsWithIndex(cells) pipe Grid.apply

  def reduce(
      reduceCellsToRow: (A, A) => A,
      reduceRowsToResult: (A, A) => A
  ): A =
    cells
      .map(row => row.reduce(reduceCellsToRow))
      .reduce(reduceRowsToResult)

  def neighbours(x: Int, y: Int): Array[Array[A]] =
    // TODO: Fix slice() when x/y wrap off grid edge
    slice(left(x), right(x), upper(y), lower(y))
      .pipe(cellsWithIndex)
      .map(row => row.filterNot { case (cell, (cx, cy)) => (cx, cy) == (x, y) })
      .pipe(stripIndex)

  private def upper(y: Int): Int = Grid.upper(this)(y)
  private def lower(y: Int): Int = Grid.lower(this)(y)
  private def left(x: Int): Int = Grid.left(this)(x)
  private def right(x: Int): Int = Grid.right(this)(x)

  private[model] def slice(xFrom: Int, xTo: Int, yFrom: Int, yTo: Int): Array[Array[A]] =
    cells.slice(yFrom, yTo + 1).map(_.slice(xFrom, xTo + 1))

  private def cellsWithIndex(subcells: Array[Array[A]]): Array[Array[(A, (Int, Int))]] =
    subcells.zipWithIndex
      .map((row, y) => row.zipWithIndex.map { (cell, x) => (cell, x -> y) })

  private def stripIndex(cellsWithIndex: Array[Array[(A, (Int, Int))]]): Array[Array[A]] =
    cellsWithIndex.map(row => row.map((cell, _) => cell))
}

object Grid {

  def of[A: ClassTag](width: Int, height: Int, empty: => A) =
    Array
      .fill(height, width)(empty)
      .pipe(Grid.apply)

  private[model] def upper[A](grid: Grid[A])(y: Int): Int = if y > 0 then y - 1 else grid.cells.length - 1
  private[model] def lower[A](grid: Grid[A])(y: Int): Int = if y < grid.cells.length - 1 then y + 1 else 0
  private[model] def left[A](grid: Grid[A])(x: Int): Int = if x > 0 then x - 1 else grid.cells(0).length - 1
  private[model] def right[A](grid: Grid[A])(x: Int): Int = if x < grid.cells(0).length - 1 then x + 1 else 0

  import cats.syntax.show.*
  import cats.Eq
  import cats.Show

  import Arrays.given
  import Eqs.*

  given gridEq[A: Eq]: Eq[Grid[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  given gridShow[A: Show]: Show[Grid[A]] =
    Show.show(_.cells.show)
}
