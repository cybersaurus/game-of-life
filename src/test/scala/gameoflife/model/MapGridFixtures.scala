package gameoflife.model

import cats.Eq
import gameoflife.util.Eqs.*

trait MapGridFixtures { self: GridSpec =>

  protected given [A: Eq]: Eq[MapGrid[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  override final protected given gridEquals[A: Eq]: Eq[Grid[A]] = Eq.by(_.asInstanceOf[MapGrid[A]])

  override final protected val emptyGrid2x5: Grid[Int] =
    MapGrid.empty(width = 2, height = 5)
  override final protected val emptyGrid4x3: Grid[Int] =
    MapGrid.empty(width = 4, height = 3)
  override final protected val emptyGrid4x5: Grid[Int] =
    MapGrid.empty(width = 4, height = 5)
  override final protected val emptyGrid10x10: Grid[Int] =
    MapGrid.empty(width = 10, height = 10)

  override final protected val int3x3Grid: Grid[Int] = increasingGrid(width = 3, height = 3)
  override final protected val int3x4Grid: Grid[Int] = increasingGrid(width = 3, height = 4)
  override final protected val int5x5Grid: Grid[Int] = increasingGrid(width = 5, height = 5)

  override final protected def increasingGrid(width: Int, height: Int, inc: Int = 1, offset: Int = 0): Grid[Int] =
    MapGrid.of(width, height)(
      List
        .tabulate(height, width) { (y, x) => (x, y) -> ((((x + 1) + (y * width)) * inc) + offset) }
        .flatten
        .toMap
    )

  override final protected def makeGrid(width: Int, height: Int, default: Int, cells: Map[(Int, Int), Int]): Grid[Int] =
    MapGrid.of(width, height)(cells)
}
