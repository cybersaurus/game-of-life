package gameoflife.model

import cats.Eq

import Empty.given_Empty_Int
import Eqs.*

trait ArrayGridFixtures { self: GridSpec =>
  protected given arrayEq[A: Eq]: Eq[Array[A]] = ArraysFixtures.arrayEq

  protected given arrayGridEq[A: Eq]: Eq[ArrayGrid[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  override final protected def gridEquals[A: Eq]: Eq[Grid[A]] = Eq.by(_.asInstanceOf[ArrayGrid[A]])

  override final protected val emptyGrid2x5: Grid[Int] =
    ArrayGrid.of(width = 2, height = 5, fill = 0)(PartialFunction.empty)
  override final protected val emptyGrid4x3: Grid[Int] =
    ArrayGrid.of(width = 4, height = 3, fill = 0)(PartialFunction.empty)
  override final protected val emptyGrid4x5: Grid[Int] =
    ArrayGrid.of(width = 4, height = 5, fill = 0)(PartialFunction.empty)
  override final protected val emptyGrid10x10: Grid[Int] =
    ArrayGrid.of(width = 10, height = 10, fill = 0)(PartialFunction.empty)

  override final protected val int3x3Grid: Grid[Int] = increasingGrid(width = 3, height = 3)
  override final protected val int3x4Grid: Grid[Int] = increasingGrid(width = 3, height = 4)
  override final protected val int5x5Grid: Grid[Int] = increasingGrid(width = 5, height = 5)

  override final protected def increasingGrid(width: Int, height: Int, inc: Int = 1, offset: Int = 0): Grid[Int] =
    ArrayGrid.of(width, height, fill = 0) { (x, y) =>
      (((x + 1) + (y * width)) * inc) + offset
    }

  override final protected def makeGrid(width: Int, height: Int, default: Int, cells: Map[(Int, Int), Int]): Grid[Int] =
    ArrayGrid.of(width, height, default) { case (x, y) =>
      cells.getOrElse(x -> y, default)
    }
}
object ArrayGridFixtures {
  given arrayEq[A: Eq]: Eq[Array[A]] = ArraysFixtures.arrayEq

  given [A: Eq]: Eq[ArrayGrid[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )
}
