package gameoflife.model

import cats.syntax.eq.*
import cats.Eq
import gameoflife.model.shape.Oscillators

import Eqs.*

trait ArrayGridFixtures {
  given [A: Eq]: Eq[Array[A]] =
    Eq.and(
      Eq.by(_.length),
      Eq.instance((arr1, arr2) => arr1.corresponds(arr2.iterator)(_ === _))
    )

  protected given [A: Eq]: Eq[ArrayGrid[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  protected given [A: Eq]: Eq[Grid[A]] = Eq.by(_.asInstanceOf[ArrayGrid[A]])

  protected final def emptyInt3x4Grid(
      insertAtCoords: PartialFunction[(Int, Int), Int] = PartialFunction.empty
  ): ArrayGrid[Int] =
    ArrayGrid.of(width = 3, height = 4, fill = 0)(insertAtCoords)

  protected final def int3x3Grid(): ArrayGrid[Int] = increasingGrid(width = 3, height = 3)
  protected final def int3x4Grid(): ArrayGrid[Int] = increasingGrid(width = 3, height = 4)
  protected final def int5x5Grid(): ArrayGrid[Int] = increasingGrid(width = 5, height = 5)

  protected final def increasingGrid(width: Int, height: Int, inc: Int = 1, offset: Int = 0): ArrayGrid[Int] =
    ArrayGrid.of(width, height, fill = 0) { (x, y) =>
      (((x + 1) + (y * width)) * inc) + offset
    }

  val hBlinker5x5: ArrayGrid[State] =
    ArrayGrid
      .fill(width = 5, height = 5, fill = State.Empty)
      .add(Oscillators.blinker, default = State.Empty, atX = 1, atY = 1)

  val vBlinker5x5: ArrayGrid[State] =
    ArrayGrid
      .of(width = 5, height = 5, fill = State.Empty) {
        case (2, 1) => State.Alive
        case (2, 2) => State.Alive
        case (2, 3) => State.Alive
      }
}
