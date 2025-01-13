package gameoflife.model

import cats.Eq
import gameoflife.model.shapes.Oscillators

trait GridFixtures {
  given arrayEq[A: Eq]: Eq[Array[A]] = Arrays.arrayEq

  protected final def emptyInt3x4Grid(
      insertAtCoords: PartialFunction[(Int, Int), Int] = PartialFunction.empty
  ): Grid[Int] =
    Grid.of(width = 3, height = 4, fill = 0)(insertAtCoords)

  protected final def int3x3Grid(): Grid[Int] = increasingGrid(width = 3, height = 3)
  protected final def int3x4Grid(): Grid[Int] = increasingGrid(width = 3, height = 4)
  protected final def int5x5Grid(): Grid[Int] = increasingGrid(width = 5, height = 5)

  protected final def increasingGrid(width: Int, height: Int, inc: Int = 1, offset: Int = 0): Grid[Int] =
    Grid.of(width, height, fill = 0) { (x, y) =>
      (((x + 1) + (y * width)) * inc) + offset
    }

  val hBlinker5x5: Grid[State] =
    Grid
      .fill(width = 5, height = 5, fill = State.Empty)
      .combine(Oscillators.blinker, default = State.Empty, atX = 1, atY = 1)

  val vBlinker5x5: Grid[State] =
    Grid
      .of(width = 5, height = 5, fill = State.Empty) {
        case (2, 1) => State.Alive
        case (2, 2) => State.Alive
        case (2, 3) => State.Alive
      }
}
