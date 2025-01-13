package gameoflife.model.shapes.array

import gameoflife.model.ArrayGrid
import gameoflife.model.State

object Oscillators {
  val blinker: ArrayGrid[State] =
    ArrayGrid
      .of(width = 3, height = 3, fill = State.Empty) {
        case (0, 1) => State.Alive
        case (1, 1) => State.Alive
        case (2, 1) => State.Alive
      }

  val toad: ArrayGrid[State] =
    ArrayGrid
      .of(width = 4, height = 2, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (3, 0) => State.Alive
        case (0, 1) => State.Alive
        case (1, 1) => State.Alive
        case (2, 1) => State.Alive
      }

  val beacon: ArrayGrid[State] =
    ArrayGrid
      .fill(width = 4, height = 4, fill = State.Empty)
      .combine(Still.block, default = State.Empty)
      .combine(Still.block, default = State.Empty, atX = 2, atY = 2)
}
