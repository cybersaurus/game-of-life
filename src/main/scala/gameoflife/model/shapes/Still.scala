package gameoflife.model.shapes

import gameoflife.model.Grid
import gameoflife.model.State

object Still {
  val block: Grid[State] =
    Grid
      .fill(width = 2, height = 2, fill = State.Alive)

  val beehive: Grid[State] =
    Grid
      .of(width = 4, height = 3, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (0, 1) => State.Alive
        case (3, 1) => State.Alive
        case (1, 2) => State.Alive
        case (2, 2) => State.Alive
      }

  val loaf: Grid[State] =
    Grid
      .of(width = 4, height = 4, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (0, 1) => State.Alive
        case (3, 1) => State.Alive
        case (1, 2) => State.Alive
        case (3, 2) => State.Alive
        case (2, 3) => State.Alive
      }

  val boat: Grid[State] =
    Grid
      .of(width = 3, height = 3, fill = State.Empty) {
        case (0, 0) => State.Alive
        case (1, 0) => State.Alive
        case (0, 1) => State.Alive
        case (2, 1) => State.Alive
        case (1, 2) => State.Alive
      }

  val tub: Grid[State] =
    Grid
      .of(width = 3, height = 3, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (0, 1) => State.Alive
        case (2, 1) => State.Alive
        case (1, 2) => State.Alive
      }
}
