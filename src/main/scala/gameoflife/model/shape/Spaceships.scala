package gameoflife.model.shape

import gameoflife.model.State

object Spaceships {
  val glider: Shape[State] =
    Shape
      .of(width = 3, height = 3, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (2, 1) => State.Alive
        case (2, 2) => State.Alive
        case (0, 1) => State.Alive
      }

  val lightweight: Shape[State] =
    Shape
      .of(width = 5, height = 4, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (3, 0) => State.Alive
        case (4, 0) => State.Alive
        case (0, 1) => State.Alive
        case (4, 1) => State.Alive
        case (4, 2) => State.Alive
        case (0, 3) => State.Alive
        case (3, 3) => State.Alive
      }

  val middleweight: Shape[State] =
    Shape
      .of(width = 6, height = 5, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (3, 0) => State.Alive
        case (4, 0) => State.Alive
        case (5, 0) => State.Alive
        case (0, 1) => State.Alive
        case (5, 1) => State.Alive
        case (5, 2) => State.Alive
        case (0, 3) => State.Alive
        case (4, 3) => State.Alive
        case (2, 4) => State.Alive
      }

  val heavyweight: Shape[State] =
    Shape
      .of(width = 7, height = 4, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (3, 0) => State.Alive
        case (4, 0) => State.Alive
        case (5, 0) => State.Alive
        case (6, 0) => State.Alive
        case (0, 1) => State.Alive
        case (6, 1) => State.Alive
        case (6, 2) => State.Alive
        case (5, 3) => State.Alive
      }
}
