package gameoflife.model.shape

import gameoflife.model.State

object Still {
  val block: Shape[State] =
    Shape
      .fill(width = 2, height = 2, fill = State.Alive)

  val beehive: Shape[State] =
    Shape
      .of(width = 4, height = 3, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (0, 1) => State.Alive
        case (3, 1) => State.Alive
        case (1, 2) => State.Alive
        case (2, 2) => State.Alive
      }

  val loaf: Shape[State] =
    Shape
      .of(width = 4, height = 4, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (0, 1) => State.Alive
        case (3, 1) => State.Alive
        case (1, 2) => State.Alive
        case (3, 2) => State.Alive
        case (2, 3) => State.Alive
      }

  val boat: Shape[State] =
    Shape
      .of(width = 3, height = 3, fill = State.Empty) {
        case (0, 0) => State.Alive
        case (1, 0) => State.Alive
        case (0, 1) => State.Alive
        case (2, 1) => State.Alive
        case (1, 2) => State.Alive
      }

  val tub: Shape[State] =
    Shape
      .of(width = 3, height = 3, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (0, 1) => State.Alive
        case (2, 1) => State.Alive
        case (1, 2) => State.Alive
      }
}
