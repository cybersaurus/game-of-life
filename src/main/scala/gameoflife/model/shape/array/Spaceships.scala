package gameoflife.model.shape.array

import gameoflife.model.shape.Shape
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
}
