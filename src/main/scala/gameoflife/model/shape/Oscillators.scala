package gameoflife.model.shape

import gameoflife.model.State

import scala.util.chaining.*

object Oscillators {
  val blinker: Shape[State] =
    Shape
      .of(width = 3, height = 3, fill = State.Empty) {
        case (0, 1) => State.Alive
        case (1, 1) => State.Alive
        case (2, 1) => State.Alive
      }

  val toad: Shape[State] =
    Shape
      .of(width = 4, height = 2, fill = State.Empty) {
        case (1, 0) => State.Alive
        case (2, 0) => State.Alive
        case (3, 0) => State.Alive
        case (0, 1) => State.Alive
        case (1, 1) => State.Alive
        case (2, 1) => State.Alive
      }

  val beacon: Shape[State] =
    Shape
      .fill(width = 4, height = 4, fill = State.Empty)
      .pipe(shape => Shape.combine(shape, Still.block, default = State.Empty))
      .pipe(shape => Shape.combine(shape, Still.block, default = State.Empty, atX = 2, atY = 2))
}
