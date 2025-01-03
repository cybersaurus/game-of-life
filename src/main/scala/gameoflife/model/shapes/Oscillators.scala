package gameoflife.model.shapes

import gameoflife.model.Cell
import gameoflife.model.Grid

object Oscillators {
  val blinker: Grid[Cell] =
    Grid
      .of(width = 3, height = 3, fill = Cell.Empty) {
        case (0, 1) => Cell.Alive
        case (1, 1) => Cell.Alive
        case (2, 1) => Cell.Alive
      }

  val toad: Grid[Cell] =
    Grid
      .of(width = 4, height = 2, fill = Cell.Empty) {
        case (1, 0) => Cell.Alive
        case (2, 0) => Cell.Alive
        case (3, 0) => Cell.Alive
        case (0, 1) => Cell.Alive
        case (1, 1) => Cell.Alive
        case (2, 1) => Cell.Alive
      }

  val beacon: Grid[Cell] =
    Grid
      .fill(width = 4, height = 4, fill = Cell.Empty)
      .combine(Still.block, default = Cell.Empty)
      .combine(Still.block, default = Cell.Empty, atX = 2, atY = 2)
}
