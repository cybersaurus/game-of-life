package gameoflife.model.shapes

import gameoflife.model.Cell
import gameoflife.model.Grid

object Spaceships {
  val glider: Grid[Cell] =
    Grid
      .of(width = 3, height = 3, fill = Cell.Empty) {
        case (1, 0) => Cell.Alive
        case (2, 0) => Cell.Alive
        case (2, 1) => Cell.Alive
        case (2, 2) => Cell.Alive
        case (0, 1) => Cell.Alive
      }
}
