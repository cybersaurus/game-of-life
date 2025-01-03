package gameoflife.model.shapes

import gameoflife.model.Cell
import gameoflife.model.Grid

object Still {
  val block: Grid[Cell] =
    Grid
      .fill(width = 2, height = 2, fill = Cell.Alive)

  val beehive: Grid[Cell] =
    Grid
      .of(width = 4, height = 3, fill = Cell.Empty) {
        case (1, 0) => Cell.Alive
        case (2, 0) => Cell.Alive
        case (0, 1) => Cell.Alive
        case (3, 1) => Cell.Alive
        case (1, 2) => Cell.Alive
        case (2, 2) => Cell.Alive
      }

  val loaf: Grid[Cell] =
    Grid
      .of(width = 4, height = 4, fill = Cell.Empty) {
        case (1, 0) => Cell.Alive
        case (2, 0) => Cell.Alive
        case (0, 1) => Cell.Alive
        case (3, 1) => Cell.Alive
        case (1, 2) => Cell.Alive
        case (3, 2) => Cell.Alive
        case (2, 3) => Cell.Alive
      }

  val boat: Grid[Cell] =
    Grid
      .of(width = 3, height = 3, fill = Cell.Empty) {
        case (0, 0) => Cell.Alive
        case (1, 0) => Cell.Alive
        case (0, 1) => Cell.Alive
        case (2, 1) => Cell.Alive
        case (1, 2) => Cell.Alive
      }

  val tub: Grid[Cell] =
    Grid
      .of(width = 3, height = 3, fill = Cell.Empty) {
        case (1, 0) => Cell.Alive
        case (0, 1) => Cell.Alive
        case (2, 1) => Cell.Alive
        case (1, 2) => Cell.Alive
      }
}
