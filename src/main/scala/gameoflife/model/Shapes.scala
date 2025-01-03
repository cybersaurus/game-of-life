package gameoflife.model

object Shapes {
  val hBlinker: Grid[Cell] =
    Grid
      .of(width = 3, height = 3, default = Cell.Empty) {
        case (0, 1) => Cell.Alive
        case (1, 1) => Cell.Alive
        case (2, 1) => Cell.Alive
      }

  private val vBlinker: Grid[Cell] =
    Grid
      .of(width = 3, height = 3, default = Cell.Empty) {
        case (1, 0) => Cell.Alive
        case (1, 1) => Cell.Alive
        case (1, 2) => Cell.Alive
      }

  val hBlinker5x5: Grid[Cell] =
    Grid
      .empty(width = 5, height = 5, empty = Cell.Empty)
      .combine(hBlinker, default = Cell.Empty, atX = 1, atY = 1)

  val vBlinker5x5: Grid[Cell] =
    Grid
      .empty(width = 5, height = 5, empty = Cell.Empty)
      .combine(vBlinker, default = Cell.Empty, atX = 1, atY = 1)
}
