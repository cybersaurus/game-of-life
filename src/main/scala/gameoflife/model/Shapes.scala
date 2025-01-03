package gameoflife.model

object Shapes {
  val hBlinker5x5: Grid[Cell] =
    Grid
      .of(width = 5, height = 5, default = Cell.Empty) {
        case (1, 2) => Cell.Alive
        case (2, 2) => Cell.Alive
        case (3, 2) => Cell.Alive
      }

  val vBlinker5x5: Grid[Cell] =
    Grid
      .of(width = 5, height = 5, default = Cell.Empty) {
        case (2, 1) => Cell.Alive
        case (2, 2) => Cell.Alive
        case (2, 3) => Cell.Alive
      }
}
