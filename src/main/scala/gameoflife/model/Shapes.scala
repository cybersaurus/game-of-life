package gameoflife.model

object Shapes {
  val hBlinker5x5: Grid[Cell] =
    Grid
      .of(5, 5, Cell.Empty)
      .setAt(1, 2, Cell.Alive)
      .setAt(2, 2, Cell.Alive)
      .setAt(3, 2, Cell.Alive)

  val vBlinker5x5: Grid[Cell] =
    Grid
      .of(5, 5, Cell.Empty)
      .setAt(2, 1, Cell.Alive)
      .setAt(2, 2, Cell.Alive)
      .setAt(2, 3, Cell.Alive)
}
