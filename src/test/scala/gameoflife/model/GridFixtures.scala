package gameoflife.model

trait GridFixtures {
  protected final val emptyGrid: Grid[Int] = emptyInt3x4Grid()
  protected final def emptyInt3x4Grid(): Grid[Int] = Grid.of(width = 3, height = 4, empty = 0)

  protected final def int3x3Grid(): Grid[Int] = increasingGrid(width = 3, height = 3)
  protected final def int3x4Grid(): Grid[Int] = increasingGrid(width = 3, height = 4)
  protected final def int5x5Grid(): Grid[Int] = increasingGrid(width = 5, height = 5)

  protected final def increasingGrid(width: Int, height: Int, inc: Int = 1, offset: Int = 0): Grid[Int] =
    makeGrid(width, height) { case (cell, (x, y)) =>
      (((x + 1) + (y * width)) * inc) + offset
    }

  protected final def makeGrid(width: Int, height: Int, empty: Int = 0)(fill: ((Int, (Int, Int))) => Int): Grid[Int] =
    Grid
      .of(width, height, empty)
      .zipWithIndex
      .map(fill)
}
