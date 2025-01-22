package gameoflife.main

import gameoflife.model.Grid
import gameoflife.model.State

object MapGridMain extends Main {

  import gameoflife.model.GridOfCells.given

  override protected final lazy val grid: Grid[State] =
    gameoflife.model.MapGrid.empty[State](width = 30, height = 40)
}
