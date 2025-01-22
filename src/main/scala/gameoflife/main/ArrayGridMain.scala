package gameoflife.main

import gameoflife.model.Grid
import gameoflife.model.State

object ArrayGridMain extends Main {

  import gameoflife.model.GridOfCells.given

  override protected final lazy val grid: Grid[State] =
    gameoflife.model.ArrayGrid.fill(width = 30, height = 40, fill = State.Empty)
}
