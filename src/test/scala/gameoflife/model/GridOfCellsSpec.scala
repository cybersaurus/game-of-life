package gameoflife.model

import scala.util.chaining.*

object GridOfCellsSpec extends weaver.FunSuite {
  private val hBlinker: Grid[Cell] = Grid
    .of(5, 5, Cell.Empty)
    .setAt(1, 2, Cell.Alive)
    .setAt(2, 2, Cell.Alive)
    .setAt(3, 2, Cell.Alive)

  val vBlinker: Grid[Cell] = Grid
    .of(5, 5, Cell.Empty)
    .setAt(2, 1, Cell.Alive)
    .setAt(2, 2, Cell.Alive)
    .setAt(2, 3, Cell.Alive)

  test("tick 1 time from hBlinker to vBlinker") {
    expect.eql(vBlinker, GridOfCells.tick(hBlinker))
  }

  test("tick 1 time from vBlinker to hBlinker") {
    expect.eql(hBlinker, GridOfCells.tick(vBlinker))
  }

  test("tick 2 times from/to hBlinker") {
    expect.eql(hBlinker, GridOfCells.tick(hBlinker).pipe(GridOfCells.tick))
  }

  test("ticks 10 times from/to hBlinker") {
    expect.eql(hBlinker, GridOfCells.ticks(hBlinker, count = 10))
  }
}
