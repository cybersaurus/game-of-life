package gameoflife.model

import scala.util.chaining.*

object GridOfCellsSpec extends weaver.FunSuite with ArrayGridFixtures {

  test("tick 1 time from hBlinker to vBlinker") {
    expect.eql(vBlinker5x5, GridOfCells.tick(hBlinker5x5))
  }

  test("tick 1 time from vBlinker to hBlinker") {
    expect.eql(hBlinker5x5, GridOfCells.tick(vBlinker5x5))
  }

  test("tick 2 times from/to hBlinker") {
    expect.eql(hBlinker5x5, GridOfCells.tick(hBlinker5x5).pipe(GridOfCells.tick))
  }

  test("ticks 10 times from/to hBlinker") {
    expect.eql(hBlinker5x5, GridOfCells.ticks(hBlinker5x5, count = 10))
  }
}
