package gameoflife.model

import cats.Eq
import gameoflife.model.shape.Oscillators

import scala.util.chaining.*

import ArrayGridFixtures.given
import GridOfCells.given_Empty_State

object GridOfCellsSpec extends weaver.FunSuite {

  protected given [A: Eq]: Eq[Grid[A]] = Eq.by(_.asInstanceOf[ArrayGrid[A]])

  private val hBlinker5x5: ArrayGrid[State] =
    ArrayGrid
      .fill(width = 5, height = 5, fill = State.Empty)
      .add(Oscillators.blinker, default = State.Empty, atX = 1, atY = 1)

  private val vBlinker5x5: ArrayGrid[State] =
    ArrayGrid
      .of(width = 5, height = 5, fill = State.Empty) {
        case (2, 1) => State.Alive
        case (2, 2) => State.Alive
        case (2, 3) => State.Alive
      }

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
