package gameoflife.model

import gameoflife.model.State.Alive
import gameoflife.model.State.Empty

import scala.annotation.tailrec
import scala.util.chaining.*

object GridOfCells {
  given Empty[State] = gameoflife.model.Empty.of(State.Empty)

  @tailrec
  def ticks(grid: Grid[State], count: Int = 1): Grid[State] =
    count match {
      case 0     => grid
      case 1     => tick(grid)
      case other => ticks(tick(grid), other - 1)
    }

  def tick(grid: Grid[State]): Grid[State] =
    grid
      .map { case (cell, (x, y)) =>
        grid
          .neighboursCount(x, y)(_ == Alive)
          .pipe { count =>
            cell match {
              case Alive if Set(2, 3).contains(count) => Alive
              case Empty if Set(3).contains(count)    => Alive
              case _                                  => Empty
            }
          }
      }
}
