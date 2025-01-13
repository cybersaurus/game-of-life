package gameoflife.model

import gameoflife.model.Cell.Alive
import gameoflife.model.Cell.Empty

import scala.annotation.tailrec
import scala.util.chaining.*

object GridOfCells {

  @tailrec
  def ticks(grid: Grid[Cell], count: Int = 1): Grid[Cell] =
    count match {
      case 0     => grid
      case 1     => tick(grid)
      case other => ticks(tick(grid), other - 1)
    }

  def tick(grid: Grid[Cell]): Grid[Cell] =
    grid
      .map { case (cell, (x, y)) =>
        grid
          .neighbours(x, y)
//          .debug(s">>>>> neighbours ($x,$y)")
          .flatten
          .count(_ == Alive)
          .pipe { count =>
            cell match {
              case Alive if Set(2, 3).contains(count) => Alive
              case Empty if Set(3).contains(count)    => Alive
              case _                                  => Empty
            }
          }
      }
}
