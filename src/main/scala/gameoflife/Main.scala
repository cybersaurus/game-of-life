package gameoflife

import cats.effect.IO
import cats.implicits.*
import cats.syntax.show.*
import doodle.image.Image
import doodle.interact.*
import doodle.interact.syntax.all.*
import doodle.java2d.*
import gameoflife.gfx.Square
import gameoflife.model.Cell
import gameoflife.model.Grid
import gameoflife.model.Shapes

import scala.concurrent.duration.*
import scala.util.chaining.*

object Main extends cats.effect.IOApp.Simple {

  private val chooseSquare: Cell => Square = {
    case Cell.Alive => Square.alive
    case Cell.Empty => Square.empty
  }

  private val gridToImage: (Grid[Cell], Int) => Image = (grid, generation) =>
    grid.map(chooseSquare).reduce(_ beside _, _ above _)

  private val initial: (Grid[Cell], Int) = Shapes.hBlinker5x5 -> 1

  private val nextGrid: (Grid[Cell], Int) => (Grid[Cell], Int) = (grid, generation) =>
    (gameoflife.model.GridOfCells.tick(grid) -> (generation + 1))
      .tap { case (newGrid: Grid[Cell], newGen: Int) =>
        println(s"Gen: [$newGen], old: [${grid.show}], new: [${newGrid.show}]")
      }

  println(s"hBlinker: [${Shapes.hBlinker5x5.show}]")

  override def run: IO[Unit] =
    fs2.Stream
      .iterate[IO, (Grid[Cell], Int)](initial)(nextGrid.tupled)
      .metered(250.milliseconds)
      .map(gridToImage.tupled)
      .map(Image.compile)
      .take(50)
      .animateToIO(Frame.default.withTitle("Game of Life"))
}
