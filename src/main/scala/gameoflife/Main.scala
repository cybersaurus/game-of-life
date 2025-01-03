package gameoflife

import cats.effect.IO
import cats.implicits.*
import doodle.image.Image
import doodle.interact.*
import doodle.interact.syntax.all.*
import doodle.java2d.*
import gameoflife.gfx.Square
import gameoflife.model.shapes.Oscillators
import gameoflife.model.shapes.Spaceships
import gameoflife.model.shapes.Still
import gameoflife.model.Cell
import gameoflife.model.Grid

import scala.concurrent.duration.*

object Main extends cats.effect.IOApp.Simple {

  private val chooseSquare: Cell => Square = {
    case Cell.Alive => Square.alive
    case Cell.Empty => Square.empty
  }

  private val gridToImage: (Grid[Cell], Int) => Image = (grid, generation) =>
    grid.map(chooseSquare).reduce(_ beside _, _ above _) above Image.text(s"Generation: $generation")

  private val initial: Grid[Cell] =
    Grid
      .fill(width = 30, height = 30, fill = Cell.Empty)
      .combine(Oscillators.blinker, default = Cell.Empty, atX = 2, atY = 1)
      .combine(Oscillators.toad, default = Cell.Empty, atX = 7, atY = 2)
      .combine(Oscillators.beacon, default = Cell.Empty, atX = 13, atY = 1)
      .combine(Still.block, default = Cell.Empty, atX = 2, atY = 7)
      .combine(Still.beehive, default = Cell.Empty, atX = 7, atY = 7)
      .combine(Still.boat, default = Cell.Empty, atX = 13, atY = 7)
      .combine(Still.loaf, default = Cell.Empty, atX = 1, atY = 11)
      .combine(Still.tub, default = Cell.Empty, atX = 7, atY = 12)
      .combine(Spaceships.glider, default = Cell.Empty, atX = 1, atY = 25)

  private val nextGrid: (Grid[Cell], Int) => (Grid[Cell], Int) = (grid, generation) =>
//    import cats.syntax.show.*
//    import scala.util.chaining.*
    (gameoflife.model.GridOfCells.tick(grid), generation + 1)
//      .tap { case (newGrid: Grid[Cell], newGen: Int) =>
//        println(s"Gen: [$newGen], old: [${grid.show}], new: [${newGrid.show}]")
//      }

  override def run: IO[Unit] =
    fs2.Stream
      .iterate[IO, (Grid[Cell], Int)](initial -> 1)(nextGrid.tupled)
      .metered(200.milliseconds)
      .map(gridToImage.tupled)
      .map(Image.compile)
      .take(50)
      .animateToIO(Frame.default.withTitle("Game of Life"))
}
