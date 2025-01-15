package gameoflife

import cats.effect.IO
import cats.implicits.*
import doodle.image.Image
import doodle.interact.*
import doodle.interact.syntax.all.*
import doodle.java2d.*
import gameoflife.gfx.Square
import gameoflife.model.shapes.array.Oscillators
import gameoflife.model.shapes.array.Spaceships
import gameoflife.model.shapes.array.Still
import gameoflife.model.ArrayGrid
import gameoflife.model.Grid
import gameoflife.model.State

import scala.concurrent.duration.*

object Main extends cats.effect.IOApp.Simple {

  private val chooseSquare: State => Square = {
    case State.Alive => Square.alive
    case State.Empty => Square.empty
  }

  private val gridToImage: (Grid[State], Int) => Image = (grid, generation) =>
    grid
      .map { case (cell, (_, _)) => chooseSquare(cell) }
      .reduce(_ beside _, _ above _)
      .above(Image.text(s"Generation: $generation"))

  private val initial: Grid[State] =
    ArrayGrid
      .fill(width = 30, height = 30, fill = State.Empty)
      .combine(Oscillators.blinker, default = State.Empty, atX = 2, atY = 1)
      .combine(Oscillators.toad, default = State.Empty, atX = 7, atY = 2)
      .combine(Oscillators.beacon, default = State.Empty, atX = 13, atY = 1)
      .combine(Still.block, default = State.Empty, atX = 2, atY = 7)
      .combine(Still.beehive, default = State.Empty, atX = 7, atY = 7)
      .combine(Still.boat, default = State.Empty, atX = 13, atY = 7)
      .combine(Still.loaf, default = State.Empty, atX = 1, atY = 11)
      .combine(Still.tub, default = State.Empty, atX = 7, atY = 12)
      .combine(Spaceships.glider, default = State.Empty, atX = 1, atY = 25)

  private val nextGrid: (Grid[State], Int) => (Grid[State], Int) = (grid, generation) =>
//    import cats.syntax.show.*
//    import scala.util.chaining.*
    (gameoflife.model.GridOfCells.tick(grid), generation + 1)
//      .tap { case (newGrid: Grid[Cell], newGen: Int) =>
//        println(s"Gen: [$newGen], old: [${grid.show}], new: [${newGrid.show}]")
//      }

  override def run: IO[Unit] =
    fs2.Stream
      .iterate[IO, (Grid[State], Int)](initial -> 1)(nextGrid.tupled)
      .metered(200.milliseconds)
      .map(gridToImage.tupled)
      .map(Image.compile)
      .take(50)
      .animateToIO(Frame.default.withTitle("Game of Life"))
}
