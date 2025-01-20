package gameoflife

import cats.effect.IO
import cats.implicits.*
import cats.Foldable
import cats.Monoid
import doodle.image.Image
import doodle.interact.*
import doodle.interact.syntax.all.*
import doodle.java2d.*
import gameoflife.gfx.Square
import gameoflife.model.shape.Oscillators
import gameoflife.model.shape.Spaceships
import gameoflife.model.shape.Still
import gameoflife.model.ArrayGrid
import gameoflife.model.Grid
import gameoflife.model.State

import scala.concurrent.duration.*
import scala.util.chaining.*

object Main extends cats.effect.IOApp.Simple {

  override def run: IO[Unit] =
    fs2.Stream
      .iterate[IO, (Grid[State], Int)](initial -> 1)(nextGrid.tupled)
      .metered(100.milliseconds)
      .map(reduceGridToImage)
      .map(Image.compile)
      .take(250)
      .animateToIO(Frame.default.withTitle("Game of Life"))

  private val initial: Grid[State] =
    ArrayGrid
      .fill(width = 30, height = 40, fill = State.Empty)
      .add(Oscillators.blinker, default = State.Empty, atX = 2, atY = 1)
      .add(Oscillators.toad, default = State.Empty, atX = 7, atY = 2)
      .add(Oscillators.beacon, default = State.Empty, atX = 13, atY = 1)
      .add(Still.block, default = State.Empty, atX = 2, atY = 7)
      .add(Still.beehive, default = State.Empty, atX = 7, atY = 7)
      .add(Still.boat, default = State.Empty, atX = 13, atY = 7)
      .add(Still.loaf, default = State.Empty, atX = 1, atY = 11)
      .add(Still.tub, default = State.Empty, atX = 7, atY = 12)
      .add(Spaceships.glider, default = State.Empty, atX = 1, atY = 25)
      .add(Spaceships.lightweight, default = State.Empty, atX = 15, atY = 19)
      .add(Spaceships.middleweight, default = State.Empty, atX = 15, atY = 26)
      .add(Spaceships.heavyweight, default = State.Empty, atX = 15, atY = 33)

  private val nextGrid: (Grid[State], Int) => (Grid[State], Int) = (grid, generation) =>
    (gameoflife.model.GridOfCells.tick(grid), generation + 1)

  private def reduceGridToImage(grid: Grid[State], generation: Int): Image = {
    given imagesBeside: Monoid[Image] = Monoid.instance(
      emptyValue = Image.Elements.Empty,
      cmb = _ beside _
    )

    given imagesAbove: Monoid[Image] = Monoid.instance(
      emptyValue = Image.Elements.Empty,
      cmb = _ above _
    )

    val getStateAt: (Int, Int) => State = (y, x) => grid.getCellAt(x, y).getOrElse(State.Empty)

    val chooseSquare: State => Square = {
      case State.Alive => Square.alive
      case State.Empty => Square.empty
    }

    List
      .tabulate[State](grid.height, grid.width)(getStateAt)
      .map(row => Foldable[List].foldMap(row)(chooseSquare)(using imagesBeside))
      .pipe(rows => Foldable[List].fold(rows)(using imagesAbove))
      .above(Image.text(s"Generation: $generation"))
  }
}
