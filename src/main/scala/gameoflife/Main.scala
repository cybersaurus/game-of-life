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
import gameoflife.model.Grid
import gameoflife.model.State

import scala.concurrent.duration.*
import scala.util.chaining.*

object Main extends cats.effect.IOApp.Simple {

  import gameoflife.model.GridOfCells.given

  override def run: IO[Unit] =
    fs2.Stream
      .iterate[IO, (Grid[State], Int)](initial -> 1)(nextGrid.tupled)
      .metered(100.milliseconds)
      .map(reduceGridToImage)
      .map(Image.compile)
      .take(250)
      .animateToIO(Frame.default.withTitle("Game of Life"))

  private val grid: Grid[State] =
//    gameoflife.model.ArrayGrid.fill(width = 30, height = 40, fill = State.Empty)
    gameoflife.model.MapGrid.empty[State](width = 30, height = 40)

  private val initial: Grid[State] =
    grid
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

  // TODO: Add debug method
  private val nextGrid: (Grid[State], Int) => (Grid[State], Int) = (grid, generation) =>
//    import cats.syntax.show.toShow
//    import scala.util.chaining.*
//    import gameoflife.model.MapGrid.given
//    import State.given_Show_State

//    given cats.Show[Grid[State]] = gameoflife.model.MapGrid.gridShow[State]
//    given cats.Show[Grid[State]] = gameoflife.model.ArrayGrid.gridShow[State]

    (gameoflife.model.GridOfCells.tick(grid), generation + 1)
//      .tap { case (newGrid: Grid[State], newGen: Int) =>
//        println(s"Gen: [$newGen], old: [${grid.show}], new: [${newGrid.show}]")
//      }

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
