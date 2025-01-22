package gameoflife.model

import cats.Show
import gameoflife.model.shape.Shape

import scala.reflect.ClassTag
import scala.util.chaining.*

trait Grid[A: ClassTag: Empty] {
  val height: Int
  val width: Int

  given gridShow[A: Show]: Show[Grid[A]]

  def getCellAt(x: Int, y: Int): Option[A]

  def map[B: ClassTag: Empty](f: (A, (Int, Int)) => B): Grid[B]

  def add(shape: Shape[A], default: A, atX: Int = 0, atY: Int = 0): Grid[A]

  final def neighboursCount(x: Int, y: Int)(pred: A => Boolean): Int =
    neighbours(x, y)
      .count { case ((_, _), cellMaybe) => cellMaybe.exists(pred) }

  protected[model] def neighbours(x: Int, y: Int): Map[(Int, Int), Option[A]] = {
    def kvAt(x: Int, y: Int): ((Int, Int), Option[A]) = getCellAt(x, y).pipe(cellMaybe => (x -> y) -> cellMaybe)

    Map.empty[(Int, Int), Option[A]]
      + kvAt(left(x), upper(y))
      + kvAt(x, upper(y))
      + kvAt(right(x), upper(y))
      + kvAt(left(x), y)
      + kvAt(right(x), y)
      + kvAt(left(x), lower(y))
      + kvAt(x, lower(y))
      + kvAt(right(x), lower(y))
  }

  protected[model] def upper(y: Int): Int = if y > 0 then y - 1 else this.height - 1
  protected[model] def lower(y: Int): Int = if y < height - 1 then y + 1 else 0
  protected[model] def left(x: Int): Int = if x > 0 then x - 1 else width - 1
  protected[model] def right(x: Int): Int = if x < width - 1 then x + 1 else 0
}
object Grid {
  extension [A: cats.Show](grid: Grid[A])
    def debug(prefix: String): Grid[A] = grid.tap(_ => println(s"$prefix: [${grid.gridShow.show(grid)}]"))
}

trait Empty[A] {
  val empty: A
}
object Empty {
  def of[A](a: A): Empty[A] = new Empty[A] {
    override val empty: A = a
  }
  given Empty[Int] = Empty.of(0)
}
