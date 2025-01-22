package gameoflife.model

import cats.syntax.show.toShow
import cats.Show
import gameoflife.model.shape.Shape

import scala.reflect.ClassTag
import scala.util.chaining.*

final case class MapGrid[A: ClassTag: Empty] private (
    override val width: Int,
    override val height: Int,
    cells: Map[(Int, Int), A]
) extends Grid[A] {

  override given gridShow[A: Show]: Show[Grid[A]] = Show.show(_.asInstanceOf[MapGrid[A]].cells.show)

  private val empty: A = summon[Empty[A]].empty

  override def getCellAt(x: Int, y: Int): Option[A] = cells.get(x -> y)

  override def map[B: ClassTag: Empty](f: (A, (Int, Int)) => B): Grid[B] =
    // Collect current live cells, plus their neighbours. If a neighbour is undefined then use Empty value.
    cells
      .flatMap { case ((x, y), cell) =>
        neighbours(x, y).view
          .mapValues { cellMaybe => cellMaybe.getOrElse(empty) }
          .toMap
          .updated(x -> y, cell)
      }
      .map { case ((x, y), cell) => (x, y) -> f(cell, (x, y)) }
      .filterNot { case ((_, _), cell) => cell == empty }
      .pipe(MapGrid.of(width, height))

  override def add(shape: Shape[A], default: A, atX: Int, atY: Int): Grid[A] =
    scala.collection.mutable.Map
      .from(cells)
      .addAll(shape.map { case (a, (x, y)) => (x + atX, y + atY) -> a }.cells.flatten)
      .filterNot { case ((_, _), cell) => cell == empty }
      .toMap
      .pipe(
        MapGrid.of(
          width = math.max(width, shape.width),
          height = math.max(height, shape.height)
        )
      )
}

object MapGrid {
  def empty[A: ClassTag: Empty](width: Int = 0, height: Int = 0): MapGrid[A] =
    of(width, height)(Map.empty)

  def of[A: ClassTag: Empty](width: Int, height: Int)(cells: Map[(Int, Int), A]): MapGrid[A] =
    MapGrid(width, height, cells)
}
