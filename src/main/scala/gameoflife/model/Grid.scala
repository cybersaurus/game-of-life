package gameoflife.model

import gameoflife.model.shape.Shape

import scala.reflect.ClassTag

trait Grid[A: ClassTag] {
  val height: Int
  val width: Int

  def getCellAt(x: Int, y: Int): Option[A]

  def map[B: ClassTag](f: (A, (Int, Int)) => B): Grid[B]

  def neighboursCount(x: Int, y: Int)(pred: A => Boolean): Int

  def add(shape: Shape[A], default: A, atX: Int = 0, atY: Int = 0): Grid[A]
}
