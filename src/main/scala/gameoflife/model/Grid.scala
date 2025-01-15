package gameoflife.model

import scala.reflect.ClassTag

trait Grid[A: ClassTag] {
  val height: Int
  val width: Int

  def getCellAt(x: Int, y: Int): Option[A]

  def map[B: ClassTag](f: (A, (Int, Int)) => B): Grid[B]

  def neighboursCount(x: Int, y: Int)(pred: A => Boolean): Int

  def reduce(
      reduceCellsToRow: (A, A) => A,
      reduceRowsToResult: (A, A) => A
  ): A
}
