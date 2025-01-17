package gameoflife.model

import cats.syntax.eq.*
import cats.Eq

import scala.reflect.ClassTag

trait ArraysFixtures {
  given arrayEq[A: Eq]: Eq[Array[A]] =
    Eq.and(
      Eq.by(_.length),
      Eq.instance((arr1, arr2) => arr1.corresponds(arr2.iterator)(_ === _))
    )

  protected val increasingCells3x4: Array[Array[Int]] =
    cellsWith(width = 3, height = 4, default = -1)(increasingInts(width = 3))

  protected def cellsWith[A: ClassTag](width: Int, height: Int, default: => A)(
      insertAtCoords: PartialFunction[(Int, Int), A]
  ): Array[Array[A]] = Array.tabulate(height, width)((y, x) => insertAtCoords.applyOrElse((x, y), (_, _) => default))

  protected def increasingInts(width: Int): PartialFunction[(Int, Int), Int] = (x, y) => (x + 1) + (y * width)
  protected def increasingIntsWithCoords(width: Int): PartialFunction[(Int, Int), (Int, (Int, Int))] = (x, y) =>
    ((x + 1) + (y * width), x -> y)
}
