package gameoflife.model

import cats.syntax.show.*
import cats.Show

import scala.reflect.ClassTag
import scala.util.chaining.*

object Arrays {
  given [A: Show]: Show[Array[A]] = Show.show(arr => arr.map(_.show).mkString(","))

  extension [A: ClassTag](cells: Array[Array[A]]) {
    def getCellAt(x: Int, y: Int): Option[A] = {
      def isDefined(x: Int, y: Int): Boolean =
        x >= 0 && y >= 0 && cells.length > y && cells(0).length > x

      def cellAt(x: Int, y: Int): A = cells(y)(x)

      Option.when(isDefined(x, y))(cellAt(x, y))
    }

    def mapWithCoords[B: ClassTag](f: (A, (Int, Int)) => B): Array[Array[B]] =
      zipWithCoords().map(row => row.map(f.tupled))

    private[model] def zipWithCoords(xOffset: Int = 0, yOffset: Int = 0): Array[Array[(A, (Int, Int))]] =
      cells.zipWithIndex
        .map((row, y) => row.zipWithIndex.map { (cell, x) => (cell, (x + xOffset) -> (y + yOffset)) })
  }

  extension [A: ClassTag: Show](cells: Array[Array[A]]) {
    def debug(prefix: String): Array[Array[A]] =
      cells.tap(arr => println(s"$prefix: [${arr.show}]"))
  }
}
