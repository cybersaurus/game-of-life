package gameoflife.model

import cats.syntax.eq.*
import cats.syntax.show.*
import cats.Eq
import cats.Show

import scala.reflect.ClassTag
import scala.util.chaining.*

object Arrays {

  given arrayEq[A: Eq]: Eq[Array[A]] =
    Eq.and(
      Eq.by(_.length),
      Eq.instance((arr1, arr2) => arr1.corresponds(arr2.iterator)(_ === _))
    )

  given arrayShow[A: Show]: Show[Array[A]] = Show.show(arr => arr.map(_.show).mkString(","))

  extension [A: ClassTag: Show](twoDimArray: Array[Array[A]]) {
    def debug(prefix: String): Array[Array[A]] =
      twoDimArray.tap(arr => println(s"$prefix: [${arr.show}]"))

    def rotateRowsUp: Array[Array[A]] =
      twoDimArray.drop(1) ++ twoDimArray.take(1)

    def rotateRowsDown: Array[Array[A]] =
      twoDimArray.takeRight(1) ++ twoDimArray.dropRight(1)

    def rotateColsLeft: Array[Array[A]] =
      twoDimArray.map { row => row.drop(1) ++ row.take(1) }

    def rotateColsRight: Array[Array[A]] =
      twoDimArray.map { row => row.takeRight(1) ++ row.dropRight(1) }
  }
}
