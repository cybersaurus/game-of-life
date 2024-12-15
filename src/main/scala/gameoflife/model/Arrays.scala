package gameoflife.model

import cats.syntax.eq.*
import cats.syntax.show.*
import cats.Eq
import cats.Show

object Arrays {

  given arrayEq[A: Eq]: Eq[Array[A]] =
    Eq.and(
      Eq.by(_.length),
      Eq.instance((arr1, arr2) => arr1.corresponds(arr2.iterator)(_ === _))
    )

  given arrayShow[A: Show]: Show[Array[A]] = Show.show(arr => arr.map(_.show).mkString(","))

  extension [A: Show](twoDimArray: Array[Array[A]]) {
    def debug(prefix: String): Array[Array[A]] = {
      println(s"$prefix: [${twoDimArray.show}]")
      twoDimArray
    }
  }
}
