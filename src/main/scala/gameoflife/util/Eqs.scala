package gameoflife.util

import cats.Eq

object Eqs {
  extension (eq: Eq.type) {

    def all[A](eqs: Eq[A]*): Eq[A] =
      eqs.iterator.reduce((eq1, eq2) => Eq.and(eq1, eq2))
  }
}
