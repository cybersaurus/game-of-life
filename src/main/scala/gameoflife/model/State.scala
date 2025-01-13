package gameoflife.model

import cats.Eq
import cats.Show

enum State {
  case Empty, Alive
}

object State {
  given Eq[State] = Eq.fromUniversalEquals
  given Show[State] = Show.fromToString
}
