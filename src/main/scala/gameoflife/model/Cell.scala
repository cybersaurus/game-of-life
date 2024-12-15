package gameoflife.model

import cats.Eq
import cats.Show

enum Cell {
  case Empty, Alive
}

object Cell {
  given Eq[Cell] = Eq.fromUniversalEquals
  given Show[Cell] = Show.fromToString
}
