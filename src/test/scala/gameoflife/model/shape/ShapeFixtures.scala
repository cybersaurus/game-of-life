package gameoflife.model.shape

import cats.Eq
import gameoflife.model.ArraysFixtures
import gameoflife.model.Eqs.*

trait ShapeFixtures extends ArraysFixtures {
  protected given [A: Eq]: Eq[Shape[A]] =
    Eq.all(
      Eq.by(_.height),
      Eq.by(_.width),
      Eq.by(_.cells)
    )

  protected val increasingInts3x4Shape: Shape[Int] =
    Shape.of(width = 3, height = 4, fill = 0)(increasingInts(width = 3))
}
