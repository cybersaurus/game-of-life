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

  protected val increasingIntsShape: Shape[Int] = Shape.of(width = 2, height = 5, fill = 0)(increasingInts(width = 2))
}
