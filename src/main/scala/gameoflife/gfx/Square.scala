package gameoflife.gfx

import doodle.core.Color
import doodle.image.Image

type Square = Image

object Square {
  val alive: Square = of(Color.orange)
  val empty: Square = of(Color.white)

  private def of(colour: Color, size: Int = 15): Image =
    Image
      .square(size)
      .strokeWidth(1)
      .strokeColor(Color.lightGray)
      .fillColor(colour)
      .margin(1)
}
