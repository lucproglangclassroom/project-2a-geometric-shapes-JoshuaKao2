package edu.luc.cs.laufer.cs371.shapes

import Shape.*

object boundingBox:
  def apply(s: Shape): Location = s match
    case Rectangle(w, h) =>
      // Rectangle starting at origin, we just return wrapped Location class
      Location(0, 0, Rectangle(w, h))

    case Ellipse(x, y) =>
      // Ellipses extend down by -x -y as well. To account for this the location
      // Starts at negative x,y and extends twice the positive values (accounts for
      // negative offset starting position)
      Location(-x, -y, Rectangle(2 * x, 2 * y))

    case Location(x, y, shape) =>
     val shapeLocation = apply(shape)
     Location(shapeLocation.x + x, shapeLocation.y + y, shapeLocation.shape)

    case Group(shapes*) =>
      val locations = shapes.map(apply) // Flattening shapes into a Seq of locations

      /*
        Our min values only look at the offset because these should always be less
        than our dimensions, considering the dimensions are at offset_x + width or offset_y + height
      */
      val minX = locations.map(_.x).min
      val minY = locations.map(_.y).min
      /*
        Kind of weird, but our locations function "apply" always returns a location with a
        rectangle here. asInstanceOf was how I got rid of the compiler complaining about
        other shapes, even though that is impossible after apply is completed
      */
      val maxX = locations.map(l => l.x + l.shape.asInstanceOf[Rectangle].width).max
      val maxY = locations.map(l => l.y + l.shape.asInstanceOf[Rectangle].height).max
      Location(minX, minY, Rectangle(maxX - minX, maxY - minY))
end boundingBox