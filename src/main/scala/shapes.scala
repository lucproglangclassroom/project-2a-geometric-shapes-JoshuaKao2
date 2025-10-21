package edu.luc.cs.laufer.cs371.shapes

/** data Shape = Rectangle(w, h) | Location(x, y, Shape) */
enum Shape derives CanEqual:
  case Rectangle(width: Int, height: Int)
  case Location(x: Int, y: Int, shape: Shape)
  case Ellipse(x: Int, y: Int)
  case Group(shapes: Shape*)
  
