package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite

import TestFixtures.*
import Shape.*

class TestBoundingBox extends AnyFunSuite:

  def testBoundingBox(description: String, s: Shape, x: Int, y: Int, width: Int, height: Int): Unit =
    test(description):
      val Location(u, v, Rectangle(w, h)) = boundingBox(s)
      assert(x == u)
      assert(y == v)
      assert(width == w)
      assert(height == h)

  // TODO comment these tests back in

  testBoundingBox("simple ellipse", simpleEllipse, -50, -30, 100, 60)
  testBoundingBox("simple rectangle", simpleRectangle, 0, 0, 80, 120)
  testBoundingBox("simple location", simpleLocation, 70, 30, 80, 120)
  testBoundingBox("basic group", basicGroup, -50, -30, 100, 70)
  testBoundingBox("simple group", simpleGroup, 150, 70, 350, 280)
  testBoundingBox("complex group", complexGroup, 30, 60, 470, 320)

end TestBoundingBox