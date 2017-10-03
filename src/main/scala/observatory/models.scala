package observatory

import scala.math._

case class Location(lat: Double, lon: Double) {
  lazy val point: Point = Point(toRadians(lat), toRadians(lon))
}


case class Point(x: Double, y: Double) {
  lazy val location: Location = Location(toDegrees(x), toDegrees(y))
}

case class Color(red: Int, green: Int, blue: Int)

