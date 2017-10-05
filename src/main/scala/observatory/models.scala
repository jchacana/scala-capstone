package observatory

import com.sksamuel.scrimage.Pixel

import scala.math._

case class Location(lat: Double, lon: Double) {
  lazy val point: Point = Point(toRadians(lat), toRadians(lon))
}


case class Point(lat: Double, lon: Double) {
  lazy val location: Location = Location(toDegrees(lat), toDegrees(lon))
}

case class Color(red: Int, green: Int, blue: Int) {
  def toPixel(): Pixel = Pixel.apply(red, green, blue, 255)

}

