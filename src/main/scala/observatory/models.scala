package observatory

import java.net.URI

import com.sksamuel.scrimage.Pixel

import scala.math._

case class Location(lat: Double, lon: Double) {
  lazy val point: Point = Point(toRadians(lat), toRadians(lon))
}


case class Point(lat: Double, lon: Double) {
  lazy val location: Location = Location(toDegrees(lat), toDegrees(lon))
}

/**
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#X_and_Y
  *
  */
case class Tile(x: Int, y: Int, zoom: Short) {
  def toLocation = new Location(
    toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<zoom))))),
    x.toDouble / (1<<zoom) * 360.0 - 180.0
  )
  def toURI = new URI("http://tile.openstreetmap.org/"+zoom+"/"+x+"/"+y+".png")
}

case class Color(red: Int, green: Int, blue: Int, alpha: Int = 255) {
  def toPixel(): Pixel = Pixel.apply(red, green, blue, alpha)

}

