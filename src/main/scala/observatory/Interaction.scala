package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{Visualizer, interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    Tile(x, y, zoom.toShort).toLocation
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val visualizer = new Milestone3Visualizer

    visualizer.visualize(temperatures, colors, x, y, zoom)
  }

  class Milestone3Visualizer extends Visualizer {
    override val alpha: Int = 127
    override val width: Int = 256
    override val height: Int = 256


    def visualize(temperatures: Iterable[(Location, Double)],
                           colors: Iterable[(Double, Color)],
                           x: Int, y: Int, zoom: Int): Image = {
      lazy val temps = temperatures.toList.sortBy(t => (t._1.lat, t._1.lon))
      val pixels: Array[Pixel] = buildPixelArray(temps, colors, x, y, zoom)
      Image.apply(width, height, pixels)
    }


    def buildPixelArray(temperatures: Iterable[(Location, Double)],
                                 colors: Iterable[(Double, Color)],
                                 x:Int, y: Int, zoom: Int): Array[Pixel] = {
      (0 until width * height).par.map {
        pos => {
          toPixel(interpolateColor(
            colors,
            predictTemperature(temperatures, indexToTileLocation(width, height, pos, x, y, zoom))
          ))
        }
      }.toArray
    }

    def indexToTileLocation(imageWidth: Int,
                            imageHeight: Int,
                            index: Int,
                            x: Int,
                            y: Int,
                            zoom: Int): Location = {


      val xPos = (index % imageWidth).toDouble / imageWidth + x
      val yPos = (index / imageWidth).toDouble / imageHeight + y

      Tile(xPos.toInt, yPos.toInt, zoom.toShort).toLocation
    }
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for{
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } {
      generateImage(year, zoom, x, y, data)
    }
  }

}
