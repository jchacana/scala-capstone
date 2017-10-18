package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{Visualizer, interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

   /**
    * @param theTile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(theTile: Tile): Location = {
    theTile.toLocation
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val visualizer = new Milestone3Visualizer

    visualizer.visualize(temperatures, colors, tile)
  }

  class Milestone3Visualizer extends Visualizer {
    override val alpha: Int = 127
    override val width: Int = 256
    override val height: Int = 256


    def visualize(temperatures: Iterable[(Location, Double)],
                           colors: Iterable[(Double, Color)],
                           tile: Tile): Image = {
      lazy val temps = temperatures.toList.sortBy(t => (t._1.lat, t._1.lon))
      val pixels: Array[Pixel] = buildPixelArray(temps, colors, tile)
      Image.apply(width, height, pixels)
    }


    def buildPixelArray(temperatures: Iterable[(Location, Double)],
                                 colors: Iterable[(Double, Color)],
                                 tile: Tile): Array[Pixel] = {
      (0 until width * height).par.map {
        pos => {
          toPixel(interpolateColor(
            colors,
            predictTemperature(temperatures, indexToTileLocation(width, height, pos, tile))
          ))
        }
      }.toArray
    }

    def indexToTileLocation(imageWidth: Int,
                            imageHeight: Int,
                            index: Int,
                            tile: Tile): Location = {


      val xPos = (index % imageWidth).toDouble / imageWidth + tile.x
      val yPos = (index / imageHeight).toDouble / imageHeight + tile.y

      Tile(xPos, yPos, tile.zoom).toLocation
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
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for{
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } {
      generateImage(year, Tile.apply(x, y, zoom), data)
    }
  }

}
