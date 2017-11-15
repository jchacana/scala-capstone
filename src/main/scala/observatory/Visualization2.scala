package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x)*(1 - point.y) +
    d10 * point.x * (1 - point.y) +
    d01 * (1 - point.x) * point.y +
    d11 * point.x * point.y
  }

  def pixelLocations(tile: Tile, width: Int, height: Int) ={
    for {
      x <- 0 until width
      y <- 0 until height
    } yield x + y * width -> Tile(x / width + tile.x, y / height + tile.y, tile.zoom).toLocation
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val width = 256
    val height = 256
    val pixels = pixelLocations(tile, width, height).par.map{
      case (position, location) => {

        val latRange = List(math.floor(location.lat).toInt, math.ceil(location.lat).toInt)
        val lonRange = List(math.floor(location.lon).toInt, math.ceil(location.lon).toInt)

        val d00 = grid(GridLocation(latRange(0), lonRange(0)))
        val d01 = grid(GridLocation(latRange(1), lonRange(0)))
        val d10 = grid(GridLocation(latRange(0), lonRange(1)))
        val d11 = grid(GridLocation(latRange(1), lonRange(1)))

        val xFraction = location.lon - lonRange(0)
        val yFraction = latRange(1) - location.lat

        position -> Visualization.interpolateColor(
          colors,
          bilinearInterpolation(CellPoint(xFraction, yFraction), d00, d01, d10, d11)
        ).toPixel(127)
      }
    }.seq
      .sortBy(p => p._1)
      .map(p2 => p2._2)

    Image(width, height, pixels.toArray)
  }

}
