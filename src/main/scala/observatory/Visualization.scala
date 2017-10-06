package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EARTH_RADIUS = 6371.0
  val P = 3.0

  val IMG_WIDTH = 360
  val IMG_HEIGHT = 180


  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val predictions = distanceTempCombination(temperatures, location)

    predictions.find{ case (distance, temp) => distance <= 1.0} match {
      case Some((_, temp)) => temp
      case _ => idw(predictions, P)
    }
  }


  def idw(predictions: Iterable[(Double, Double)], p: Double): Double = {

    def w(distance: Double) = 1/pow(distance, P)

    val (num, denom) = predictions
      .aggregate((0.0, 0.0))(
        {
          case ((ws, iws), (distance, temp)) => {
            val resp = w(distance)
            (resp * temp + ws, resp + iws)
          }
        }, {
          case ((wsA, iwsA), (wsB, iwsB)) => (wsA + wsB, iwsA + iwsB)
        }
      )
    num / denom
  }

  /**
    * Calculates Great Circle Distance between two locations
    *
    * @param location
    * @param otherLocation
    * @return
    */
  def gdc(location: Location, otherLocation: Location) = {
    val deltaSigma = acos(
      sin(location.point.lat) * sin(otherLocation.point.lat) +
      cos(location.point.lat) * cos(otherLocation.point.lat) * cos(abs(location.point.lon - otherLocation.point.lon))
    )
    EARTH_RADIUS * deltaSigma
  }

  /**
    * Calculates the series of distances between list of (temperatures, Location) and a given Location
    * @param temperatures
    * @param location
    * @return
    */
  def distanceTempCombination(temperatures: Iterable[(Location, Double)], location: Location) = {
    temperatures.map{
      case (otherLocation, temp) => (gdc(location, otherLocation), temp)
    }
  }



  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    points.find(p => p._1 == value) match {
      case Some((_, color)) => color
      case None => {
        val (smaller, greater) = points.toList.sortBy(_._1).partition(_._1 < value)
        interpolation(smaller.reverse.headOption, greater.headOption, value)
      }
    }
  }

  def interpolation(p0: Option[(Double, Color)], p1: Option[(Double, Color)], value: Double): Color = (p0, p1) match {
    case (Some((paVal, paColor)), Some((pbVal, pbColor))) => {
      val ratio = (value - paVal)/(pbVal - paVal)
      Color(
        red = math.round(paColor.red + (pbColor.red - paColor.red) * ratio).toInt,
        green = math.round(paColor.green + (pbColor.green - paColor.green) * ratio).toInt,
        blue = math.round(paColor.blue + (pbColor.blue - paColor.blue ) * ratio).toInt
      )
    }
    case (Some(pA), None) => pA._2
    case (None, Some(pB)) => pB._2
    case _ => Color(0, 0, 0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    lazy val temps = temperatures.toList.sortBy(t => (t._1.lat, t._1.lon))
    val pixels: Array[Pixel] = buildPixelArray(temps, colors)
    Image.apply(IMG_WIDTH, IMG_HEIGHT, pixels)
  }

  /**
    * Builds a pixel Array from a collection of (Location, Double), sorted by Location
    * @param temperatures
    * @param colors
    * @return
    */
  def buildPixelArray(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Array[Pixel] = {
    (0 until IMG_WIDTH * IMG_HEIGHT).par.map {
      pos => {
        interpolateColor(
          colors,
          predictTemperature(temperatures, indexToLocation(IMG_WIDTH, IMG_HEIGHT, pos))
        ).toPixel()
      }
    }.toArray
  }

  def indexToLocation(width: Int, height: Int, index: Int): Location = {
    val widthFactor = 180 * 2 / width.toDouble
    val heightFactor = 90 * 2 / height.toDouble

    val x:Int = index % width
    val y:Int = index / width

    Location(90 - (y * heightFactor), (x * widthFactor) - 180)
  }
  def xyToLocation(x: Int, y: Int): Location = Location(90 - y, x - 180)


}

