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
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

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
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

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
        blue = math.round(paColor.blue + (pbColor.blue - paColor.blue ) * ratio).toInt,
        255)
    }
    case (Some(pA), None) => pA._2
    case (None, Some(pB)) => pB._2
    case _ => Color(0, 0, 0, 255)
  }


  trait Visualizer {
    val alpha: Int
    val width: Int
    val height: Int


    def toPixel(color: Color) = Pixel.apply(color.red, color.green, color.blue, alpha)
    /**
      * @param temperatures Known temperatures
      * @param colors Color scale
      * @return A 360×180 image where each pixel shows the predicted temperature at its location
      */
    def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

      lazy val temps = temperatures.toList.sortBy(t => (t._1.lat, t._1.lon))
      val pixels: Array[Pixel] = buildPixelArray(temps, colors)
      Image.apply(width, height, pixels)
    }

    /**
      * Builds a pixel Array from a collection of (Location, Double), sorted by Location
      * @param temperatures
      * @param colors
      * @return
      */
    def buildPixelArray(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Array[Pixel] = {
      (0 until width * height).par.map {
        pos => {
          toPixel(interpolateColor(
            colors,
            predictTemperature(temperatures, indexToLocation(width, height, pos))
          ))
        }
      }.toArray
    }

    def indexToLocation(imageWidth: Int, imageHeight: Int, index: Int): Location = {
      val widthFactor = 180 * 2 / imageWidth.toDouble
      val heightFactor = 90 * 2 / imageHeight.toDouble

      val x:Int = index % imageWidth
      val y:Int = index / imageWidth

      Location(90 - (y * heightFactor), (x * widthFactor) - 180)
    }
    def xyToLocation(x: Int, y: Int): Location = Location(90 - y, x - 180)

  }

  class Milestone2Visualizer extends Visualizer {
    override val alpha: Int = 255
    override val width: Int = IMG_WIDTH
    override val height: Int = IMG_HEIGHT

  }


  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val visualizer:Visualizer = new Milestone2Visualizer

    visualizer.visualize(temperatures, colors)
  }




}

