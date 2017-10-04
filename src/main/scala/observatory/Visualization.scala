package observatory

import com.sksamuel.scrimage.Image
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EARTH_RADIUS = 6371.0
  val P = 3.0


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
    val sortedPoints = points.toList.sortWith(_._1 < _._1).toArray

    for(i <- 0 until sortedPoints.length - 1) {
      (sortedPoints(i), sortedPoints(i + 1)) match {
        case ((v1, Color(r1, g1, b1)), (v2, Color(r2, g2, b2))) => {
          if(v1 > value) {
            Color(r1, g1, b1)
          }
          else if (v2 > value) {
            val ratio = (value - v1) / (v2 - v1)
            Color(
              math.round(r1 + (r2 - r1) * ratio).toInt,
              math.round(g1 + (g2 - g1) * ratio).toInt,
              math.round(b1 + (b2 - b1) * ratio).toInt
            )
          }
        }
      }
    }

    sortedPoints(sortedPoints.length - 1)._2

  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

