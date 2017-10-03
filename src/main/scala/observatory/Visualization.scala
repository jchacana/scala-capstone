package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {



  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val combs = distanceTempCombination(temperatures, location)
  }

  /**
    * Calculates Great Circle Distance between two locations
    * @param location
    * @param otherLocation
    * @return
    */
  def gdc(location: Location, otherLocation: Location) = ???

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
    ???
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

