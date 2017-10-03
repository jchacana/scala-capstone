package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}

import scala.reflect.ClassTag


/**
  * 1st milestone: data extraction
  */

case class Station(stn: String, wban: String, location: Location) extends Serializable
case class Temperature(stn: String, wban: String, month: Int, day: Int, temp: Double )

object Extraction {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("observatory")
      .config("spark.master", "local")
      .getOrCreate()

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val resp = readTemperatures(temperaturesFile).join(readStations(stationsFile))
      .map { r =>
        (LocalDate.of(year, r._2._1.month, r._2._1.day), r._2._2.location, (r._2._1.temp - 32)/1.8)
      }
    resp.collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(loc => loc._2)
      .map {
        case (loc, temp) =>
          (loc,
            (temp
              .map(m => m._3).sum / temp.size)
          )
      }
  }

  def read[T:ClassTag](resource: String, f: Array[String] => T)= {
    val rdd = spark.sparkContext.textFile(fsPath(resource))
    rdd.map(line => {
      val arr =  line.split(",", -1)
      f(arr)
    })
  }

  def readStations(resource: String) = {
    read[Station](resource, composeStation).filter(s => s.location != Location(9999.9,9999.9))
      .map(station => ((station.stn, station.wban), station))
  }

  def readTemperatures(resource: String) = {
    read[Temperature](resource, composeTemperature).filter(t => t.temp != 9999.9)
      .map(temp => ((temp.stn, temp.wban), temp))
  }

  def composeStation(arr: Array[String]): Station = {
    Station(
      stn = arr(0),
      wban = arr(1),
      location = if(arr(2) != "" || arr(3) != "")
        Location(arr(2).toDouble, arr(3).toDouble) else Location(9999.9,9999.9)
    )
  }

  def composeTemperature(arr: Array[String]): Temperature = {
    Temperature(
      stn = arr(0),
      wban = arr(1),
      month = arr(2).toInt,
      day = arr(3).toInt,
      temp = if (arr(4) != "") arr(4).toDouble else 9999.9
    )
  }



  def fsPath(resource: String): String = {
    val path = Paths.get(
      getClass.getResource(resource).toURI
    ).toString
    path
  }


  def stationSchema(): StructType =
    StructType(
      List(
        StructField("STN", StringType, nullable = true),
        StructField("WBAN", StringType, nullable = true),
        StructField("LAT", DoubleType, nullable = true),
        StructField("LON", DoubleType, nullable = true)
      )
    )
}
