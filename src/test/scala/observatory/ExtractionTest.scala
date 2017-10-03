package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
trait ExtractionTest extends FunSuite {


  test("composeStation") {

    val values = List("010013,,,",
      "724017,03707,+37.358,-078.438",
      "724017,,+37.350,-078.433")
    val stations = values.map(
      v =>  Extraction.composeStation(v.split(",", -1))
    )

    assert(stations(0).stn === "010013")
    assert(stations(0).wban === "")
    assert(stations(0).location === Location(9999.9, 9999.9))

    assert(stations(1).stn === "724017")
    assert(stations(1).wban === "03707")
    assert(stations(1).location === Location(37.358, -078.438))

  }

  test("composeTemperature") {
    val values = List("010013,,11,25,39.2",
      "724017,,08,11,81.14",
      "724017,03707,12,06,32",
      "724017,03707,01,29,35.6",
      "724017,03707,01,29,9999.9",
      "724017,03707,01,29,"
    )

    val temps = values.map(
      v => Extraction.composeTemperature(v.split(",", -1))
    )

    assert(temps(0).stn === "010013")
    assert(temps(0).wban === "")
    assert(temps(0).month === 11)
    assert(temps(0).day === 25)
    assert(temps(0).temp === 39.2)


    assert(temps(4).stn === "724017")
    assert(temps(4).wban === "03707")
    assert(temps(4).month === 1)
    assert(temps(4).day === 29)
    assert(temps(4).temp === 9999.9)

    assert(temps(5).stn === "724017")
    assert(temps(5).wban === "03707")
    assert(temps(5).month === 1)
    assert(temps(5).day === 29)
    assert(temps(5).temp === 9999.9)

  }

  test("readStationsTest") {
    val stations = Extraction.readStations("/stations-test.csv")

    assert(stations.count() === 19¡)
    val station = stations.collect()(0)

    assert(station._1._1 === "")
    assert(station._1._2 === "94908")
    assert(station._2.location === Location(42.398, -090.704))

  }

  test("readTemperaturesTest") {
    val temperatures = Extraction.readTemperatures("/2010.csv")

    assert(temperatures.count() === 47)
    val temp = temperatures.collect()(0)

    assert(temp._1._1 === "008268")
    assert(temp._1._2=== "")
    assert(temp._2.month === 5)
    assert(temp._2.day === 20)
    assert(temp._2.temp === 91.8)

  }

  test("locateTemperatures") {
    val temperatures = Extraction.locateTemperatures(2010, "/stations-test.csv", "/2010.csv")

    val temp = temperatures.filter(t => t._2 === Location(43.350, -073.617)).toList.head

    assert(temp._2 === Location(43.350, -073.617))
    assert(temp._1 === LocalDate.of(2010, 5, 20))
  }

  test("locationYearlyAverageRecords") {
    val temperatures = Extraction.locateTemperatures(2010, "/stations-test.csv", "/2010.csv")

    val temp = Extraction.locationYearlyAverageRecords(temperatures)
      .filter(t => t._1 === Location(43.350, -073.617)).toList.head

    assert(Math.floor(temp._2) === Math.floor((80.13333 - 32)/1.8))
  }

  test("fullWeek1Test") {
    val expectedList = List(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )

    val expectedAvgTemps = List(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )

  }


}