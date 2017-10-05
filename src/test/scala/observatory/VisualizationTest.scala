package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
trait VisualizationTest extends FunSuite with Checkers {

  test("distance Santiago Concepcion") {
    val santiago = Location(-33.437830, -70.650449)
    val concepcion = Location(-36.820135, -73.044390)

    assert(434.0 === scala.math.floor(Visualization.gdc(santiago, concepcion)))
    assert(434.0 === scala.math.floor(Visualization.gdc(concepcion, santiago)))
  }

  test("distance Nisum Alcantara") {
    val nisum = Location(-33.4124054,-70.580630)
    val alcantara = Location(-33.414256,-70.5888841)

    assert(scala.math.round(0.8) === scala.math.round(Visualization.gdc(nisum, alcantara)))
  }


  test("xy to Location") {
    assert(Location(90,-180) === Visualization.xyToLocation(0,0))
    assert(Location(0,0) === Visualization.xyToLocation(180, 90))
  }



}
