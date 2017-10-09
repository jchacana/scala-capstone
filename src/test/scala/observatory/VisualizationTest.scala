package observatory


import observatory.Visualization.Milestone2Visualizer
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


  test("linearInterpolation") {
    assert(Visualization.interpolation(Some((0, Color(0, 0, 0, 255))), Some((100, Color(255, 255, 255, 255))), 50) === Color(128, 128, 128, 255))
    assert(Visualization.interpolation(Some((0, Color(0, 0, 0, 255))), Some((80, Color(255, 255, 255, 255))), 10) === Color(32, 32, 32, 255))
    assert(Visualization.interpolation(Some((0, Color(255, 128, 0, 255))), Some((80, Color(0, 128, 255, 255))), 10) === Color(223, 128, 32, 255))
  }

  test("xy to Location") {
    val visualizer = new Milestone2Visualizer()
    assert(Location(90,-180) === visualizer.xyToLocation(0,0))
    assert(Location(0,0) === visualizer.xyToLocation(180, 90))
  }


  test("interpolateColor") {
    val palette = List(
      (100.0, Color(255, 255, 255, 255)),
      (50.0, Color(0, 0, 0, 255)),
      (0.0, Color(255, 0, 128, 255))
    )

    assert(Visualization.interpolateColor(palette, 50.0) === Color(0, 0, 0, 255))
    assert(Visualization.interpolateColor(palette, 0.0) === Color(255, 0, 128, 255))
    assert(Visualization.interpolateColor(palette, -10.0) === Color(255, 0, 128, 255))
    assert(Visualization.interpolateColor(palette, 200.0) === Color(255, 255, 255, 255))
    assert(Visualization.interpolateColor(palette, 75.0) === Color(128, 128, 128, 255))
    assert(Visualization.interpolateColor(palette, 25.0) === Color(128, 0, 64, 255))
  }

  test("posToLocation") {
    val visualizer = new Milestone2Visualizer()
    assert(Location(90.0, -180.0) === visualizer.indexToLocation(360,180, 0))
    assert(Location(0.0,0.0) === visualizer.indexToLocation(360,180, 32580))
    assert(Location(-89.0, 179.0) === visualizer.indexToLocation(360,180, 64799))
    assert(Location(90.0,0.0) === visualizer.indexToLocation(720,360, 360))
    assert(Location(0.0,0.0) === visualizer.indexToLocation(720,360, 129960))
  }

}
