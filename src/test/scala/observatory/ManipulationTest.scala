package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
trait ManipulationTest extends FunSuite with Checkers {

  test("testGrid"){
    val temperatures:Iterable[(Location, Temperature)] = List(
      (Location(37.35, -78.433), 27.3),
      (Location(80.358, -40.438), 0.0),
      (Location(10.358, 160.438), 2.0)
    )
    val grid = Manipulation.makeGrid(temperatures)
    assert(math.round(grid(GridLocation(37, -78))) === math.round(27.3))
  }

  test("testAverage"){
    val temperatures:Iterable[Iterable[(Location, Temperature)]] = List(
      List(
        (Location(37.35, -78.433), 27.3),
        (Location(80.358, -40.438), 0.0),
        (Location(10.358, 160.438), 2.0)
      ),
      List(
        (Location(37.35, -78.433), 27.3),
        (Location(80.358, -40.438), 0.0),
        (Location(10.358, 160.438), 2.0)
      ),
      List(
        (Location(37.35, -78.433), 27.3),
        (Location(80.358, -40.438), 0.0),
        (Location(10.358, 160.438), 2.0)
      )
    )

    val avg = Manipulation.average(temperatures)

    assert(math.round(avg(GridLocation(37, -78))) === math.round(27.3))
  }

}