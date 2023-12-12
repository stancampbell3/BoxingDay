import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class BoxingDaySpec extends AnyFlatSpec {

  "A BoxingDay" should "process a file into a grid" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    assert(grid.size == 4, "Not all lines were processed")
    val width = grid(0).size
    grid.foreach( e => assert(e.size == width, "Not all lines are the same length"))

    BoxingDay.printGrid(grid)
  }

  it should "be able to convert to a List of Points" in {

    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    val result = BoxingDay.gridToPoints(grid)
    assert(result.size == 4, "Incorrect points read")

    result.foreach( p => println(p))
  }

  it should "be able to detect adjacent points" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    val result = BoxingDay.gridToPoints(grid)

    // Now, find adjacent points
  }
}
