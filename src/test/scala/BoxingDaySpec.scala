import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class BoxingDaySpec extends AnyFlatSpec {

  "A BoxingDay" should "process a file into a grid" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    assert(grid.size == 4, "Not all lines were processed")
    val width = grid(0).size
    grid.foreach( e => assert(e.size == width, "Not all lines are the same length"))
  }
}
