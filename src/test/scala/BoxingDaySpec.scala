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

  it should "be able to detect simply adjacent points" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    val result = BoxingDay.gridToPoints(grid)

    // Now, find adjacent(simple directly) points
    assert(result(0).adjacent(result(1)))
    assert(!result(0).adjacent(result(3)))
  }

  it should "be able to detect transitively adjacent points" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    val points = BoxingDay.gridToPoints(grid)

    // transitively adjacent points
    val result = BoxingDay.allAdjacentPoints(points(0), Set.empty[Point], points)
    println(result)
    assert(result.size == 4, "didn't detect all adjacent points")
  }

  it should "be able to find a bounding box around a set of points" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile1.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    val points = BoxingDay.gridToPoints(grid)
    val adjacents = BoxingDay.allAdjacentPoints(points(0), Set.empty[Point], points)
    val box = BoxingDay.pointsToBoundingBox(adjacents)
    println(box)
    assert(box.topLeft == Point(1,1))
    assert(box.bottomRight == Point(2,2))
  }

  it should "be able to find all the sets of contiguous points" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("specfile5.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    grid.foreach(e => println(e.mkString(",")))
    val points = BoxingDay.gridToPoints(grid)
    val result = BoxingDay.getAllSetsAdjacentPoints(points)
    result.foreach( e => println(e))
    assert(result.size == 2, "Failed to detect all groups of adjacent points")
  }
}
