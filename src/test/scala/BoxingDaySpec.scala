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
    val str = classOf[BoxingDaySpec].getResourceAsStream("groups.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    grid.foreach(e => println(e.mkString(",")))
    val points = BoxingDay.gridToPoints(grid)
    val result = BoxingDay.getAllSetsAdjacentPoints(points)
    result.foreach( e => println(e))
    assert(result.size == 3, "Failed to detect all groups of adjacent points")
  }

  it should "be able to determine if two bounding boxes are overlapping" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("groups.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    grid.foreach(e => println(e.mkString(",")))
    val points = BoxingDay.gridToPoints(grid)
    val groups = BoxingDay.getAllSetsAdjacentPoints(points)
    assert(groups.size == 3, "Failed to detect all groups of adjacent points")
    val boxes = groups.map( e => BoxingDay.pointsToBoundingBox(e.toSet))
    assert(boxes.size == 3, "Failed to compute bounding boxes")
    // group 1 should overlap with group 2 but not group 0
    assert(boxes(1).overlapsWith(boxes(2)), "Group 1 should overlap Group 2")
    assert(!boxes(1).overlapsWith(boxes(0)), "Group 1 should not overlap Group 0")
    // group 0 should not overlap with either
    assert(!boxes(0).overlapsWith(boxes(1)), "Group 0 should not overlap Group 1")
    assert(!boxes(0).overlapsWith(boxes(2)), "Group 0 should not overlap Group 1")
  }

  it should "be able to determine if a given bounding box overlaps with a list of boxes" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("groups.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    grid.foreach(e => println(e.mkString(",")))
    val points = BoxingDay.gridToPoints(grid)
    val groups = BoxingDay.getAllSetsAdjacentPoints(points)
    val boxes = groups.map( e => BoxingDay.pointsToBoundingBox(e.toSet))
    println(boxes)
    assert(!boxes(0).overlapsWith(boxes(1)), "Box 0 should not overlap")
    assert(boxes(1).overlapsWith(boxes(2)), "Box 1 should overlap with Box 2")
    assert(!boxes(1).overlapsWith(boxes(0)), "Box 1 should not overlap Box 0")
  }

  it should "be able to find the largest bounding box with no overlaps" in {
    val str = classOf[BoxingDaySpec].getResourceAsStream("groups.txt")
    val grid = BoxingDay.processSpecfile(Source.fromInputStream(str))
    grid.foreach(e => println(e.mkString(",")))
    val points = BoxingDay.gridToPoints(grid)
    val groups = BoxingDay.getAllSetsAdjacentPoints(points)
    val boxes = groups.map( e => BoxingDay.pointsToBoundingBox(e.toSet))
    val result = BoxingDay.findLargestBoundingBox(boxes)
    assert(result.isDefined, "No bounding box found which did not overlap")
    println(result)
  }
}
