import scala.io.Source

case class Point(x:Int, y:Int) {
  def adjacent(other:Point):Boolean = {
    // Adjacent without considering diagonals
    (other == this) ||
    (other.x == this.x && other.y == this.y - 1) ||
      (other.x == this.x && other.y == this.y + 1) ||
      (other.y == this.y && other.x == this.x - 1 ) ||
      (other.y == this.y && other.x == this.x + 1 )
  }
}

case class BoundingBox(topLeft:Point, topRight:Point, bottomLeft:Point, bottomRight:Point ) {
  def overlapsWith(other:BoundingBox):Boolean = {
    println(this + "\n" + other)

    /**
     * if (A.x1 < B.x2 &&
     * A.x2 > B.x1 &&
     * A.y1 < B.y2 &&
     * A.y2 > B.y1) {
     * then A and B intersect
     * }
     */
    topLeft.x <= other.bottomRight.x &&
      bottomRight.x > other.topLeft.x &&
      topLeft.y < other.bottomRight.y &&
      bottomRight.y > other.topLeft.y
  }

  def hasAnOverlapWith(boxes:List[BoundingBox]):Boolean = {
    boxes.collectFirst(e => e.overlapsWith(this)).isDefined
  }

  def size = (bottomRight.x - topLeft.x) * (bottomRight.y - topLeft.y)
}

object BoxingDay {
  type Grid = Array[Array[Char]]

  def processSpecfile(path:String):Grid = BoxingDay.processSpecfile(Source.fromFile(path))
  def processSpecfile(source:Source):Grid = {
    source.getLines().toArray.map( e => e.toCharArray)
  }

  def printGrid(grid:Grid):Unit = {
    grid.foreach( line => {
      line.foreach( print(_))
      println
    })
  }

  def gridToPoints(grid:Grid):List[Point] = {

    val width = grid(0).size
    grid.indices.flatMap(i => {
      (0 until width).map(j => {
        grid(i)(j) match {
          case '*' => Some(Point(i, j))
          case _ => None
        }
      }).filter(_.isDefined).map(_.get)
    }).toList
  }

  def allAdjacentPoints(point:Point,adjacent:Set[Point], allPoints:List[Point]):Set[Point] = {
    val immedAdj = allPoints.filter( _.adjacent(point)).toSet
    val onDeck = (immedAdj.diff(adjacent))
    if(onDeck.size > 0) {
      immedAdj ++ onDeck.flatMap( e => allAdjacentPoints(e, adjacent ++ immedAdj, allPoints))
    } else {
      Set.empty[Point]
    }
  }

  def pointsToBoundingBox(points:Set[Point]):BoundingBox = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    val xmin = xs.min
    val xmax = xs.max
    val ymin = ys.min
    val ymax = ys.max

    BoundingBox(Point(xmin, ymin), Point(xmax,ymin), Point(xmin,ymax), Point(xmax,ymax))
  }

  def getAllSetsAdjacentPoints(points:List[Point]):List[List[Point]] = {
    if(points.size > 0) {
      val adjacent = allAdjacentPoints(points.head, Set.empty[Point], points).toList
      val other = getAllSetsAdjacentPoints(points.diff(adjacent))
      List(adjacent) ++ other
    } else {
      List.empty[List[Point]]
    }
  }

  def findLargestBoundingBox(boxes:List[BoundingBox]):Option[BoundingBox] = {
    // find all the non-overlapping boxes
    boxes.flatMap( box => {
      boxes.diff(List(box).filter(_.overlapsWith(box)))
    }).sortBy(_.size).headOption
  }

  // Main
  def main(args:Array[String]):Int = {
    // Read standard in as a stream
    val src = Source.fromInputStream(System.in)
    // Process the specfile
    val grid = BoxingDay.processSpecfile(src)
    // Extract the points
    val points = BoxingDay.gridToPoints(grid)
    // Find the largest bounding box, if present
    val groups = BoxingDay.getAllSetsAdjacentPoints(points)
    val boxes = groups.map( e => BoxingDay.pointsToBoundingBox(e.toSet))
    val result = BoxingDay.findLargestBoundingBox(boxes)
    if(result.isDefined) {
      val box = result.get
      println(s"""(${box.topLeft.x+1},${box.topLeft.y+1}),(${box.bottomRight.x+1},${box.bottomRight.y+1})""")
      0
    } else {
      -1
    }
  }
}