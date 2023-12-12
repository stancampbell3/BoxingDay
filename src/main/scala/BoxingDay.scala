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
  def overlapsWith(other:BoundingBox):Boolean = ???
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
}

class BoxingDay {

}
