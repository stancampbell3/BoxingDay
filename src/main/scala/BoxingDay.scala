import scala.io.Source

case class Point(val x:Int, val y:Int) {
  def adjacent(other:Point):Boolean = {
    // Adjacent without considering diagonals
    (other == this) ||
    (other.x == this.x && other.y == this.y - 1) ||
      (other.x == this.x && other.y == this.y + 1) ||
      (other.y == this.y && other.x == this.x - 1 ) ||
      (other.y == this.y && other.x == this.x + 1 )
  }
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
    (0 until grid.size).map( i => {
      (0 until width).map( j => {
        grid(i)(j) match {
          case '*' => Some(Point(i,j))
          case _ => None
        }
      }).filter(_.isDefined).map(_.get)
    }).flatten.toList
  }
}

class BoxingDay {

}
