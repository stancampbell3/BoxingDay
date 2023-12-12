import scala.io.Source

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
}

class BoxingDay {

}
