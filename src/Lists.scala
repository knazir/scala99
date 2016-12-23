import scala.util.Try
import scala.util.control.Breaks._

object Lists {

  /* List helper methods */

  def getKValue(list: List[Any]) : Int = {
    println("Enter value for k:")
    var k: Try[Int] = null
    breakable {
      while (true) {
        k = Try(scala.io.StdIn.readLine().toInt)
        if (k.get >= 0 && k.get < list.length) break
        println(s"Please enter a valid integer between 0 and ${list.length}")
      }
    }
    k.get
  }

  /* Problems */

  def P1(list: List[Any]) : Any = {
    list.last
  }

  def P2(list: List[Any]) : Any = {
    list.init.last
  }

  def P3(list: List[Any]) : Any = {
    val k = getKValue(list)
    list(k)
  }

  def P4(list: List[Any]) : Int = {
    list.length
  }

  def P5(list: List[Any]) : List[Any] = {
    list.reverse
  }

  def P6(list: List[Any]) : Boolean = {
    list == list.reverse
  }

  // TODO: Add capabilities for inputting nested list structures
  def P7(list: List[Any]) : List[Any] = list flatMap {
    case listElement: List[_] => P7(listElement)
    case flatElement => List(flatElement)
  }

  def P8(list: List[Any]): Any = list.foldLeft(List[Any]()) {
    case (l, e) if l.isEmpty || l.last != e => l:::List(e)
    case (l, e) => l
  }
}
