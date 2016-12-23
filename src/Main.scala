import scala.util.Try
import scala.util.control.Breaks._

import scala.reflect.runtime.{universe => ru}

object Main {
  /* Settings */
  val Sentinel = -1

  /* Question lists */
  val ProblemTypes = List((1, "Lists"), (2, "Arithmetic"), (3, "Binary Trees"), (4, "Multiway Trees"), (5, "Graphs"),
    (6, "Miscellaneous"))
  val ListProblems = List((1, "Last Element"), (2, "Penultimate Element"), (3, "K-th Element"), (4, "List Size"),
    (5, "Reverse List"), (6, "Is List Palindrome"), (7, "Eliminate Consecutive Duplicates"))


  /* Choosing and running problems */
  def printOptions(options: List[(Int, String)]) : Unit = {
    options.foreach(q => println(s"${q._1} - ${q._2}"))
  }

  def showGreeting() : Unit = {
    println("Welcome to 99 Scala Problems. Please choose a category:")
    printOptions(ProblemTypes)
  }

  def getUserChoice(options: List[(Int, String)]) : Int = {
    println(s"Choose a problem ($Sentinel to quit):")
    val firstQuestionNumber = options.head._1
    val lastQuestionNumber = firstQuestionNumber + options.length
    var choice: Try[Int] = null

    breakable {
      while (true) {
        val input = scala.io.StdIn.readLine()
        if (input == "") return Sentinel
        choice = Try(input.toInt)
        if (choice.isSuccess && choice.get >= firstQuestionNumber && choice.get <= lastQuestionNumber) break
        println(s"Please enter a valid numbet between $firstQuestionNumber and $lastQuestionNumber")
      }
    }
    choice.get
  }

  def runProblem(className: String, choice: Int, data: Any) : Any = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val module = m.staticModule(className.dropRight(1))
    val im = m.reflectModule(module)
    val method = im.symbol.info.decl(ru.TermName(s"P$choice")).asMethod
    val objMirror = m.reflect(im.instance)
    objMirror.reflectMethod(method)(data)
  }


  /* List problems */
  def toInferredType(element: String) : Any = {
    val integer = Try(element.toInt)
    if (integer.isSuccess) return integer.get

    val double = Try(element.toDouble)
    if (double.isSuccess) return double.get

    val boolean = Try(element.toBoolean)
    if (boolean.isSuccess) return boolean.get

    element
  }

  def createListFromInput() : List[Any] = {
    println("Enter comma-separated list. Press return when finished.")
    scala.io.StdIn.readLine().split(",").map(_.trim).toList.map(toInferredType)
  }

  def showListsProblems() : Unit = {
    println("Pick a list problem:")
    printOptions(ListProblems)

    val choice = getUserChoice(ListProblems)
    val list = createListFromInput()
    if (list.isEmpty) throw new IllegalArgumentException("List cannot be empty")
    println(s"Created ${list.toString()}")

    println(s"Result: ${runProblem(Lists.getClass.getName, choice, list)}")
  }


  /* Main method */
  def main(args: Array[String]) : Unit = {
    showGreeting()
    breakable {
      val choice = getUserChoice(ProblemTypes)

      choice match {
        case 1        => showListsProblems()
        case Sentinel => break
        case _        => println(s"Invalid choice.")
      }
    }
  }
}
