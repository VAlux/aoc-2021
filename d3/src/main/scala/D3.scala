import scala.util.chaining.*
import scala.annotation.tailrec

case class DiagnosticData(gamma: String, epsilon: String)

@main def entrypoint: Unit =
  val input = FileLoader.readFile("input_test.txt")
  val resultPart1 = run(input)
  println(resultPart1)

def run(input: List[String]): Int =
  val transposedData = transpose(input)
  // transposedData.map(calcDiagnosticData).pipe(reduceDiagnosticData).pipe(calcDiagnosticResult)
  transposedData.map(calcDiagnosticData).pipe(data => locateLifeSupport(input, data)).pipe(calcDiagnosticResult)

def calcDiagnosticResult(data: DiagnosticData): Int =
  Integer.parseInt(data.gamma, 2) * Integer.parseInt(data.epsilon, 2)

def reduceDiagnosticData(data: List[DiagnosticData]): DiagnosticData =
  data.reduce((a, b) => DiagnosticData(a.gamma + b.gamma, a.epsilon + b.epsilon))

def locateLifeSupport(input: List[String], data: List[DiagnosticData]): DiagnosticData =
  DiagnosticData(locate(input, data, d => d.gamma), locate(input, data, d => d.epsilon))

def locate(input: List[String], data: List[DiagnosticData], locator: DiagnosticData => String): String =
  @tailrec
  def filterInput(filteredInput: List[String], index: Int = 0): String = filteredInput match
    case Nil      => ""
    case x :: Nil => x
    case _ =>
      filterInput(filteredInput.filter(a => a.charAt(index) == locator(data(index)).charAt(index)), index + 1)

  filterInput(input)

def calcDiagnosticData(data: String): DiagnosticData =
  val oneCount = data.count(_ == '1')
  val zeroCount = data.length - oneCount
  if oneCount >= zeroCount then DiagnosticData("1", "0") else DiagnosticData("0", "1")

def transpose(input: List[String]): List[String] =
  def go(a: List[List[Char]]): List[List[Char]] = a.filter(_.nonEmpty) match
    case Nil => Nil
    case xs  => xs.map(_.head) :: go(xs.map(_.tail))

  go(input.map(_.toCharArray.toList)).map(_.mkString)
