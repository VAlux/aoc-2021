import scala.util.chaining.*

case class DiagnosticData(gamma: String, epsilon: String)

@main def entrypoint: Unit =
  val input = FileLoader.readFile("input.txt")
  val resultPart1 = run(input)
  println(resultPart1)

def run(input: List[String]): Int =
  transpose(input).map(calcDiagnosticData).pipe(reduceDiagnosticData).pipe(calcDiagnosticResult)

def calcDiagnosticResult(data: DiagnosticData): Int =
  Integer.parseInt(data.gamma, 2) * Integer.parseInt(data.epsilon, 2)

def reduceDiagnosticData(data: List[DiagnosticData]): DiagnosticData =
  data.reduce((a, b) => DiagnosticData(a.gamma + b.gamma, a.epsilon + b.epsilon))

def calcDiagnosticData(data: String): DiagnosticData =
  val oneCount = data.count(_ == '1')
  val zeroCount = data.length - oneCount
  if oneCount > zeroCount then DiagnosticData("1", "0") else DiagnosticData("0", "1")

def transpose(input: List[String]): List[String] =
  def go(a: List[List[Char]]): List[List[Char]] = a.filter(_.nonEmpty) match
    case Nil => Nil
    case xs  => xs.map(_.head) :: go(xs.map(_.tail))

  go(input.map(_.toCharArray.toList)).map(_.mkString)
