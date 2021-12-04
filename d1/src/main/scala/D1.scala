@main def entrypoint: Unit =
  val input = FileLoader.readFile("input.txt")
  val resultPart1 = run(input, 2, 1)
  val resultPart2 = run(input, 4, 3)
  println(s"Part 1: $resultPart1")
  println(s"Part 2: $resultPart2")

def run(input: List[String], windowSize: Int, subWindowSize: Int): Int =
  input.map(_.toInt).sliding(windowSize).foldLeft(0)((acc, window) => acc + delta(window, subWindowSize))

def delta(window: List[Int], subWindowSize: Int): Int =
  window.sliding(subWindowSize).map(_.sum).toList match
    case a :: b :: Nil if a < b => 1
    case _                      => 0
