@main def entrypoint: Unit =
  val result = run(FileLoader.readFile("input.txt"))
  println(result)

def run(input: List[String]): Int =
  input.map(_.toInt).sliding(2).foldLeft(0) { case (acc, a :: b :: Nil) =>
    if b > a then acc + 1 else acc
  }
