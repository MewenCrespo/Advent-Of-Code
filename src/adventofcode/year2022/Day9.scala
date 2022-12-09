package adventofcode.year2022

object Day9 extends App {

  val INPUT_TEST_1 = """R 4
            |U 4
            |L 3
            |D 1
            |R 4
            |D 1
            |L 5
            |R 2""".stripMargin

  val INPUT_TEST_2 = """R 5
            |U 8
            |L 8
            |D 3
            |R 17
            |D 10
            |L 25
            |U 20""".stripMargin

  val INPUT = INPUT_TEST_1

  case class Vector(dx: Int, dy: Int) {
    def sign: Vector = Vector(dx.sign.toInt, dy.sign.toInt)

    def squaredNorm: Int = dx * dx + dy * dy

    override def toString: String = f"Vector($dx%d,$dy%d)"
  }

  enum Direction(dx: Int, dy: Int) extends Vector(dx, dy) {
    case Up extends Direction(0, 1)
    case Down extends Direction(0, -1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)
  }

  export Direction.* //Otherwise Left is scala.Left, same for Right

  case class Position(x: Int, y: Int) {
    def +(delta: Vector): Position = Position(x + delta.dx, y + delta.dy)

    def -(other: Position): Vector = Vector(x - other.x, y - other.y)
  }

  object Position {
    def START = Position(0, 0)
  }

  def motionIterator: Iterator[Direction] = {
    for (line <- INPUT.linesIterator) yield {
      line.strip match {
        case s"U $n" => Iterator.fill(n.toInt)(Up)
        case s"D $n" => Iterator.fill(n.toInt)(Down)
        case s"L $n" => Iterator.fill(n.toInt)(Left)
        case s"R $n" => Iterator.fill(n.toInt)(Right)
        case l => throw new IllegalArgumentException(s"Invalid motion string: '$l'")
      }
    }
  }.flatten

  def headPositionIterator: Iterator[Position] = motionIterator.scanLeft(Position.START)((pos, dir) => pos + dir)

  def nextKnot(previousNot: Iterator[Position], start: Position = Position.START): Iterator[Position] =
    previousNot.scanLeft(start)((tail, head) => {
      val delta = head - tail
      if (delta.squaredNorm > 2) tail + delta.sign else tail
    })

  // Step 1
  def secondKnotPositionIterator: Iterator[Position] = nextNot(previousNot = headPositionIterator)
  println(f"The tail of the rope visit ${secondNotPositionIterator.to(Set).size}%d position at least once")

  // Step 2
  val N = 10
  def lastKnotPositionIterator: Iterator[Position] = (3 to N).foldLeft(secondNotPositionIterator)((prev, i) => nextNot(previousNot = prev))
  println(f"The tail of the rope visit ${lastNotPositionIterator.to(Set).size}%d position at least once")
}
