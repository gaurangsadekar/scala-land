import scala.annotation.tailrec

/*
  My solutions for Advent of Code 2016
  I am about 10 days behind, so I'll try to catch up as and when I get the time
 */

class NoTimeForTaxicab {
  type coordinate = (Int, Int)
  case class DirAndPos(facing: Int, pos: coordinate)
  val dirMultipliers = Array((0, 1), (1, 0), (0, -1), (-1, 0))

  def getNextDirection(currDir: Int, turn: String) = {
    val turnDir: Int = turn match {
      case "R" => 1
      case "L" => -1
    }
    (currDir + turnDir + 4) % 4
  }

  def getPosAfterMoving(curr: coordinate, dirMult: coordinate, step: Int) =
    (curr._1 + dirMult._1 * step, curr._2 + dirMult._2 * step)

  def getTaxiCabDist(finalPos: coordinate) =
    Math.abs(finalPos._1) + Math.abs(finalPos._2)

  def getDistance(moves: List[String]) = {
    val initPos = DirAndPos(0, (0, 0))

    val finalPos = moves.foldLeft(initPos){ (dir, move) =>
      val (turn, steps) = move.splitAt(1)
      val nowFacing = getNextDirection(dir.facing, turn)
      val newPos = getPosAfterMoving(dir.pos, dirMultipliers(nowFacing), steps.toInt)
      DirAndPos(nowFacing, newPos)
    }.pos

    getTaxiCabDist(finalPos)
  }

  /**
    * This approach is incorrect because it is only checking endpoints,
    * not all the points revisited on the path
 *
    * @param moves
    * @return the distance of the first repeated point from starting position
    * or Int.MaxValue if no repeated position is found
    */
  def getFirstReVisitedPositionWrong(moves: List[String]) = {
    @tailrec
    def getFirstRepeat(moves: List[String], dir: DirAndPos, visited: Set[coordinate]): Int = {
      moves match {
        case move :: ms => {
          val (turn, steps) = move.splitAt(1)
          val nowFacing = getNextDirection(dir.facing, turn)
          val newPos = getPosAfterMoving(dir.pos, dirMultipliers(nowFacing), steps.toInt)
          if (visited(newPos)) getTaxiCabDist(newPos)
          else getFirstRepeat(ms, DirAndPos(nowFacing, newPos), visited + newPos)
        }
        case Nil => Int.MaxValue
      }
    }
    getFirstRepeat(moves, DirAndPos(0, (0, 0)), Set.empty)
  }

  /**
    * mark a grid with visited points and find the first point crossed
    */
  type equation = (Char, Int)
  case class Segment(start: coordinate, end: coordinate, eq: equation)

  def getFirstReVisitedDistance(moves: List[String]) = {
    def getEquation(start: coordinate, end: coordinate) = {
      val (x1, y1) = start
      val (x2, y2) = end
      if (x1 == x2) ('x', x1)
      else ('y', y1)
    }

    def intersection(line1: Segment)(line2: Segment) = {
      def pointOfIntersection(eq1: equation, eq2: equation) = (eq1._1, eq2._1) match {
        case ('x', 'y') | ('y', 'x') => Some((eq1._2, eq2._2))
        case _ => None
      }
      /*
      intersection is allowed to be endpoint of line1 (but not start)
      and either start or end point of line2
      */
      pointOfIntersection(line1.eq, line2.eq).filter(p =>
        withinLimits(line1, true, p) && withinLimits(line2, false, p))
    }

    def withinLimits(segment: Segment, isFirst: Boolean, point: coordinate) = {
      val start = segment.start
      val end = segment.end
      def funcLimits(f: (coordinate) => Int) =
        if (isFirst)
          f(start) < f(point) && f(point) <= f(end)
        else
          f(start) <= f(point) && f(point) <= f(end)
      // x coord is same, compare y
      if (start._1 == end._1) funcLimits(_._2)
      // y coord is same, compare x
      else if (start._2 == end._2) funcLimits(_._1)
      else false
    }

    def manhattan(start: coordinate)(point: coordinate) =
      Math.abs(start._1 - point._1) + Math.abs(start._2 - point._2)

    @tailrec
    def getFirstRepeatPosition(moves: List[String], dir: DirAndPos, lines: List[Segment]): Option[coordinate] = {
      moves match {
        case Nil => None
        case move :: ms => {
          val (turn, steps) = move.splitAt(1)
          val start = dir.pos
          println("Move:" + move)
          print("Start:")
          println(start)
          val nowFacing = getNextDirection(dir.facing, turn)
          val end = getPosAfterMoving(start, dirMultipliers(nowFacing), steps.toInt)
          print("End:")
          println(end)
          val currLine = Segment(start, end, getEquation(start, end))
          val intersections = lines.flatMap(intersection(currLine)(_))

          if (intersections.nonEmpty)
            Option(intersections.minBy(manhattan(start)))
          else
            getFirstRepeatPosition(ms, DirAndPos(nowFacing, end), currLine :: lines)
        }
      }
    }
    getFirstRepeatPosition(moves, DirAndPos(0, (0,0)), List.empty).map { x =>
      println(x)
      getTaxiCabDist(x)
    }.getOrElse(Int.MaxValue)
  }
}

object Day1 {
  def main(args: Array[String]) {
    val input = "R4, R5, L5, L5, L3, R2, R1, R1, L5, R5, R2, L1, L3, L4, R3, L1, L1, R2, R3, R3, R1, L3, L5, R3, R1, L1, R1, R2, L1, L4, L5, R4, R2, L192, R5, L2, R53, R1, L5, R73, R5, L5, R186, L3, L2, R1, R3, L3, L3, R1, L4, L2, R3, L5, R4, R3, R1, L1, R5, R2, R1, R1, R1, R3, R2, L1, R5, R1, L5, R2, L2, L4, R3, L1, R4, L5, R4, R3, L5, L3, R4, R2, L5, L5, R2, R3, R5, R4, R2, R1, L1, L5, L2, L3, L4, L5, L4, L5, L1, R3, R4, R5, R3, L5, L4, L3, L1, L4, R2, R5, R5, R4, L2, L4, R3, R1, L2, R5, L5, R1, R1, L1, L5, L5, L2, L1, R5, R2, L4, L1, R4, R3, L3, R1, R5, L1, L4, R2, L3, R5, R3, R1, L3"
    println(input.split(",").length)
    val inputList = input.split(",").map(_.trim).toList

    val sol1 = new NoTimeForTaxicab()
    //println(sol1.getDistance(inputList))
    println(sol1.getFirstReVisitedDistance(inputList))

  }
}
