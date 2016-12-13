import scala.annotation.tailrec

class ToiletCode {
  case class Position(row: Int, col: Int)
  def evalRow(rowStr: String, prevPos: Position) = {
    def nextPosition(pos: Position, move: Char) = {
      def valid(p: Int, old: Int) = if (0 <= p && p < 3) p else old
      move match {
        case 'R' => Position(pos.row, valid(pos.col + 1, pos.col))
        case 'L' => Position(pos.row, valid(pos.col - 1, pos.col))
        case 'U' => Position(valid(pos.row - 1, pos.row), pos.col)
        case 'D' => Position(valid(pos.row + 1, pos.row), pos.col)
      }
    }
    val row = rowStr.toCharArray()
    row.foldLeft(prevPos)(nextPosition)
  }

  def keyCode(rows: List[String]) = {
    def numPadEntry(pos: Position) = pos.row * 3 + pos.col + 1
    @tailrec
    def getKeyCode(rows: List[String], pos: Position, code: Int): Int = rows match {
      case Nil => code
      case row :: rs => {
        val posAfterRow = evalRow(row, pos)
        val key = numPadEntry(posAfterRow)
        getKeyCode(rs, posAfterRow, code * 10 + key)
      }
    }
    getKeyCode(rows, Position(1, 1), 0)
  }
}


object Day2 {
  def apply(inputList: List[String]) {
    val tc = new ToiletCode()
    println(inputList.length)
    println(tc.keyCode(inputList))
  }
}
