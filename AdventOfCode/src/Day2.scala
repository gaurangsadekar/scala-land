
import scala.annotation.tailrec

case class Position(row: Int, col: Int)
class ToiletCode {

  def keyCode(rows: List[String], keypad: Array[Array[String]], initPos: Position) = {
    def evalRow(rowStr: String, prevPos: Position) = {
      def nextPosition(pos: Position, move: Char) = {
        def valid(p: Position) = {
          // sensitive to keypad
          val len = keypad.length
          val inBounds = (x: Int) => 0 <= x && x < len
          if (inBounds(p.row) && inBounds(p.col) && numPadEntry(p) != "0")
            p
          else pos
        }
        move match {
          case 'R' => valid(Position(pos.row, pos.col + 1))
          case 'L' => valid(Position(pos.row, pos.col - 1))
          case 'U' => valid(Position(pos.row - 1, pos.col))
          case 'D' => valid(Position(pos.row + 1, pos.col))
        }
      }
      val row = rowStr.toCharArray()
      row.foldLeft(prevPos)(nextPosition)
    }

    def numPadEntry(pos: Position) = keypad(pos.row)(pos.col)

    @tailrec
    def getKeyCode(rows: List[String], pos: Position, code: String): String = rows match {
      case Nil => code
      case row :: rs => {
        val posAfterRow = evalRow(row, pos)
        val key = numPadEntry(posAfterRow)
        getKeyCode(rs, posAfterRow, code + key)
      }
    }
    getKeyCode(rows, initPos, "")
  }
}

object Day2 {
  def apply(inputList: List[String]) {
    val tc = new ToiletCode()
    val keypad1 = Array(Array("1","2","3"), Array("4","5","6"), Array("7","8","9"))
    val keypad2 = Array(
      Array("0", "0", "1", "0", "0"),
      Array("0","2", "3", "4", "0"),
      Array("5", "6", "7","8","9"),
      Array("0", "A", "B","C", "0"),
      Array("0", "0", "D", "0", "0")
    )
    println("Part 1 keycode:", tc.keyCode(inputList, keypad1, Position(1,1)))
    println("Part 2 keycode:", tc.keyCode(inputList, keypad2, Position(2, 0)))
  }
}
