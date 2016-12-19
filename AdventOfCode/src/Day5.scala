import java.security.MessageDigest

import scala.annotation.tailrec

class DoorPassword {
  def getPassword(input: String, naive: Boolean) = {
    def makeHexString(candidate: String) = {
      MessageDigest
        .getInstance("MD5")
        .digest((input + candidate).getBytes)
        .take(4)
        .map(_.formatted("%02x"))
        .reduceLeft(_ + _)
    }

    def getNextKey(idx: Int): Option[Char] = {
      val hexString = makeHexString(idx.toString)
      if (hexString.startsWith("00000"))
        Some(hexString.charAt(5))
      else None
    }
    @tailrec
    def genNaivePassword(password: String, idx: Int = 0): String = {
      if (password.length < 8) {
        val nextKey: Option[Char] = getNextKey(idx)
        val newPass = if (nextKey.nonEmpty) password + nextKey.get else password
        genNaivePassword(newPass, idx + 1)
      }
      else password
    }

    case class PosAndKey(pos: Int, key: Char)
    def getNextPosAndKey(idx: Int) = {
      val hexString = makeHexString(idx.toString)
      val pos = hexString.charAt(5)
      if (hexString.startsWith("00000") && pos < '8')
        Some(PosAndKey(pos - '0', hexString.charAt(6)))
      else None
    }
    @tailrec
    def genPosPassword(password: Array[Option[Char]], idx: Int = 0): Array[Option[Char]] = {
      if (!password.forall(_.isDefined)) {
        getNextPosAndKey(idx).foreach { posAndKey =>
          if (password(posAndKey.pos).isEmpty) {
            println(s"Key at index ${posAndKey.pos}: ${posAndKey.key}")
            password(posAndKey.pos) = Some(posAndKey.key)
          }
        }
        genPosPassword(password, idx + 1)
      }
      else password
    }

    if (naive) genNaivePassword("")
    else genPosPassword(Array.fill(8)(None)).flatten.mkString("")
  }
}

object Day5 extends App {
  val input = "wtnhxymk"
  val dp = new DoorPassword()
  //println(dp.getPassword(input, naive = true))
  println(dp.getPassword(input, naive = false))
}
