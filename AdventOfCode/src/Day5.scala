import java.security.MessageDigest

import scala.annotation.tailrec

class DoorPassword {
  def getPassword(input: String) = {
    def getNextKey(candidate: String): Option[Char] = {
      val hexString = MessageDigest
        .getInstance("MD5")
        .digest((input + candidate).getBytes)
        .take(3)
        .map(_.formatted("%02x"))
        .reduceLeft(_ + _)
      if (hexString.startsWith("00000"))
        Some(hexString.charAt(5))
      else None
    }

    @tailrec
    def genPassword(password: String, idx: Int): String = {
      if (password.length < 8) {
        val nextKey: Option[Char] = getNextKey(idx.toString)
        val newPass = if (nextKey.nonEmpty) password + nextKey.get else password
        genPassword(newPass, idx + 1)
      }
      else password
    }
    genPassword("", 0)
  }
}

object Day5 extends App {
  val input = "wtnhxymk"
  val dp = new DoorPassword()
  println(dp.getPassword(input))
}
