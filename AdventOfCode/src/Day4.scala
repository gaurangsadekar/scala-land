
class RoomSecurity {
  case class Room(encryptedName: Array[String], sectorId: Int, checksum: String) {
    def isReal: Boolean = {
      val computedChecksum = encryptedName
        .reduce(_ + _)
        .foldLeft(Map.empty[Char, Int]){ (map, ch) =>
          map + (ch -> (map.getOrElse(ch, 0) + 1)) }
        .foldLeft(Map.empty[Int, List[Char]]){ case (map, (ch, count)) =>
            map + (count -> (ch :: map.getOrElse(count, Nil)))}
        .toSeq
        .sortBy(-_._1)
        .flatMap(_._2.sorted)
        .take(5)
        .mkString("")
      computedChecksum == checksum
    }

    def decryptName = {
      def caesar(word: String) = {
        word.map { ch =>
          val i = ch - 'a'
          val res = (i + this.sectorId) % 26
          (res + 'a').toChar
        }
      }
      encryptedName.map(caesar(_)).mkString(" ")
    }

  }
  def processRoom(roomStr: String) = {
    val (encrypted, checksum) = roomStr.splitAt(roomStr.indexOf("["))
    val arr = encrypted.split("-")
    Room(arr.dropRight(1),
      arr.last.toInt,
      checksum.substring(1, checksum.length - 1)
    )
  }

  def sumSectorId(input: List[String]) = {
    input.map(processRoom)
      .flatMap{ r => if (r.isReal) Some(r.sectorId) else None }
      .sum
  }

  def sectorIdNorthPole(input: List[String]) = {
    input.map(processRoom)
      .filter(_.decryptName.startsWith("northpole"))
      .head
      .sectorId
  }
}

object Day4 {
  def apply(input: List[String]) = {
    println("Part 1:")
    val rs = new RoomSecurity()
    println(rs.sumSectorId(input))
    println(rs.sectorIdNorthPole(input))
  }
}
