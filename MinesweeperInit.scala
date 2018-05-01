import util.Random

object MinesweeperInit {
    def getPositions(length: Int, width: Int, numMines: Int): Seq[(Int, Int)] = {
        val range: Seq[Int] = 0 until length * width
        val mineSpots = Random.shuffle(range).take(numMines)
        mineSpots.map(spot => (spot / length, spot % width))
    }
}
