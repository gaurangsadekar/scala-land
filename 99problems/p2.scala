object P2 {
    def penultimateOfList(list: List[Int]): Int = {
        list match {
            case Nil => throw new NoSuchElementException("not enough elements in the list") 
            case List(x) => throw new NoSuchElementException("not enough elements in the list") 
            case List(fst, snd) => fst
            case _ => penultimateOfList(list.tail)
        }
    }

    def apply() = {
        println(penultimateOfList(List(1,2)))
        println(penultimateOfList(List(1,2,3,4)))
        println(penultimateOfList(List(1)))
        println(penultimateOfList(Nil))
    }
}
