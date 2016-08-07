object P1 {
    def lastElementOfList(list: List[Int]): Int = {
        if (list.tail.isEmpty) list.head
        else lastElementOfList(list.tail)
    }

    def apply() = {
        val l1 = List(1,2,3,4)
        println(lastElementOfList(l1))
        val l2 = List(1)
        println(lastElementOfList(l2))
    }
}
