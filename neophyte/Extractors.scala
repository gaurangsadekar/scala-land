trait User {
    def name: String
    def score: Int
}
/*
    different unapply signatures
    unapply(object: S): Option[T] // if only one value has to be extracted
    unapply(object: S): Option[(T1, ...., Tn)] // multiple values are to be extracted
    unapply(object: S): Boolean
*/
class FreeUser(val name: String, val score: Int, val upgradeProb: Double) extends User
object FreeUser {
    def unapply(user: FreeUser): Option[(String, Int, Double)] =
        Some((user.name, user.score, user.upgradeProb))
}

class PremiumUser(val name: String, val score: Int) extends User
object PremiumUser {
    def unapply(user: PremiumUser): Option[(String, Int)] =
        Some((user.name, user.score))
}

// boolean extractors are not used as often as the others, knowledge helps
object premiumCandidate {
    def unapply(user: FreeUser): Boolean = user.upgradeProb >= 0.75
}

/*
there are other kinds of extractors such as list and stream extractors.
There are typically written in infix notation to resemble the actual operator
All extractors can be used in infix, but that is discouraged
*/
