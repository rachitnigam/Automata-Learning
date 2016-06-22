package learner

case class State(name: String) {
  override def toString() = name.toString
}

class Hypothesis(letters: Seq[String], states: Seq[State], iState: State, fStates: Seq[State], transition: (State,String) => State) {
  assert(states contains iState)
  assert(fStates.map(states contains _).foldLeft(true)(_ && _))

  def generateTransitionRow(state: State): Seq[State] = {
    for(a <- letters) yield transition(state,a)
  }

  def generateTable: String = {
    val buf = new StringBuilder
    buf ++= "δ  | "
    buf ++= letters.mkString("  | ")
    buf += '\n'

    for(state <- states) {
      buf ++= s"$state | "
      buf ++= generateTransitionRow(state).mkString(" | ")
      buf += '\n'
    }

    buf.toString
  }
}

object Hypo{
  def main(args: Array[String]) = {
    val q1 = new State("q1")
    val q2 = new State("q2")
    val l = Seq("a","b")
    val m: Map[(State,String),State] = Map((q1,"a")->q2, (q1,"b")->q1, (q2,"a")->q1, (q2,"b")->q2)
    def fun(st:State, str:String): State = m((st,str))

    val h = new Hypothesis(l,Seq(q1,q2),q1,Seq(q2),fun)
    println(h.generateTable)

  }
}
