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
    buf++= s"The final states are: ${fStates.mkString(",")}"
    buf.toString
  }

  override def toString() = generateTable
}
