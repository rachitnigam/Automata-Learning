package learner

class Learner(letters: Seq[String], teacher: Teacher) {
  private def makeObsTable(ps: Seq[String], ss: Seq[String]) =  
    new ObsTable(letters, teacher.isMember,ps,ss)
  
  private def allPreOf(str: String): Seq[String] = {
    def helper(str: String): Seq[String] = str match {
      case "" => Seq("")
      case s => Seq(s) union helper(s.tail)
    }
    helper(str)
  }

  def generateHypo(obs: ObsTable): Hypothesis = {
    if(obs.isClosed && obs.isConsistent) 
      obs.generateHypothesis
    if(obs.isClosed == false) {
      val u: String = obs.findUnclosed.get
      val preStrings: Seq[String] = obs.preStrings union allPreOf(u)
      generateHypo(makeObsTable(preStrings, obs.sufStrings))
    }
    else {
      val i: String = obs.findInconsistent.get
      val ss: Seq[String] = obs.sufStrings :+ i
      generateHypo(makeObsTable(obs.preStrings, ss))
    }
  }
}
