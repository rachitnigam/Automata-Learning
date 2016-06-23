package learner

class Learner(teacher: Teacher) {
  private def makeObsTable(ps: Seq[String], ss: Seq[String]) =  
    new ObsTable(teacher.letters, teacher.isMember,ps,ss)
  
  private def allPreOf(str: String): Seq[String] = {
    def helper(str: String): Seq[String] = str match {
      case "" => Seq("")
      case s => Seq(s) union helper(s.dropRight(1))
    }
    helper(str)
  }

  def generateHypo(obs: ObsTable): (Hypothesis, ObsTable) = {
    if(obs.isClosed && obs.isConsistent){
      println(s"This table is closed and consistent:\n$obs\n")
      (obs.generateHypothesis,obs)
    }
    else if(obs.isClosed == false) {
      println(s"This table is not closed...\n$obs\n")
      val u: String = obs.findUnclosed.get
      val preStrings: List[String] = obs.preStrings :+ u
      val newObs = makeObsTable(preStrings, obs.sufStrings)
      generateHypo(newObs)
    }
    else if(obs.isConsistent == false) {
      println(s"This table is not inconsistent...\n$obs\n")
      val i: String = obs.findInconsistent.get
      val ss: List[String] = obs.sufStrings :+ i
      val newObs = makeObsTable(obs.preStrings, ss)
      generateHypo(newObs)
    }
    else { throw new Exception("This can't happen") }
  }

  def learn(): Hypothesis = {
    def loop(obs: ObsTable): Hypothesis = {
      val (h,nobs) = generateHypo(obs)
      println(s"Is this the correct hypothesis: \n$h\n")
      val counter = teacher.isTrueHypothesis(h)
      if(counter.isDefined){
        println(s"This is not the correct, ${counter.get} is the counterexample")
        val ps: List[String] = (nobs.preStrings union allPreOf(counter.get)).toList
        loop(makeObsTable(ps,nobs.sufStrings))
      }
      else{
        println("This is the correct hypothesis")
        h
      }
    }
    loop(makeObsTable(Seq(""),Seq("")))
  }
}
