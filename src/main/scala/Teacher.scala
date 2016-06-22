package learner

abstract class Teacher {
  def isMember: String => Boolean
  def isTrueHypothesis: Hypothesis => Option[String]
}
