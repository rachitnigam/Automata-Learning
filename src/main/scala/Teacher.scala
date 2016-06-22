package learner

abstract class Teacher(l: Seq[String]) {
  def letters = l
  def isMember(str: String): Boolean
  def isTrueHypothesis(h: Hypothesis): Option[String]
}
