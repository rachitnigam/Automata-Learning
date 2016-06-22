package learner

case class ExampleTeacher(l: Seq[String]) extends Teacher(l) {
  def isMember(str: String): Boolean = {
    val lst = str.toList
    (lst.count(_ == '1') % 2 == 0) && (lst.count(_ == '0') % 2 == 0)
  }

  def isTrueHypothesis(h: Hypothesis): Option[String] = {
    val c: String = scala.io.StdIn.readLine
    if(c.trim == "??") None
    else Some(c)
  }
}

case class RegexTeacher(l: Seq[String]) extends Teacher(l) {
  def isMember(str: String): Boolean = str matches """ab*""" 

  def isTrueHypothesis(h: Hypothesis): Option[String] = {
    val c: String = scala.io.StdIn.readLine
    if(c.trim == "??") None
    else Some(c)
  }
}

object Simple{
 
  def main(args: Array[String]) = {
    val t = new ExampleTeacher(Seq("0","1"))
    val l = new Learner(t)
    println(l.learn)
  }

}
