package learner

case class ExampleTeacher() extends Teacher(Seq("1","0")) {
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

case class RegexTeacher(reg: String) extends Teacher(Seq("a","b")) {
  def isMember(str: String): Boolean = str matches reg

  def isTrueHypothesis(h: Hypothesis): Option[String] = {
    val c: String = scala.io.StdIn.readLine
    if(c.trim == "??") None
    else Some(c)
  }
}

object Simple{
 
  def main(args: Array[String]) = {
    val t1 = new RegexTeacher("ab")
    val t2 = new ExampleTeacher()
    val l = new Learner(t1)
    println(l.learn)
  }

}
