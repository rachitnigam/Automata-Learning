package learner

case class ObsTable(l: Seq[String], isMember: String => Boolean, ps: Seq[String] = Seq(""), ss: Seq[String] = Seq("")) {
  val preStrings = ps.distinct
  val sufStrings = ss.distinct
  val letters = l.distinct

  assert(isPrefixClosed(preStrings), s"${preStrings} is not prefix closed")
  assert(isSuffixClosed(sufStrings), s"${sufStrings} is not suffix closed")

  implicit class Printer(s: String) {
    def pString = s match {
      case "" => "λ"
      case _ => s
    }
  }

  val sa: Seq[String] = for(a <- letters; s <- preStrings) yield (s + a)

  def generateTable: String = {
    val buf = new StringBuilder
    val maxSa = sa.map(_.length).max
    val sufStr = sufStrings.map(_.pString).mkString(" | ")
    val div = "-" * (1 + maxSa + 3 + sufStr.length) + '\n'

    buf ++= "T" + " " * (maxSa - 1)+ " | "
    buf ++= sufStr
    buf += '\n'

    buf++= div
    for(s <- preStrings.sortWith(_.length < _.length)) {
      buf ++= s"${s.pString}" + " " * (maxSa - s.pString.length) + " | "
      buf ++= rowOf(s).map{x:Boolean => if(x) "1" else "0"}.map(_.pString).mkString(" | ")
      buf += '\n'
    }

    buf++= div
    for(s <- (sa diff preStrings).sortWith(_.length < _.length)) {
      buf ++= s"${s.pString}" + " "* (maxSa - s.pString.length) + " | "
      buf ++= rowOf(s).map{x:Boolean => if(x) "1" else "0"}.map(_.pString).mkString(" | ")
      buf += '\n'
    }

    buf.toString
  }

  def isSuffixClosed(lst: Seq[String]): Boolean = {
    def helper(str: String, set: Set[String]): Boolean = str match {
      case "" => set contains ""
      case _ => {
        val p = str.tail
        (set contains p) && helper(p,set)
      }
    }
    lst.map(helper(_,lst.toSet)).foldLeft(true)(_ && _)
  }

  def isPrefixClosed(lst: Seq[String]): Boolean = {
    def helper(str: String, set: Set[String]): Boolean = str match {
      case "" => set contains ""
      case _ => {
        val p = str.dropRight(1)
        (set contains p) && helper(p,set)
      }
    }
    lst.map(helper(_,lst.toSet)).foldLeft(true)(_ && _)
  }

  def rowOf(str: String): Seq[Boolean] = sufStrings.map{e: String => str + e}.map(isMember)  
  lazy val isClosed: Boolean = { 
    (findUnclosed == None)
  }

  def findUnclosed: Option[String] = {
    def iter(lst: Seq[String]): Option[String] = lst match {
      case hd::tl => {
        if( (for(s <- preStrings) yield (rowOf(s) != rowOf(hd))).foldLeft(true)(_ && _))
          Some(hd)
        else
          iter(tl)
      }
      case _ => None
    }
    iter(sa)
  }

  def isConsistent: Boolean = {
    (findInconsistent == None)
  } 

  def findInconsistent: Option[String] = {
    val p = 
      for(s1 <- preStrings; s2 <- preStrings; a <- letters; e <- sufStrings;
         if (s1 != s2 && rowOf(s1) == rowOf(s2) && isMember(s1 + a + e) != isMember(s2+a+e)))
           yield (a+e)
    if(p.length > 0) Some(p.head) else None
  }

  def generateHypothesis: Hypothesis = {
    assert(this.isClosed,"Cannot generate hypothesis with unclosed table")
    assert(this.isConsistent, "Cannot generate hypothesis with inconsistent table")
    type RowStateMap = Map[Seq[Boolean], State]

    def generateStates: RowStateMap = {
      val pd = preStrings.map{s => rowOf(s)}.distinct.zipWithIndex
      pd.map{case (s,i) => s -> State("q"+i)}(collection.breakOut)
    }
    def genTFun(map: RowStateMap): (State,String) => State = {
      val stateLetterList = for(s <- preStrings; a <- letters) yield (s,a)
      val m: Map[(State,String),State] = stateLetterList.map{case (s,a) => (map(rowOf(s)),a) -> map(rowOf(s + a))}(collection.breakOut)
      def f(st: State, str: String) = m((st,str))
      f
    }
    def getFStates(map: RowStateMap): Seq[State] = {
      def iter(lst: Seq[String], set: Set[State] = Set()): Set[State] = lst match {
        case str::tl => 
          if(isMember(str)) 
            iter(tl,set + map(rowOf(str)))
          else
            iter(tl,set)
        case _ => set
      }
      iter(preStrings).toSeq
    }
    
    val stateMap = generateStates
    val states = stateMap.values.toSeq
    val iState = stateMap(rowOf(""))
    val fStates = getFStates(stateMap)
    val tFun = genTFun(stateMap)
    new Hypothesis(letters, states, iState, fStates, tFun)
  }

  override def toString() = generateTable
}

object Observation {
//  def main(args: Array[String]) = {
//    def f(s: String) = s matches """ab*"""
//    def l = Seq("a","b")
//    def ps = Seq("","a","b","ba")
//    def ss = Seq("","a")
//
//    val obs = new ObsTable(l,f,ps,ss)
//    println(obs.generateTable)
//    println(s"Inconsistent: ${obs.findInconsistent}")
//    println(s"Unclosed: ${obs.findUnclosed}")
//    println(obs.generateHypothesis.generateTable)
//  }
}
