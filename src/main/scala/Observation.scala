class ObsTable(letters: Seq[String], preStrings: Seq[String], sufStrings: Seq[String], isMember: String => Boolean) {

  val sa: Seq[String] = for(a <- letters; s <- preStrings; if !( preStrings contains (s + a))) yield (s + a)

  def isPrefixClosed(lst: Seq[String]): Boolean = {
    def helper(str: String, set: Set[String]): Boolean = str match {
      case "" => set contains ""
      case _ => {
        val p = str.tail
        (set contains p) && helper(p,set)
      }
    }
    lst.map(helper(_,lst.toSet)).foldLeft(true)(_ && _)
  }

  def isSuffixClosed(lst: Seq[String]): Boolean = {
    def helper(str: String, set: Set[String]): Boolean = str match {
      case "" => set contains ""
      case _ => {
        val p = str.dropRight(1)
        (set contains p) && helper(p,set)
      }
    }
    lst.map(helper(_,lst.toSet)).foldLeft(true)(_ && _)
  }
  assert(isPrefixClosed(preStrings), s"${preStrings} is not prefix closed")
  assert(isSuffixClosed(sufStrings), s"${sufStrings} is not suffix closed")

  def rowOf(str: String): Seq[Boolean] = sufStrings.map{e: String => str + e}.map(isMember)  
  def isClosed: Boolean = { 
  //  //println(sa.map(isMember))
  //  def hasSomeSameRow(sa: String, lst: Seq[String]): Boolean = lst match {
  //    case hd::tl => { //print(s"$hd: ${rowOf(hd)}   "); println(s"$sa: ${rowOf(sa)}");  
  //      if(rowOf(hd) == rowOf(sa)) true 
  //      else hasSomeSameRow(sa,tl) 
  //    }
  //    case _ => false
  //  }

  //  (for(sa1 <- sa)
  //    yield hasSomeSameRow(sa1,preStrings)
  //  ).foldLeft(true)(_ && _)
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
  //  (for(s1 <- preStrings; s2 <- preStrings; if(s1 != s2 && rowOf(s1) == rowOf(s2)))
  //      yield 
  //      ((for(a <- letters) 
  //            yield ( rowOf(s1 + a) == rowOf(s2 + a))).foldLeft(true)(_ && _))
  //  ).foldLeft(true)(_ && _)
  (findInconsistent == None)
  } 

  def findInconsistent: Option[String] = {
    val p = 
      for(s1 <- preStrings; s2 <- preStrings; a <- letters; e <- sufStrings;
         if (s1 != s2 && rowOf(s1) == rowOf(s2) && isMember(s1 + a + e) != isMember(s2+a+e)))
           yield (a+e)
    //p.foreach{case (s1,s2,_,_) => println(s"$s1 : ${rowOf(s1)} & $s2 : ${rowOf(s2)}")}
    //p.foreach{case (s1,s2,a,e) =>println( s"$s1+$a+$e & $s2+$a+$e : ${isMember(s1 + a + e)} & ${isMember(s2+a+e)}")}
    if(p.length > 0) Some(p.head) else None
  }
}

object Observation {
  def main(args: Array[String]) = {
    def f(s: String) = s matches """ab*"""
    def l = Seq("a","b")
    def ps = Seq("","a","ba","b")
    def ss = Seq("")
    val obs = new ObsTable(l,ps,ss,f)
    println(s"Inconsistent: ${obs.findInconsistent}")
    println(s"Unclosed: ${obs.findUnclosed}")
  }
}
