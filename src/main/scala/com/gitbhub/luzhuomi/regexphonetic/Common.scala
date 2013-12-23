package com.github.luzhuomi.regexphonetic

object Common {
  

  def splitBy(f : Char => Boolean)(s : String) : List[String] = {
    if (s.length == 0) {
      Nil
    } else {
      break(f)(s) match {
	case (first,rest) => first :: (splitBy(f)(rest.dropWhile(f)))
      } // match
    } // if
  } // splitBy
  


  def splitBy2(f : Char => Boolean)(s : String) : List[String] = {
    if (s.length == 0) {
      Nil
    } else {
      break(f)(s) match {
	case (first,rest) => 
	  if (rest.length == 1 ) {
	    first :: List("")
	  } else {
	    first :: (splitBy2(f)(tail2(rest)))
	  } // if
      } // match
    } // if
  } // splitBy

  def tail2(s:String) : String = if (s.length == 0) { "" } else { s.tail }

  def break(f: Char => Boolean) (s : String) :(String, String) = {
    val first = s.takeWhile(x => !f(x))
    val second = s.dropWhile(x => !f(x))
    (first,second)
  }

  def isTab(c:Char) : Boolean = { c == '\t' }

}
