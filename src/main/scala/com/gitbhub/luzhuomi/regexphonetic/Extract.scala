package com.github.luzhuomi.regexphonetic

import com.github.luzhuomi.regexphonetic.{Dictionary => D} 
import com.github.luzhuomi.regexphonetic.Pattern._
import com.github.luzhuomi.regexphonetic.Consonant._
import com.github.luzhuomi.regexphonetic.Vowel._
import com.github.luzhuomi.regexphonetic.SoundMap._

object Extract {

  abstract class Segment
  case class CSeg(s:String) extends Segment
  case class VSeg(s:String) extends Segment

  def getSoundsFromSentence(str:String)(consonants:D.Dictionary[String,String])(vowels:D.Dictionary[String,String]) : List[String] = {
    val ws = str.split(" ").toList
    ws.flatMap( (w:String) => getSoundsFromWord(w)(consonants)(vowels)) 
  }  

  def getSoundsFromWord(w:String)(consonants:D.Dictionary[String,String])(vowels:D.Dictionary[String,String]) : List[String] = {
    def dropSegTag(seg:Segment):String = seg match { 
      case CSeg(s) => s
      case VSeg(s) => s
    }
    segmentWord(w)(consonants)(vowels).map(dropSegTag) 
  }

  def getScores(k:String)(dict:D.Dictionary[String,List[(String,Float)]]) = D.lookup(k)(dict) match {
    case None => Nil
    case Some(kvs) => kvs
  }

  def segmentWord(w:String)(consonants:D.Dictionary[String,String])(vowels:D.Dictionary[String,String]) : List[Segment] = {

    def myLookup(k:String)(d:D.Dictionary[String,String]) : Option[String] = D.lookup(k)(d) match {
      case Some(_) => Some(k)
      case None    => None
    }

    def firstSome[A] (l:List[Option[A]]) : Option[A] = {
      val lp = l.filter( x => !(x.isEmpty ) )
      lp match {
	case Nil => None
	case (x::_) => x
      }
    }

    if (w.length == 0) {
      Nil
    } else {
      val maxLength = (D.values(consonants) ++ D.values(vowels)).map(_.length).max

      // prefices
      val p = w.take(maxLength).map(_.toLower)
      // scala inits "abc".inits.toList = List(abc, ab, a, ""), hence no need to reverse before tailing, because we want to exclude ""
      val prefices = p.inits.toList.reverse.tail.reverse

      val opConsonants = prefices.map(w => myLookup(w)(consonants))
      val opVowels     = prefices.map(w => myLookup(w)(vowels))

      (firstSome(opConsonants),firstSome(opVowels)) match {
	case (Some(c), Some(v)) if v.length > c.length => VSeg(v)::(segmentWord(w.drop(v.length))(consonants)(vowels))
	case (Some(c), Some(v)) if true                => CSeg(c)::(segmentWord(w.drop(c.length))(consonants)(vowels))
	case (Some(c), None)                           => CSeg(c)::(segmentWord(w.drop(c.length))(consonants)(vowels))
	case (None, Some(v))                           => VSeg(v)::(segmentWord(w.drop(v.length))(consonants)(vowels))
	case (None, None)                              => segmentWord(w.tail)(consonants)(vowels)
      }
      
    }
  }

  def mkSoundPattern(sounds:List[String])(cTab:ConsonantTable)(vTab:VowelTable) : Pat = {
    val cD = cTab.rowScores
    val vD = vTab.rowScores
    val soundScores : List[List[(String,Float)]] = sounds.map(s => (getScores(s)(cD)) ++ (getScores(s)(vD)))
    val choiceP = soundScores.map( soundScore => mkChoicePattern(addEmp(soundScore.map(x => mkLitPattern(x._1,x._2)))))
    mkSequencePattern( interleave(choiceP)(anySounds) )
  }

  def anySounds : Pat = PStar(mkLitPattern("_", -1.0f))
  
  
  

}
