package com.github.luzhuomi.regexphonetic

import com.github.luzhuomi.regexphonetic.{Dictionary => D}
import com.github.luzhuomi.regexphonetic.Common._
import scala.io.Source

object SoundMap {
  case class SoundMap(rowIDs : List[String], colIDs : List[String], rowScores : D.Dictionary[String,List[(String,Float)]], colScores : D.Dictionary[String,List[(String,Float)]])
  
  def emptyMap : SoundMap = new SoundMap(List(), List(), D.empty, D.empty) 
  
  def transpose (sm : SoundMap) : SoundMap = sm match {
    case SoundMap (rowIDs, colIDs, rowScores, colScores) => SoundMap ( colIDs, rowIDs, colScores, rowScores)
  } // transpose

  // option #1
  // using side effect to read file directly
  // downside requires using var instead of val
  def parseSoundMap(fp:String) : SoundMap = {
    // var colIDs : List[String] = List()
    var soundMap = emptyMap
    for (line <- Source.fromFile(fp).getLines()) {
      if (soundMap.colIDs.length == 0) {
	parseHeader(line) match {
	  case (_ :: columnIDs) => { 
	    soundMap = SoundMap(List(), columnIDs, D.empty, D.empty)
	  } // case
	  case Nil => { }
	} // match
      } else {
	// splitBy2(isTab)(line) match {
	line.split("\t").toList match {
	  case Nil => { }
	  case (rowID :: scores) => {
	    val row_scores = soundMap.colIDs
	    .zip(scores.map(parseScore))
	    .filter(sndIsSome)
	    .map( (xsy => xsy match { 
	      case (x,Some(y)) => (x,y)  // it will never be None
	    } ) )
	    val rScores = D.insert(rowID)(row_scores)(soundMap.rowScores)
	    val cScores = row_scores.foldLeft(soundMap.colScores)( 
	      (d, colID_score ) => colID_score match { 
		case (colID,score) => D.lookup(colID)(d) match {
		  case None      => D.insert(colID)(List((rowID,score)))(d)
		  case Some(kvs) => D.update(colID)(kvs ++ List((rowID,score)))(d)
		} // inner match
	      }) // outer match and lambda
	    val rIDs = soundMap.rowIDs
	    val cIDs = soundMap.colIDs
	    soundMap = SoundMap(rIDs ++ List(rowID), cIDs, rScores, cScores)
	  } // case 
	} // match
      } // if
    } // for
    soundMap
  }

  def sndIsSome[A,B](p:(A,Option[B])) : Boolean = p match {
    case (x,Some(y)) => true 
    case _ => false
  }

  def parseHeader(s:String) : List[String] = s.split("\t").toList // splitBy2(isTab)(s)

  def parseScore(s:String) : Option[Float] = 
    if (s.length > 0) {
      try { Some(s.toFloat) } catch { case _ => None }
    } else {
      None
    }


}
