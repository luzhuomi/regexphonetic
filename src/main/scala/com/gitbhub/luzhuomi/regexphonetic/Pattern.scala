package com.github.luzhuomi.regexphonetic

object Pattern {
  abstract class Pat
  case object PPhi extends Pat
  case object PEmp extends Pat
  case class PLit (sym:Sym, conf:Conf) extends Pat
  case class PSeq (p1:Pat, p2:Pat) extends Pat
  case class PChoice (ps:List[Pat]) extends Pat
  case class PStar (p:Pat) extends Pat

  type Sym = String
  type Conf = Float
  type Sound = List[Sym]

  def sigma (r : Pat) : List[Sym] = {
    val s = sigmaSub(r)
    s.distinct
  }
  


  def sigmaSub(r:Pat): List[Sym] = r match {
    case PLit(s, _)   => List(s)
    case PSeq(p1, p2) => sigmaSub(p1) ++ sigmaSub(p2)
    case PChoice(ps)  => ps.flatMap(sigmaSub)
    case PStar(p)     => sigmaSub(p)
    case PPhi         => List()
    case PEmp         => List()
  }

  def isAny(s:Sym) : Boolean = (s == "_")

  def isEmpty(r:Pat) : Boolean = r match {
    case PEmp => true
    case PSeq(p1, p2) => isEmpty(p1) && isEmpty(p2) 
    case PChoice(ps)  => ps.exists(isEmpty)
    case PStar(p)     => true
    case _            => false
  }

  def mkLitPattern(s:String, c:Float) : Pat = PLit(s,c)
  
  def mkChoicePattern(ps:List[Pat]) : Pat = ps match {
    case Nil => PPhi
    case p::Nil => PChoice(ps)
    case _      => PChoice(ps)
  }

  def mkSequencePattern(ps:List[Pat]) : Pat = ps match {
    case Nil => PEmp
    case p::Nil => p
    case (p::ps2) => ps2.foldLeft(p)(PSeq)
  }

  def addEmp (ps:List[Pat]) : List[Pat] = PEmp::ps

  def interleave[A](xs:List[A])(y:A) : List[A] = xs match { 
    case Nil => Nil
    case (x::xs2) => x::y::(interleave(xs2)(y)) 
  }
}
