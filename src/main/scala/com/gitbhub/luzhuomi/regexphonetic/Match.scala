package com.github.luzhuomi.regexphonetic


import com.github.luzhuomi.regexphonetic.{Dictionary => D}
import com.github.luzhuomi.regexphonetic.Pattern._

object Match {
  def match_(p:Pat)(s:Sound) : Conf = 
    if (s.length > 0) {
      val pcs = matchIterator(s)(List((p,0.0f)))
      val pcs2 = sort2(pcs)
      pcs2 match { 
	case Nil => -1.0f
	case ((p,c)::_) => c
      }
    } else {
      -1.0f
    } // if
  
  def matchIterator(s:Sound)(pcs:List[(Pat,Conf)]) : List[(Pat,Conf)] = s match {
    case Nil => pcs
    case (l::ls) => {
      val pcs2 = nub2(sort2(for ((p,c) <- pcs ;
				 (p2,f2) <- pd(l)(p)) yield (p2,f2(c)) ))
      matchIterator(ls)(pcs2)
    }
  } // match

  def nub2[A,B] (l:List[(A,B)]) : List[(A,B)] = {
    val a : List[(A,B)] = List() 
    val s : Set[A] = Set()
    l.foldLeft((a,s))( (as,xy) => 
      {
	val x = xy._1
	val y = xy._2
	val a = as._1
	val s = as._2
	if (s.contains(x)) 
	  { as } 
	else 
	  { (xy::a, s+(x)) }
      })._1.reverse
    }
  
  def sort2[A,B] (l:List[(A,B)])(implicit ord: math.Ordering[B]) : List[(A,B)] = 
    l.sortBy(_._2)(ord).reverse

  
  def pd(s:Sym)(p:Pat) : List[(Pat, Conf => Conf)] = p match {
    case PEmp => Nil
    case PLit(s2,c) if isAny(s2)    => List((PEmp, (x => c + x)))
    case PLit(s2,c) if (s==s2)      => List((PEmp, (x => c + x)))
    case PLit(s2,c) if true         => List((PEmp, (x => x - 1.0f)))
    case PSeq(p1,p2) if isEmpty(p1) => 
      { (for ((p1p, op) <- pd(s)(p1)) yield (PSeq(p1p,p2),op)) ++ pd(s)(p2) }
    case PSeq(p1,p2) if true        => 
      { for ((p1p, op) <- pd(s)(p1)) yield (PSeq(p1p,p2),op) }
    case PChoice(ps)                => ps.flatMap(p => pd(s)(p))
    case PStar(p)                   => 
      { for ((pp, op) <- pd(s)(p)) yield (PSeq(pp, PStar(p)), op) }
  }

  // todo compilation scheme
	   
        
}
