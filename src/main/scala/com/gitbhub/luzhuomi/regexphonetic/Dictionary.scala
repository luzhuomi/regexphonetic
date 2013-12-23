package com.github.luzhuomi.regexphonetic

import scala.collection.immutable.HashMap

object Dictionary {
  
  type Dictionary[K,A] = HashMap[K,A]

  def empty [A,B] : Dictionary[A,B] = scala.collection.immutable.HashMap.empty

  def insert[K,A] (key:K) (value:A) (dict:Dictionary[K,A]) : Dictionary[K,A] = {
    dict + ((key,value))
  }

  def lookup[K,A] (key:K) (dict:Dictionary[K,A]) : Option[A] = dict.get(key)

  def update[K,A] (key:K) (value:A) (dict:Dictionary[K,A]) : Dictionary[K,A] =
    dict - (key) + ((key,value))

  def isNull[K,A]  (dict:Dictionary[K,A]) : Boolean = ! (dict.nonEmpty)

  def fromList[K,A] (l:List[(K,A)]) : Dictionary[K,A] = {
    val ed : Dictionary[K,A] = Dictionary.empty
    l.foldLeft(ed)((d:Dictionary[K,A] , p:(K,A)) => d + (p))
  }

  def isIn [K,A] (key:K) (dict:Dictionary[K,A]) : Boolean = dict.contains(key)

  def values [K,A] (dict:Dictionary[K,A]) : List[A] = List() ++ (dict.values)
}
