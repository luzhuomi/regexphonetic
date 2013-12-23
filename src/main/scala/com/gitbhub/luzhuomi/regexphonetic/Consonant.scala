package com.github.luzhuomi.regexphonetic


import com.github.luzhuomi.regexphonetic.SoundMap._


object Consonant {
  type ConsonantTable = SoundMap
  def mConsonantsList = List( "ng", "ps")   // minor
  def wConsonantsList = List( "r", "l", "n") // weak
  def eConsonantsList = List( "n", "r", "y") // ending 
}
