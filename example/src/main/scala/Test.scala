

import com.github.luzhuomi.regexphonetic.Match._
import com.github.luzhuomi.regexphonetic.Extract._
import com.github.luzhuomi.regexphonetic.SoundMap._
import com.github.luzhuomi.regexphonetic.Vowel._
import com.github.luzhuomi.regexphonetic.Consonant._
import com.github.luzhuomi.regexphonetic.{Dictionary => D}

object Test {

  val singlish_consonant_table = parseSoundMap("resources/consonants_snglish.dct")
  val singlish_vowel_table = parseSoundMap("resources/vowels_snglish.dct")

  def phonetic_distance(ctab:ConsonantTable)(vtab:VowelTable)(en:String)(sg:String) : Float = {
    val consonant_dict = D.fromList (ctab.rowIDs.map(x => (x,x)))
    val vowel_dict     = D.fromList (vtab.rowIDs.map(x => (x,x)))
    
    val sSounds = getSoundsFromSentence(sg)(consonant_dict)(vowel_dict)
    val eSounds = getSoundsFromSentence(en)(consonant_dict)(vowel_dict)


    val eSoundPattern = mkSoundPattern(eSounds)(ctab)(vtab)

    match_(eSoundPattern)(sSounds) / (sSounds.length)
  }

}


/*
scala> import Test._
import Test._

scala> phonetic_distance(singlish_consonant_table)(singlish_vowel_table)("goverment")("gahmen")
res0: Float = 0.6
*/
