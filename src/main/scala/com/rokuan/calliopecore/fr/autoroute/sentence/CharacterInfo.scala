package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.ICharacterInfo
import com.rokuan.calliopecore.sentence.structure.data.nominal.CharacterObject.CharacterType

/**
  * Created by Christophe on 09/12/2016.
  */
case class CharacterInfo(value: String, characterType: CharacterType) extends ICharacterInfo {
  override def getCharacterType: CharacterType = characterType
  override def getValue: String = value
}
