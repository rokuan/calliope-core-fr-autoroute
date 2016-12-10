package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IColorInfo

/**
  * Created by Christophe on 09/12/2016.
  */
case class ColorInfo(value: String, hexCode: String) extends IColorInfo {
  override def getColorHexCode: String = hexCode
  override def getValue: String = value
}
