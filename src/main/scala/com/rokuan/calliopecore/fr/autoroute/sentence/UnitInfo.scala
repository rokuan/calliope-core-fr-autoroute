package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IUnitInfo
import com.rokuan.calliopecore.sentence.structure.data.nominal.UnitObject.UnitType

/**
  * Created by Christophe on 09/12/2016.
  */
case class UnitInfo(value: String, unitType: UnitType) extends IUnitInfo {
  override def getUnitType: UnitType = unitType
  override def getValue: String = value
}
