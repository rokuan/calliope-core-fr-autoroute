package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.IPlaceInfo
import com.rokuan.calliopecore.sentence.structure.data.place.PlaceObject.PlaceCategory

/**
  * Created by Christophe on 09/12/2016.
  */
case class PlaceInfo(value: String, placeCategory: PlaceCategory) extends IPlaceInfo {
  override def getPlaceCategory: PlaceCategory = placeCategory
  override def getValue: String = value
}
