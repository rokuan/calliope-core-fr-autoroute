package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.sentence.{ICustomMode, ICustomObject, ICustomPerson, ICustomPlace}

/**
  * Created by Christophe on 09/12/2016.
  */
abstract class CustomData(name: String, code: String) {
  def getValue(): String = name
  def getCode(): String = code
}

case class CustomObject(name: String, code: String) extends CustomData(name, code) with ICustomObject
case class CustomPlace(name: String, code: String) extends CustomData(name, code) with ICustomPlace
case class CustomMode(name: String, code: String) extends CustomData(name, code) with ICustomMode
case class CustomPerson(name: String, code: String) extends CustomData(name, code) with ICustomPerson