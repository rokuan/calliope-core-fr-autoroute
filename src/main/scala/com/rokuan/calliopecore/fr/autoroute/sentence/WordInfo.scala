package com.rokuan.calliopecore.fr.autoroute.sentence

import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType


/**
  * Created by Christophe on 09/12/2016.
  */
case class WordInfo(value: String, types: Set[WordType]) {
  def this(v: String, ts: WordType*) = this(v, ts.toSet)
}
