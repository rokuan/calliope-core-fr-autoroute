package com.rokuan.calliopecore.fr.autoroute.data

import com.rokuan.calliopecore.fr.autoroute.sentence.{PossessivePronoun, Pronoun, Word}
import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType
import com.rokuan.calliopecore.sentence.structure.data.count.CountObject.ArticleType
import com.rokuan.calliopecore.sentence.structure.data.count._

/**
  * Created by Christophe on 27/11/2016.
  */
object CountConverter {
  import com.rokuan.calliopecore.fr.autoroute.pattern.WordRules._
  import com.rokuan.calliopecore.fr.autoroute.sentence.Word.WordType._

  private val NumberMap = Array("un", "deux", "trois", "quatre", "cinq", "six",
    "sept", "huit", "neuf", "dix", "onze", "douze", "treize", "quatorze", "quinze",
    "seize")

  private val TenStepMap = Array("dix", "vingt", "trente", "quarante",
    "cinquante", "soixante")

  private val TenPowerMap = Array("dix", "cent", "mille", "_", "_", "million", "_",
    "_", "milliard")

  def isSingular(article: String) = !article.split("-").exists(w => w.last == 's' || w.last == 'x')

  def parsePosition(position: String) = position match {
    case "premier" | "première" | "1er" | "1ère" | "1ere" => 1
    case s if s.endsWith("ième") =>
      val count = s.substring(0, s.length - 4)
      val trimmedCount =
        if(count.last == 'u'){
          count.substring(0, count.length - 1)
        } else {
          count
        }
      parseCount(trimmedCount)
    case _ => 0 // TODO
  }

  def parseCount(count: String) =
    TenPowerMap.zipWithIndex.collectFirst {
      case (s, index) if Math.abs(s.length - count.length) <= 1
        && s.startsWith(count) => math.pow(10, (index + 1)).toInt
    }.orElse {
      TenStepMap.zipWithIndex.collectFirst {
        case (s, index) if Math.abs(s.length - count.length) <= 1
          && s.startsWith(count) => (index + 1) * 10
      }
    }.orElse {
      NumberMap.zipWithIndex.collectFirst {
        case (s, index) if math.abs(s.length - count.length) <= 1
          && s.startsWith(count) => (index + 1)
      }
    }.getOrElse(0)

  val ArticleTransformer = { (t: WordType, d: ArticleType) =>
    word(t) { article =>
      val result =
        if(isSingular(article.getValue)){
          new FixedItemObject(1)
        } else {
          new AllItemsObject
        }
      result.possessiveTarget = PossessivePronoun(article.getValue)
      result.definition = d
      result
    }
  }

  val FixedItemRule = (word(DEFINITE_ARTICLE) ~ word(NUMERICAL_POSITION)) {
    case List(_, position: Word) =>
      // TODO: how to do for the last element ?
      new FixedItemObject(parsePosition(position.getValue))
  }
  val FixedRangeRule = (word(DEFINITE_ARTICLE) ~ word(NUMBER) ~ word(NUMERICAL_POSITION)) {
    case List(_, n: Word, position: Word) =>
      val count = n.getValue.toLong
      val range = position.getValue match {
        case s if s.startsWith("premi") => CountObject.Range.FIRST
        case s if s.startsWith("derni") => CountObject.Range.LAST
        case _ => CountObject.Range.FIRST
      }
      new LimitedItemsObject(range, count)
  }
  val AllRule = (word("tout|toute|tous|toutes") ~ word(DEFINITE_ARTICLE)) {
    case List(_, article: Word) =>
      val result = new AllItemsObject
      result.definition =
        if(article.isOfType(POSSESSIVE_ADJECTIVE)){
          ArticleType.POSSESSIVE
        } else if(article.isOfType(INDEFINITE_ARTICLE)) {
          ArticleType.INDEFINITE
        } else {
          ArticleType.DEFINITE
        }
      result
  }
  val QuantityRule = (word(DEFINITE_ARTICLE) ~ word(NUMBER)) {
    case List(_, n: Word) =>
      new QuantityObject(parseCount(n.getValue))
  }
  val SimpleArticleRule = ArticleTransformer(DEFINITE_ARTICLE, ArticleType.DEFINITE) |
    ArticleTransformer(INDEFINITE_ARTICLE, ArticleType.INDEFINITE) |
    ArticleTransformer(POSSESSIVE_ADJECTIVE, ArticleType.POSSESSIVE) |
    ArticleTransformer(DEMONSTRATIVE_ADJECTIVE, ArticleType.DEMONSTRATIVE)

  val CountRule = FixedItemRule | FixedRangeRule | AllRule | QuantityRule | SimpleArticleRule
}
