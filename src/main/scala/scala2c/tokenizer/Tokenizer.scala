package scala2c.tokenizer

import scala2c.io.Source

class Tokenizer(val source: Source) {
  var start: Int = 0
  var current: Int = 0
  var newLineProduced: Boolean = true
  var indentLevels: List[Int] = List(0)
  
  def content: String = source.content
  
  def blockIdentLevel: Int = indentLevels.head
  
  def currentIdentLevel: Int = {
    var i = current
    var x = 0
    while i >= 0 && content(i) != '\n' do {
      i -= 1
      x += 1
    }
    x
  }
  
  def startBlock(): Unit = 
    indentLevels = start :: indentLevels
  
  def endBlock(): Unit = indentLevels = indentLevels match {
    case Nil => Nil
    case _ :: xs => xs
  }
  
  def maybeProduceNewLine: Option[ScalaToken] =
    if !newLineProduced && currentIdentLevel <= blockIdentLevel then {
      newLineProduced = true
      endBlock()
      ???
    }
  
  def nextToken: ScalaToken = ???
  
  def allTokens: LazyList[ScalaToken] = nextToken match {
    case ScalaToken(_, _, ScalaTokenType.EndOfSource) => LazyList.empty
    case t => t #:: allTokens
  }
}
