package c4.ast

import java_cup.runtime.Scanner
import java_cup.runtime.Symbol

import c4.io._
import c4.io.SourcePhase7Reader


class C4Scanner(val reader: SourcePhase7Reader) extends Scanner {
  @throws(classOf[java.lang.Exception])
  override def next_token(): Symbol = {
    reader.get() match {
      case None => new Symbol(CupSymbols.EOF)
      case Some(located) => located.value match {
        case tok: TokId         => new Symbol(tok.cupSymbol, located)
        case tok: TokInteger    => new Symbol(tok.cupSymbol, located)
        case tok: TokFloat      => new Symbol(tok.cupSymbol, located)
        case tok: TokDouble     => new Symbol(tok.cupSymbol, located)
        case tok: TokLongDouble => new Symbol(tok.cupSymbol, located)
        case tok: TokChar       => new Symbol(tok.cupSymbol, located)
        case tok: TokWideChar   => new Symbol(tok.cupSymbol, located)
        case tok: TokStr        => new Symbol(tok.cupSymbol, located)
        case tok: TokWideStr    => new Symbol(tok.cupSymbol, located)
        case tok => new Symbol(tok.cupSymbol)
      }
    }
  }
}
