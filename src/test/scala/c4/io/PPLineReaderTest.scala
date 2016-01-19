package c4.io

import c4.messaging.Message
import c4.util.{Located, TestUtil}
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class PPLineReaderTest extends FlatSpec with Matchers {
  it should "recognize pp-tokens" in {
    checkPPTokens("===\n", Seq(Seq("==", "=").map(PPTokSym)))
    checkPPTokens("+====\n", Seq(Seq("+=", "==", "=").map(PPTokSym)))
    checkPPTokens("/= =\n", Seq(Seq("/=", "=").map(PPTokSym)))
    checkPPTokens("/== \n", Seq(Seq("/=", "=").map(PPTokSym)))
    checkPPTokens("/==\n", Seq(Seq("/=", "=").map(PPTokSym)))
    checkPPTokens("/====\n", Seq(Seq("/=", "==", "=").map(PPTokSym)))
    checkPPTokens("/+\n", Seq(Seq("/", "+").map(PPTokSym)))
    checkPPTokens("/#\n", Seq(Seq("/", "#").map(PPTokSym)))
    checkPPTokens("F1()\n",
      Seq(Seq(PPTokId("F1"), PPTokSym("("), PPTokSym(")"))))
  }

  it should "recognize comments" in {
    checkPPTokens("//\n", Seq.empty)
    checkPPTokens("//===\n", Seq.empty)
    checkPPTokens("//\n/=\n", Seq(Seq(PPTokSym("/="))))
    checkPPTokens("//===\n/+\n", Seq(Seq("/", "+").map(PPTokSym)))
    checkPPTokens("*//\n/=\n", toPPTokSyms(Seq(Seq("*"), Seq("/="))))
    checkPPTokens("**//===\n/+\n",
      toPPTokSyms(Seq(Seq("*", "*"), Seq("/", "+"))))
    checkPPTokens(
      """= =/*!!//
        |^==+=// jjj \\
        |kkk
        |--++ // */ )
        |>.< <<= >>= ###
        |...
        |. .. ... .... ..... ......
        |""".stripMargin,
      toPPTokSyms(Seq(
        Seq("=", "=", ")"),
        Seq(">", ".", "<", "<<=", ">>=", "##", "#"),
        Seq("..."),
        Seq(".", ".", ".", "...", "...", ".", "...", ".", ".", "...", "...")
      )))
    checkPPTokens(
      """// /*!!
        | ^= */ ^
        |/* haskell大法坏 */ ($)
        |""".stripMargin,
      toPPTokSyms(Seq(Seq("^=", "*", "/", "^"), Seq("(", "$", ")"))))
  }

  it should "recognize pp-numbers" in {
    checkPPTokens("5 .5 .5. .5.. .5... .5....\n",
      Seq(Seq("5", ".5", ".5.", ".5..", ".5...", ".5....").map(PPTokNum)))
    checkPPTokens("42 42.42 0 08 1a 54.abc .4ab\n",
      Seq(Seq("42", "42.42", "0", "08", "1a", "54.abc", ".4ab").map(PPTokNum)))
    checkPPTokens("1ae 1e+ 1ae- 1aE+ 1E- 1ae-0xF.E+\n",
      Seq(Seq("1ae", "1e+", "1ae-", "1aE+", "1E-", "1ae-0xF.E+").map(PPTokNum)))
  }

  it should "recognize string and character literals" in {
    checkPPTokens("\"//\"\n", Seq(Seq(PPTokStr("\"//\""))))
    checkPPTokens("L\"/*\"\n", Seq(Seq(PPTokStr("L\"/*\""))))
    checkPPTokens("'\\a'\n", Seq(Seq(PPTokChar("'\\a'"))))
    checkPPTokens("L'\\bc'\n", Seq(Seq(PPTokChar("L'\\bc'"))))
    checkPPTokens("'abc'\n", Seq(Seq(PPTokChar("'abc'"))))
    checkPPTokens("'\\\''\n", Seq(Seq(PPTokChar("'\\\''"))))
    checkPPTokens("'\\\"'\n", Seq(Seq(PPTokChar("'\\\"'"))))
    checkPPTokens("'\\?'\n", Seq(Seq(PPTokChar("'\\?'"))))
    checkPPTokens("'\\\\'\n", Seq(Seq(PPTokChar("'\\\\'"))))
    checkPPTokens("'\\a'\n", Seq(Seq(PPTokChar("'\\a'"))))
    checkPPTokens("'\\b'\n", Seq(Seq(PPTokChar("'\\b'"))))
    checkPPTokens("'\\f'\n", Seq(Seq(PPTokChar("'\\f'"))))
    checkPPTokens("'\\n'\n", Seq(Seq(PPTokChar("'\\n'"))))
    checkPPTokens("'\\r'\n", Seq(Seq(PPTokChar("'\\r'"))))
    checkPPTokens("'\\t'\n", Seq(Seq(PPTokChar("'\\t'"))))
    checkPPTokens("'\\v'\n", Seq(Seq(PPTokChar("'\\v'"))))
    checkPPTokens("'\\0'\n", Seq(Seq(PPTokChar("'\\0'"))))
    checkPPTokens("'\\11'\n", Seq(Seq(PPTokChar("'\\11'"))))
    checkPPTokens("'\\18'\n", Seq(Seq(PPTokChar("'\\18'"))))
    checkPPTokens("'\\777'\n", Seq(Seq(PPTokChar("'\\777'"))))
    checkPPTokens("'\\7777'\n", Seq(Seq(PPTokChar("'\\7777'"))))
    checkPPTokens("'\\7778'\n", Seq(Seq(PPTokChar("'\\7778'"))))
    checkPPTokens("'\\xffff'\n", Seq(Seq(PPTokChar("'\\xffff'"))))

    checkPPTokens("'\\n\\0018i\n'\n", Seq(
      Seq(PPTokSym("'"), PPTokSym("\\"), PPTokId("n"),
        PPTokSym("\\"), PPTokNum("0018i")),
      Seq(PPTokSym("'"))))
  }

  it should "recognize preprocessing directives" in {
    val input: String =
      """  #if/**/0
        |a
        |#pragma
        |#
        |#      elif 1//hee-hee-hee!
        |#include <stdio.h>
        |#include "wow.h"
        |#define/**/X/**/b
        |#define F1() "hi"
        |#define F2(x) {x}
        |#define F3(x,y) {x+y}
        |#define F4(x,/*
        |*/y) {\
        |    x+/*
        |*/y \
        |}
        |#line 35
        |X
        |#line 32 "naxx.c"
        |F1()
        |#ifndef X
        |#error impossible
        |#endif
        |#undef X
        |#ifdef X
        |#error no way
        |#endif
        |#/*
        |*/else
        |#error what happened?
        |#endif
        |""".stripMargin
    val expected: Seq[PPLine] = Seq(
      PPLineIf(
        (1, 3),
        List(
          Located((1, 6), PPTokWhiteSpc(' ')),
          Located((1, 10), PPTokNum("0")))),
      PPLineTokens(
        List(
          Located((2, 1), PPTokId("a")))),
      PPLinePragma((3, 1)),
      PPLineNull((4, 1)),
      PPLineElif(
        (5, 1),
        List(
          Located((5, 12), PPTokWhiteSpc(' ')),
          Located((5, 13), PPTokNum("1")))),
      PPLineInclude(
        (6, 1),
        Located((6, 10), "stdio.h"),
        isCaret = true),
      PPLineInclude(
        (7, 1),
        Located((7, 10), "wow.h"),
        isCaret = false),
      PPLineDefineObj(
        (8, 1),
        Located((8, 12), "X"),
        List(
          Located((8, 13), PPTokWhiteSpc(' ')),
          Located((8, 17), PPTokId("b")))),
      PPLineDefineFunc(
        (9, 1),
        Located((9, 9), "F1"),
        List(),
        List(
          Located((9, 13), PPTokWhiteSpc(' ')),
          Located((9, 14), PPTokStr("\"hi\"")))),
      PPLineDefineFunc(
        (10, 1),
        Located((10, 9), "F2"),
        List(Located((10, 12), "x")),
        List(
          Located((10, 14), PPTokWhiteSpc(' ')),
          Located((10, 15), PPTokSym("{")),
          Located((10, 16), PPTokId("x")),
          Located((10, 17), PPTokSym("}")))),
      PPLineDefineFunc(
        (11, 1),
        Located((11, 9), "F3"),
        List(
          Located((11, 12), "x"),
          Located((11, 14), "y")),
        List(
          Located((11, 16), PPTokWhiteSpc(' ')),
          Located((11, 17), PPTokSym("{")),
          Located((11, 18), PPTokId("x")),
          Located((11, 19), PPTokSym("+")),
          Located((11, 20), PPTokId("y")),
          Located((11, 21), PPTokSym("}")))),
      PPLineDefineFunc(
        (12, 1),
        Located((12, 9), "F4"),
        List(
          Located((12, 12), "x"),
          Located((13, 3), "y")),
        List(
          Located((13, 5), PPTokWhiteSpc(' ')),
          Located((13, 6), PPTokSym("{")),
          Located((14, 1), PPTokWhiteSpc(' ')),
          Located((14, 2), PPTokWhiteSpc(' ')),
          Located((14, 3), PPTokWhiteSpc(' ')),
          Located((14, 4), PPTokWhiteSpc(' ')),
          Located((14, 5), PPTokId("x")),
          Located((14, 6), PPTokSym("+")),
          Located((14, 7), PPTokWhiteSpc(' ')),
          Located((15, 3), PPTokId("y")),
          Located((15, 4), PPTokWhiteSpc(' ')),
          Located((16, 1), PPTokSym("}")))),
      PPLineLine((17, 1), 35, None),
      PPLineTokens(List(Located((18, 1), PPTokId("X")))),
      PPLineLine((19, 1), 32, Some("\"naxx.c\"")),
      PPLineTokens(
        List(
          Located((20, 1), PPTokId("F1")),
          Located((20, 3), PPTokSym("(")),
          Located((20, 4), PPTokSym(")")))),
      PPLineIfndef((21, 1), Located((21, 9), "X")),
      PPLineError((22, 1), "impossible"),
      PPLineEndif((23, 1)),
      PPLineUndef((24, 1), Located((24, 8), "X")),
      PPLineIfdef((25, 1), Located((25, 8), "X")),
      PPLineError((26, 1), "no way"),
      PPLineEndif((27, 1)),
      PPLineElse((28, 1)),
      PPLineError((30, 1), "what happened?"),
      PPLineEndif((31, 1))
    )

    val path: String = TestUtil.createTempFile(input)
    val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val reader: PPLineReader = new PPLineReader(warnings, path)

    def recur(rest: Seq[PPLine]): Unit = {
      reader.read() match {
        case None =>
          rest.isEmpty should be (true)
        case Some(line) =>
          line should be (rest.head)
          recur(rest.tail)
      }
    }

    recur(expected)
    reader.close()
  }

  private def toPPTokSyms(xs: Seq[Seq[String]]): Seq[Seq[PPTokSym]] = {
    xs.map(_.map(PPTokSym))
  }

  private def checkPPTokens(str: String, tokens: Seq[Seq[PPTok]]): Unit = {
    val path: String = TestUtil.createTempFile(str)
    val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val reader: PPLineReader = new PPLineReader(warnings, path)

    def isPPTokWhiteSpc(t: PPTok): Boolean = t match {
      case PPTokWhiteSpc(_) => true
      case _ => false
    }

    def checkLine(tokens: Seq[Seq[PPTok]]): Unit = {
      reader.read() match {
        case None =>
          tokens should be (Seq.empty)
        case Some(PPLineTokens(xs)) =>
          val ts: Seq[PPTok] = xs.map(x => x.value).filterNot(isPPTokWhiteSpc)
          ts should be (tokens.head)
          checkLine(tokens.tail)
        case Some(x) =>
          fail(s"unexpected PPLine: $x")
      }
    }

    checkLine(tokens)
    warnings.isEmpty should be (true)
    reader.close()
  }
}
