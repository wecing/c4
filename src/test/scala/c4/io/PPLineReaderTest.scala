package c4.io

import c4.messaging.Message
import c4.util.TestUtil
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
  }
}
