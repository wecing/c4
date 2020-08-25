package c4.io

import c4.messaging.{IllegalSourceException, Message}
import c4.util.TestUtil
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class PPReaderTest extends FlatSpec with Matchers {
  it should "accept function-like macros with no arguments" in {
    checkPPTokens(
      """#define f() 998
        |f()
        |""".stripMargin,
      Seq("998"))

    checkPPTokens(
      """#define f() 998
        |f()
        |f()
        |""".stripMargin,
      Seq("998", "998"))
  }

  it should "expand the example macros in c89 spec as expected" in {
    checkPPTokens(
      """#define x    3
        |#define f(a) f(x * (a))
        |#undef  x
        |#define x    2
        |#define g    f
        |#define z    z[0]
        |#define h    g(~
        |#define m(a) a(w)
        |#define w    0,1
        |#define t(a) a
        |
        |f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
        |g(x+(3,4)-w) | h 5) & m
        |         (f)^m(m);
        |""".stripMargin,
      // each token is only 1 character, so...
      wrapString("f(2*(y+1))+f(2*(f(2*(z[0]))))%f(2*(0))+t(1);" +
        "f(2*(2+(3,4)-0,1))|f(2*(~5))&f(2*(0,1))^m(0,1);").map(_.toString))
  }

  it should "implement # and ##" in {
    def q(s: String): String = "\"" + s + "\""

    checkPPTokens(
      """#define str(s)      # s
        |#define xstr(s)     str(s)
        |#define debug(s, t) printf("x" # s "= %d, x" # t "= %s", \
        |                                    x ## s, x ## t)
        |#define INCFILE(n)  vers ## n /* from previous #include example */
        |#define glue(a, b)  a ## b
        |#define xglue(a, b) glue(a, b)
        |#define HIGHLOW     "hello"
        |#define LOW         LOW ", world"
        |
        |debug(1, 2);
        |fputs(str(strncmp("abc\0d", "abc", '\4')  /* this goes away */
        |         == 0) str(: @\n), s);
        |include xstr(INCFILE(2).h)
        |glue(HIGH, LOW);
        |xglue(HIGH, LOW)
        |""".stripMargin,
      Seq(
        "printf", "(", q("x"), q("1"), q("= %d, x"), q("2"), q("= %s"), ",",
        "x1", ",", "x2", ")", ";",
        "fputs", "(",
        q("strncmp(\\\"abc\\\\0d\\\", \\\"abc\\\", '\\\\4') == 0"),
        q(": @\\n"), ",", "s", ")", ";",
        "include", q("vers2.h"),
        q("hello"), ";",
        q("hello"), q(", world")))
    // here's the output of `clang -E`:
    //
    // printf("x" "1" "= %d, x" "2" "= %s", x1, x2);
    // fputs("strncmp(\"abc\\0d\", \"abc\", '\\4') == 0" ": @\n", s);
    //
    // include "vers2.h"
    // "hello";
    // "hello" ", world"
  }

  it should "reject redefinition of macros" in {
    checkPPTokens(
      """#define OBJ_LIKE      (1-1)
        |#define OBJ_LIKE      /* white space */ (1-1) /* other */
        |#define FTN_LIKE(a)   ( a )
        |#define FTN_LIKE( a )(              /* note the white space */ \
        |                                    a /* other stuff on this line
        |                                      */ )
        |
        |// #define OBJ_LIKE    (0)     /*  different token sequence */
        |// #define OBJ_LIKE    (1 - 1) /*  different white space */
        |// #define FTN_LIKE(b) ( a )   /*  different parameter usage */
        |// #define FTN_LIKE(b) ( b )   /*  different parameter spelling */
        |""".stripMargin,
      Seq.empty)

    def w(cur: Int, enableOn: Int): String = if (cur == enableOn) "" else "//"

    w(1, 1) should be ("")
    w(1, 2) should be ("//")
    w(1, 3) should be ("//")
    w(1, 4) should be ("//")

    for (i <- 1 to 4) {
      val e = intercept[IllegalSourceException](checkPPTokens(
        s"""#define OBJ_LIKE      (1-1)
           |#define OBJ_LIKE      /* white space */ (1-1) /* other */
           |#define FTN_LIKE(a)   ( a )
           |#define FTN_LIKE( a )(              /* note the white space */ \\
           |                                    a /* other stuff on this line
           |                                      */ )
           |
           |${w(i,1)} #define OBJ_LIKE    (0)     // different token sequence
           |${w(i,2)} #define OBJ_LIKE    (1 - 1) // different white space
           |${w(i,3)} #define FTN_LIKE(b) ( a )   // different parameter usage
           |${w(i,4)} #define FTN_LIKE(b) ( b )   // different param spelling
           |""".stripMargin,
        Seq.empty))
      e.msg.toString should include ("Redefinition of macro")
    }
  }

  def checkPPTokens(input: String, expected: Seq[String]): Unit = {
    val fileName: String = TestUtil.createTempFile(input)
    val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
    val tokens: Seq[String] =
      PPReader.read(warnings, fileName)
        .map(_.value)
        .filterNot(_.isInstanceOf[PPTokWhiteSpc])
        .map(_.raw)
    tokens should be (expected)
  }
}
