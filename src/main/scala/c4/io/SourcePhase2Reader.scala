package c4.io

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Reader that combines translation phase 1 and 2.
 */
class SourcePhase2Reader(val warnings: ArrayBuffer[Message],
                         val fileName: String) {
  // phase 1: translate tri-graphs and handle unicode
  //    tri-graphs:
  //      ??=    #
  //      ??(    [
  //      ??/    \
  //      ??)    ]
  //      ??'    ^
  //      ??<    {
  //      ??!    |
  //      ??>    }
  //      ??-    ~
  //
  // phase 2: remove "\\\n". a non-empty source file shall end with "\n" but
  //          not "\\\n".

  private val file: Source = Source.fromFile(fileName)

  /**
   * Close the underlying reader.
   */
  def close() = file.close()

  private var buffered: Seq[Char] = Seq.empty
  private var bufferedLoc: (Int, Int) = (-1, -1) // init value doesn't matter
  private var nextCharLoc: (Int, Int) = (1, 1)

  // bufferedExtra should be located at nextCharLoc.
  // it is used to implement ungetc() for scala.io.Source
  private var bufferedExtra: Option[Char] = None

  private var last2: Option[(Char, (Int, Int))] = None
  private var last1: Option[(Char, (Int, Int))] = None
  private def checkFileEnding(buf: Option[(Char, (Int, Int))]): Unit = {
    if (buf.isEmpty) {
      (last2, last1) match {
        case (Some(('\\', loc)), Some(('\n', _))) =>
          throw IllegalSourceException(SimpleMessage(
            fileName, loc, "file ends with '\\' followed by newline character"))
        case (_, Some((c: Char, loc))) =>
          if (c != '\n') {
            throw IllegalSourceException(SimpleMessage(
              fileName, loc, "file does not end with newline character"))
          }
        case _ => ; // do nothing!
      }
    } else {
      last2 = last1
      last1 = buf
    }
  }

  /**
   * Read the next character and its physical location (line, col).
   */
  def read(): Option[(Char, (Int, Int))] = {
    val r = readImpl()
    checkFileEnding(r)
    r
  }

  // handles phase 1 and most of phase 2
  // (except checking '\n' and '\\\n' at end of file, which is caught by read())
  //
  // doesn't support CRLF.
  private def readImpl(): Option[(Char, (Int, Int))] = {
    if (buffered.nonEmpty) {
      val r: (Char, (Int, Int)) = (buffered.head, bufferedLoc)
      buffered = buffered.drop(1)
      bufferedLoc = (bufferedLoc._1, bufferedLoc._2 + 1)
      return Some(r)
    }
    // if (bufferedExtra.nonEmpty) {
    //   val c: Char = bufferedExtra.get
    //   val r: (Char, (Int, Int)) = (c, nextCharLoc)
    //   if (c == '\n') {
    //     nextCharLoc = (nextCharLoc._1 + 1, 1)
    //   } else {
    //     nextCharLoc = (nextCharLoc._1, nextCharLoc._2 + 1)
    //   }
    //   bufferedExtra = None
    //   return Some(r)
    // }

    // '\\' '\n' => restart, update nextCharLoc
    // '?' '?' '/' '\n' => restart, update nextCharLoc
    // '?' '?' (x) => return
    // '\n' => return
    // etc => add to return buffer
    var buffer: Seq[Char] = Seq.empty
    var bufferLoc: (Int, Int) = nextCharLoc
    var cont = true
    while (cont && (bufferedExtra.nonEmpty || file.hasNext)) {
      if (bufferedExtra.nonEmpty) {
        buffer = buffer :+ bufferedExtra.get
        bufferedExtra = None
      } else {
        buffer = buffer :+ file.next()
      }
      nextCharLoc = (nextCharLoc._1, nextCharLoc._2 + 1)

      if (buffer.endsWith(Seq('\\', '\n'))) {
        if (!file.hasNext) {
          throw IllegalSourceException(SimpleMessage(
            fileName,
            (bufferLoc._1, bufferLoc._2 + buffer.size - 2),
            "file should not end with backslash followed by newline"
          ))
        }

        buffer = buffer.dropRight(2)
        if (buffer.nonEmpty) {
          cont = false
        } else {
          bufferLoc = (bufferLoc._1 + 1, 1)
        }
        nextCharLoc = (nextCharLoc._1 + 1, 1)
      } else if (buffer.endsWith(Seq('?', '?', '/'))) {
        buffer = buffer.dropRight(3) :+ '\\'
        if (file.hasNext) { // peek next
          val c: Char = file.next()
          if (c == '\n') {
            if (!file.hasNext) {
              throw IllegalSourceException(SimpleMessage(
                fileName,
                (bufferLoc._1, bufferLoc._2 + buffer.size - 1),
                "file should not end with backslash followed by newline"
              ))
            }

            buffer = buffer.dropRight(1)
            if (buffer.nonEmpty) {
              cont = false
            } else {
              bufferLoc = (bufferLoc._1 + 1, 1)
            }
            nextCharLoc = (nextCharLoc._1 + 1, 1)
          } else {
            bufferedExtra = Some(c) // does not incr nextCharLoc
            cont = false
          }
        }
      } else if (buffer.endsWith(Seq('?', '?', '='))) {
        buffer = buffer.dropRight(3) :+ '#'
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', '('))) {
        buffer = buffer.dropRight(3) :+ '['
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', ')'))) {
        buffer = buffer.dropRight(3) :+ ']'
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', '\''))) {
        buffer = buffer.dropRight(3) :+ '^'
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', '<'))) {
        buffer = buffer.dropRight(3) :+ '{'
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', '!'))) {
        buffer = buffer.dropRight(3) :+ '|'
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', '>'))) {
        buffer = buffer.dropRight(3) :+ '}'
        cont = false
      } else if (buffer.endsWith(Seq('?', '?', '-'))) {
        buffer = buffer.dropRight(3) :+ '~'
        cont = false
      } else if (buffer.endsWith(Seq('\n'))) {
        cont = false
        nextCharLoc = (nextCharLoc._1 + 1, 1)
        // '\\\s+\n' should probably be caught here and produce warning messages
        // but given that we cannot catch '??/\s+\n', this check is omitted.
      }
    }

    if (buffer.nonEmpty) {
      buffered = buffer
      bufferedLoc = bufferLoc
      return readImpl() // only goes back to head of readImpl()
    }

    None // EOF
  }
}
