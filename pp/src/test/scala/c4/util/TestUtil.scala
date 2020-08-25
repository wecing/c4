package c4.util

import java.io.{InputStream, File, FileOutputStream}
import java.net.URL

object TestUtil {

  /**
   * Creates a temporary file with given content, returns its absolute path.
   */
  def createTempFile(input: String): String = {
    val file = File.createTempFile("c4_test_", ".tmp.c")
    val stream = new FileOutputStream(file)
    stream.write(input.getBytes)
    stream.close()

    file.getAbsolutePath
  }

  /**
   * Creates a temporary file with given content, returns its absolute path.
   */
  def createTempFile(input: URL): String = {
    val file = File.createTempFile("c4_test_", ".tmp.c")
    val os = new FileOutputStream(file)
    val is: InputStream = input.openStream()
    val buf: Array[Byte] = new Array[Byte](4096)

    var n = is.read(buf)
    while (n > 0) {
      os.write(buf, 0, n)
      n = is.read(buf)
    }
    os.close()
    is.close()
    file.getAbsolutePath
  }
}
