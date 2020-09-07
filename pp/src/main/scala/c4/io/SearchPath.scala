package c4.io

import java.nio.file.{Files, Paths}

object SearchPath {
  def exists(f: String): Boolean = Files.exists(Paths.get(f))

  def find(curFile: String, file: String, isCaret: Boolean): Option[String] = {
    if (isCaret) {
      val url = getClass().getResource("/include/" + file)
      if (url != null) {
        return Some("jar:include/" + file)
      }
    } else {
      if (Paths.get(file).isAbsolute()) {
        return Some(file)
      }

      val path =
        Option(Paths.get(curFile).getParent())
          .map(_.toString())
          .getOrElse("") + "/" + file
      if (exists(path)) {
        return Some(path)
      }
    }

    None
  }
}
