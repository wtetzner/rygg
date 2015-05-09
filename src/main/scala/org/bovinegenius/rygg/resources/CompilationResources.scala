package org.bovinegenius.rygg.resources

import java.net.URLClassLoader
import java.net.URL
import org.apache.commons.io.IOUtils

case class CompilationResources(val classpath: String) {
  val classpathUrls: Array[URL] = classpath.split(System.getProperty("path.separator")).map(u => new URL(s"file:$u"))
  private lazy val classloader: ClassLoader = new URLClassLoader(classpathUrls, null)

  def loadBytes(path: String): Option[Array[Byte]] = {
    val stream = classloader.getResourceAsStream(path)
    if (stream == null) {
      None
    } else {
      Some(IOUtils.toByteArray(stream))
    }
  }
}
