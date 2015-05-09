package org.bovinegenius.rygg.io

import java.io.FileOutputStream

object IO {
  def spit(filename: String, bytes: Array[Byte]): Unit = {
    val outstream = new FileOutputStream(filename)
    try {
      outstream.write(bytes)
    } finally {
      outstream.close()
    }
  }
}
