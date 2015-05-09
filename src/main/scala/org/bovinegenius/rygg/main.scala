package org.bovinegenius.rygg

import org.bovinegenius.rygg.jil.AsmBuilder
import org.bovinegenius.rygg.io.IO

object Main {
  def main(args: Array[String]): Unit = {
    val Array(inputFile, outputFile) = args
    CodeGen.generateCode()
    println(s"${inputFile} -> ${outputFile}")
    
    val classFile = AsmBuilder.HelloWorld
    IO.spit(outputFile, classFile)
  }
}

