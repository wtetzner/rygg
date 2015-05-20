package org.bovinegenius.rygg.ril


object Tester {
  def func(): String = {
    val y = {
      val r = "x"
      r + "f"
    }
    val x = try {
      println("asdf")
      "hmm"
    } catch {
      case e: RuntimeException => throw e
      case _: Exception => "blah"
    } finally {
      println("I dunno")
    }
    println("bob")
    x
  }
}
