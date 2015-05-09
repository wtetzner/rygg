package org.bovinegenius.rygg.jil

class AccessFlags(val integer: Int) extends AnyVal {
  def isSet(flag: Int): Boolean = (integer & flag) != 0
}

object AccessFlags {
  implicit def int2AccessFlags(integer: Int): AccessFlags = new AccessFlags(integer)
}
