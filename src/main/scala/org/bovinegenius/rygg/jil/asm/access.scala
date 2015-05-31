package org.bovinegenius.rygg.jil.asm

import org.bovinegenius.rygg.java.{ AccessLevel => AccessLevelType }
import org.bovinegenius.rygg.java.Public
import org.objectweb.asm.Opcodes
import org.bovinegenius.rygg.java.Protected
import org.bovinegenius.rygg.java.Private
import org.bovinegenius.rygg.java.Package
import org.bovinegenius.rygg.java.Access
import org.bovinegenius.rygg.java.Final
import org.bovinegenius.rygg.java.Static

class AccessFlags(val integer: Int) extends AnyVal {
  def isSet(flag: Int): Boolean = (integer & flag) != 0
}

object AccessFlags {
  implicit def int2AccessFlags(integer: Int): AccessFlags = new AccessFlags(integer)

  def apply(access: Access): AccessFlags = {
    new AccessFlags(access.options.foldLeft(accessLevelFlag(access.level))((result, option) => {
      val flag: Int = option match {
        case Static => Opcodes.ACC_STATIC
        case Final => Opcodes.ACC_FINAL
      }
      result | flag
    }))
  }

  private def accessLevelFlag(level: AccessLevelType): Int = level match {
    case Public => Opcodes.ACC_PUBLIC
    case Private => Opcodes.ACC_PRIVATE
    case Protected => Opcodes.ACC_PROTECTED
    case Package => 0
  }
}

object AccessLevel {
  def apply(flags: AccessFlags): AccessLevelType = {
    if (flags.isSet(Opcodes.ACC_PUBLIC)) {
      Public
    } else if (flags.isSet(Opcodes.ACC_PRIVATE)) {
      Private
    } else if (flags.isSet(Opcodes.ACC_PROTECTED)) {
      Protected
    } else {
      Package
    }
  }
}
