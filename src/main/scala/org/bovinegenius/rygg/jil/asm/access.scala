package org.bovinegenius.rygg.jil.asm

import org.bovinegenius.rygg.java.{ AccessLevel => AccessLevelType }
import org.bovinegenius.rygg.java.Public
import org.objectweb.asm.Opcodes
import org.bovinegenius.rygg.java._

class AccessFlags(val integer: Int) extends AnyVal {
  def isSet(flag: Int): Boolean = (integer & flag) != 0
}

object AccessFlags {
  implicit def int2AccessFlags(integer: Int): AccessFlags = new AccessFlags(integer)

  def apply(access: MethodAccess): AccessFlags = {
    new AccessFlags(access.options.foldLeft(accessLevelFlag(access.level))((result, option) => {
      result | accessFlag(option)
    }))
  }

  def apply(access: FieldAccess): AccessFlags = {
    new AccessFlags(access.options.foldLeft(accessLevelFlag(access.level))((result, option) => {
      result | accessFlag(option)
    }))
  }

  def apply(access: ClassAccess): AccessFlags = {
    new AccessFlags(access.options.foldLeft(accessLevelFlag(access.level))((result, option) => {
      result | accessFlag(option)
    }))
  }

  def apply(access: InnerClassAccess): AccessFlags = {
    new AccessFlags(access.options.foldLeft(accessLevelFlag(access.level))((result, option) => {
      result | accessFlag(option)
    }))
  }

  private def accessFlag(option: MethodAccessOption): Int = option match {
        case Static => Opcodes.ACC_STATIC
        case Final => Opcodes.ACC_FINAL
        case Abstract => Opcodes.ACC_ABSTRACT
        case Bridge => Opcodes.ACC_BRIDGE
        case Native => Opcodes.ACC_NATIVE
        case Strict => Opcodes.ACC_STRICT
        case Synchronized => Opcodes.ACC_SYNCHRONIZED
        case VarArgs => Opcodes.ACC_VARARGS
      }

  private def accessFlag(option: FieldAccessOption): Int = option match {
        case Static => Opcodes.ACC_STATIC
        case Final => Opcodes.ACC_FINAL
        case Enum => Opcodes.ACC_ENUM
        case Synthetic => Opcodes.ACC_SYNTHETIC
        case Transient => Opcodes.ACC_TRANSIENT
        case Volatile => Opcodes.ACC_VOLATILE
      }

  private def accessFlag(option: ClassAccessOption): Int = option match {
        case Final => Opcodes.ACC_FINAL
        case Enum => Opcodes.ACC_ENUM
        case Synthetic => Opcodes.ACC_SYNTHETIC
        case Abstract => Opcodes.ACC_ABSTRACT
        case Annotation => Opcodes.ACC_ANNOTATION
        case Interface => Opcodes.ACC_INTERFACE
        case Super => Opcodes.ACC_SUPER
      }

  private def accessFlag(option: InnerClassAccessOption): Int = option match {
        case Final => Opcodes.ACC_FINAL
        case Enum => Opcodes.ACC_ENUM
        case Synthetic => Opcodes.ACC_SYNTHETIC
        case Abstract => Opcodes.ACC_ABSTRACT
        case Annotation => Opcodes.ACC_ANNOTATION
        case Interface => Opcodes.ACC_INTERFACE
        case Static => Opcodes.ACC_STATIC
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
