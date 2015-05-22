package org.bovinegenius.rygg.jil.asm

import org.objectweb.asm.{Type => AsmType}
import java.util.Arrays

sealed trait Type {
  def bytecodeName: String
  def prettyName: String
  def boxed: Type
  def sameAs(other: Type): Boolean
  def stackSize: Int
  def primitive: Boolean
  def bytecodeType: Type
}
object Type {
  implicit def fromAsmType(asmType: AsmType): Type = {
    asmType.getSort match {
      case AsmType.ARRAY => ArrayType(fromAsmType(asmType.getElementType))
      case AsmType.BOOLEAN => BooleanType
      case AsmType.BYTE => ByteType
      case AsmType.CHAR => CharType
      case AsmType.DOUBLE => DoubleType
      case AsmType.FLOAT => FloatType
      case AsmType.INT => IntType
      case AsmType.LONG => LongType
      case AsmType.METHOD => throw new RuntimeException(s"Method types are currently unsupported: ${asmType}")
      case AsmType.OBJECT => ClassType(InternalName(asmType.getInternalName).className)
      case AsmType.SHORT => ShortType
      case AsmType.VOID => VoidType
      case _ => throw new RuntimeException(s"Unknown type: ${asmType}")
    }
  }
}
case class ClassType(val name: ClassName) extends Type {
  val bytecodeName: String = name.bytecodeName
  val prettyName: String = name.prettyName
  def sameAs(otherType: Type): Boolean = this == otherType
  val boxed: Type = this
  val stackSize: Int = 1
  val primitive = false
  val bytecodeType: Type = this
}
object ClassType {
  val string: ClassType = ClassType("java.lang.String")
  def apply(qualifiedName: String): ClassType = ClassType(ClassName(qualifiedName))
}

case class ArrayType(val innerType: Type) extends Type {
  val bytecodeName: String = s"[${innerType.bytecodeName}"
  val prettyName: String = s"${innerType.prettyName}[]"
  def sameAs(otherType: Type): Boolean = otherType match {
    case ArrayType(inner) => inner.sameAs(innerType)
    case _ => false
  }
  val boxed: Type = this
  val stackSize: Int = 1
  val primitive = false
  val bytecodeType: Type = this
}
object ArrayType {
  def apply(qualifiedName: String): ArrayType = ArrayType(ClassType(qualifiedName))
}
case object VoidType extends Type {
  val name: String = "void"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Void"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 0
  val primitive = true
  val bytecodeType: Type = this
}
case object IntType extends Type {
  val name: String = "int"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Integer"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 1
  val primitive = true
  val bytecodeType: Type = IntType
}
case object ByteType extends Type {
  val name: String = "byte"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Byte"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 1
  val primitive = true
  val bytecodeType: Type = IntType
}
case object ShortType extends Type {
  val name: String = "short"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Short"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 1
  val primitive = true
  val bytecodeType: Type = IntType
}
case object LongType extends Type {
  val name: String = "long"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Long"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 2
  val primitive = true
  val bytecodeType: Type = this
}
case object FloatType extends Type {
  val name: String = "float"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Float"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 1
  val primitive = true
  val bytecodeType: Type = this
}
case object DoubleType extends Type {
  val name: String = "double"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Double"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 2
  val primitive = true
  val bytecodeType: Type = this
}
case object BooleanType extends Type {
  val name: String = "boolean"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Boolean"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 1
  val primitive = true
  val bytecodeType: Type = IntType
}
case object CharType extends Type {
  val name: String = "char"
  val bytecodeName: String = name;
  val prettyName: String = name;
  val boxed: Type = ClassType(ClassName("java.lang.Character"))
  def sameAs(otherType: Type): Boolean = this == otherType || boxed == otherType.boxed
  val stackSize: Int = 1
  val primitive = true
  val bytecodeType: Type = this
}

case class PackageName(val name: String) {
  val bytecodeName: String = name.replace(".", "/")
}

case class ClassName(val packageName: PackageName, val name: String) {
  val bytecodeName: String = s"${packageName.bytecodeName}/${name}"
  val classpath: String = s"${bytecodeName}.class"
  val prettyName: String = s"${packageName.name}.${name}"
  def apply(memberName: String): MemberName = MemberName(this, memberName)
}
object ClassName {
  def apply(qualifiedName: String): ClassName = {
    val parts: Array[String] = qualifiedName.split("\\.")
    val partsStr = Arrays.toString(parts.asInstanceOf[Array[Object]])
    val packageName = PackageName(parts.slice(0, parts.length - 1).mkString("."))
    ClassName(packageName, parts.last)
  }
}

case class MemberName(val className: ClassName, val name: String) {
  def asMethodName: MethodName = MethodName(className, name)
  def asFieldName: FieldName = FieldName(className, name)
}

case class FieldName(val className: ClassName, val name: String) {
  val prettyName: String = s"${className.prettyName}.${name}"
}
object FieldName {
  def apply(qualifiedName: String): FieldName = {
    val parts = qualifiedName.split("\\.")
    val className = ClassName(parts.slice(0, parts.length - 1).mkString("."))
    FieldName(className, parts(parts.length - 1))
  }
}

case class MethodName(val className: ClassName, val name: String) {
  val prettyName: String = s"${className.prettyName}.${name}"
}
object MethodName {
  def apply(qualifiedName: String): MethodName = {
    val parts = qualifiedName.split("\\.")
    val className = ClassName(parts.slice(0, parts.length - 1).mkString("."))
    MethodName(className, parts(parts.length - 1))
  }
}

case class InternalName(parts: Array[String]) {
  val className: ClassName = {
    val packageName: PackageName = PackageName(parts.slice(0, parts.length - 1).mkString("."))
    ClassName(packageName, parts.last)
  }
}
object InternalName {
  def apply(name: String): InternalName = {
    InternalName(name.split("/"))
  }
}
