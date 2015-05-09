package org.bovinegenius.rygg.jil

import java.util.Arrays

import org.objectweb.asm.Opcodes
import org.objectweb.asm.{Type => AsmType}

case class PackageName(val name: String) {
  val bytecodeName: String = name.replace(".", "/")
}

case class ClassName(val packageName: PackageName, val name: String) {
  val bytecodeName: String = s"${packageName.bytecodeName}/${name}"
  val classpath: String = s"${bytecodeName}.class"
  val prettyName: String = s"${packageName.name}.${name}"
}
object ClassName {
  def apply(qualifiedName: String): ClassName = {
    val parts: Array[String] = qualifiedName.split("\\.")
    val partsStr = Arrays.toString(parts.asInstanceOf[Array[Object]])
    val packageName = PackageName(parts.slice(0, parts.length - 1).mkString("."))
    ClassName(packageName, parts.last)
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

sealed trait AccessLevel {
  def prettyName: String
}
case object Public extends AccessLevel { val prettyName: String = "public" }
case object Private extends AccessLevel { val prettyName: String = "private" }
case object Protected extends AccessLevel { val prettyName: String = "protected" }
case object Package extends AccessLevel { val prettyName: String = "package" }

object AccessLevel {
  def apply(flags: AccessFlags): AccessLevel = {
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

sealed trait Classy {
  def className: ClassName
  def pretty: String
}
case class Class(val sourceFile: String, val access: AccessLevel, val classType: ClassType, val fields: List[Field], val methods: List[Method]) extends Classy {
  val className: ClassName = classType.name
  val pretty: String = {
    val prettyFields = fields.map(_.pretty).mkString(";\n  ")
    val prettyMethods = methods.map(_.signature.pretty).mkString(";\n  ")
    s"${access.prettyName} class ${classType.prettyName} {\n  ${prettyFields}\n  ${prettyMethods}\n}"
  }
}
case class Interface(val sourceFile: String, val access: AccessLevel, val classType: ClassType, val methods: List[MethodSignature]) extends Classy{
  val className: ClassName = classType.name
  val pretty: String = {
    val prettyMethods = methods.map(_.pretty).mkString(";\n  ")
    s"${access.prettyName} interface ${classType.prettyName} {\n  ${prettyMethods}\n}"
  }
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

case class Field(val name: FieldName, val access: AccessLevel, val static: Boolean, val fieldType: Type) {
  val pretty: String = {
    val staticStr = if (static) " static" else ""
    s"${access.prettyName}${staticStr} ${fieldType.prettyName} ${name.prettyName}"
  }
}

case class Arg(val name: String, val argType: Type)
object Arg {
  implicit def pair2Arg(pair: (String,Type)): Arg = Arg(pair._1, pair._2)
}
case class MethodSignature(val name: MethodName, val access: AccessLevel, val static: Boolean, val returnType: Type, val args: List[Arg]) {
  val pretty: String = {
    val staticStr = if (static) " static" else ""
    val argsStr = args.map(arg => s"${arg.argType.prettyName} ${arg.name}").mkString(", ")
    s"${access.prettyName}${staticStr} ${returnType.prettyName} ${name.prettyName}(${argsStr})"
  }
}
case class Method(val signature: MethodSignature, val body: Option[Expression])
object Method {
  def apply(signature: MethodSignature, body: Expression): Method = Method(signature, Some(body))
}
sealed trait Expression {
  def expressionType: Type
}
//case class StaticMethodCall(val signature: MethodSignature, val args: List[Expression]) extends Expression {
//  val expressionType: Type = signature.returnType
//}
case class StaticField(val fieldName: FieldName, val fieldType: Type) extends Expression {
  val expressionType: Type = fieldType
}
case class VirtualMethodCall(val obj: Expression, val signature: MethodSignature, val args: List[Expression]) extends Expression {
  val expressionType: Type = signature.returnType
}
case class LiteralString(val value: String) extends Expression {
  val expressionType: Type = ClassType(ClassName(PackageName("java.lang"), "String"))
}
case class Sequence(expr1: Expression, expr2: Expression) extends Expression {
  val expressionType: Type = expr2.expressionType
}

sealed trait Type {
  def bytecodeName: String
  def prettyName: String
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
}
object ClassType {
  def apply(qualifiedName: String): ClassType = ClassType(ClassName(qualifiedName))
}

case class ArrayType(val innerType: Type) extends Type {
  val bytecodeName: String = s"[${innerType.bytecodeName}"
  val prettyName: String = s"${innerType.prettyName}[]"
}
object ArrayType {
  def apply(qualifiedName: String): ArrayType = ArrayType(ClassType(qualifiedName))
}
case object VoidType extends Type {
  val name: String = "void"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object IntType extends Type {
  val name: String = "int"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object ByteType extends Type {
  val name: String = "byte"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object ShortType extends Type {
  val name: String = "short"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object LongType extends Type {
  val name: String = "long"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object FloatType extends Type {
  val name: String = "float"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object DoubleType extends Type {
  val name: String = "double"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object BooleanType extends Type {
  val name: String = "boolean"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
case object CharType extends Type {
  val name: String = "char"
  val bytecodeName: String = name;
  val prettyName: String = name;
}
