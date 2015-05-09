package org.bovinegenius.rygg.jil

case class PackageName(val name: String) {
  val bytecodeName: String = name.replace(".", "/")
}

case class ClassName(val packageName: PackageName, val name: String) {
  val bytecodeName: String = s"${packageName.bytecodeName}/${name}"
}

sealed trait AccessLevel
case object Public extends AccessLevel
case object Private extends AccessLevel
case object Protected extends AccessLevel
case object Package extends AccessLevel

case class Class(val sourceFile: String, val access: AccessLevel, val classType: ClassType, val fields: List[Field], val methods: List[Method])
case class Interface(val sourceFile: String, val access: AccessLevel, val classType: ClassType, val methods: List[MethodSignature])

case class FieldName(val className: ClassName, val name: String)

case class MethodName(val className: ClassName, val name: String)

case class Field(val name: FieldName, val access: AccessLevel, val static: Boolean, val fieldType: Type)

case class Arg(val name: String, val argType: Type)
case class MethodSignature(val name: MethodName, val access: AccessLevel, val static: Boolean, val returnType: Type, val args: List[Arg])
case class Method(val signature: MethodSignature, val body: Expression)


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

sealed trait Type {
  def bytecodeName: String
}
case class ClassType(val name: ClassName) extends Type {
  val bytecodeName: String = name.bytecodeName
}
case class ArrayType(val innerType: Type) extends Type {
  val bytecodeName: String = s"[${innerType.bytecodeName}"
}
case object VoidType extends Type {
  val name: String = "void"
  val bytecodeName: String = name;
}
case object IntType extends Type {
  val name: String = "int"
  val bytecodeName: String = name;
}
case object ByteType extends Type {
  val name: String = "byte"
  val bytecodeName: String = name;
}
case object ShortType extends Type {
  val name: String = "short"
  val bytecodeName: String = name;
}
case object LongType extends Type {
  val name: String = "long"
  val bytecodeName: String = name;
}
case object FloatType extends Type {
  val name: String = "float"
  val bytecodeName: String = name;
}
case object DoubleType extends Type {
  val name: String = "double"
  val bytecodeName: String = name;
}
case object BooleanType extends Type {
  val name: String = "boolean"
  val bytecodeName: String = name;
}
case object CharType extends Type {
  val name: String = "char"
  val bytecodeName: String = name;
}
