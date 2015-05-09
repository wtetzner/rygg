package org.bovinegenius.rygg.jil

case class PackageName(val name: String)

case class ClassName(val packageName: PackageName, val name: String)

case class Class(val public: Boolean, val classType: ClassType, val fields: List[Field], val methods: List[Method])

case class FieldName(val className: ClassName, val name: String)

case class MethodName(val className: ClassName, val name: String)

case class Field(val name: FieldName, val fieldType: Type)

case class Arg(val name: String, val argType: Type)
case class Method(val name: MethodName, val returnType: Type, val args: List[Arg])

sealed trait Type
case class ClassType(val name: ClassName)
case object VoidType {
  val name: String = "void"
}
case object IntType {
  val name: String = "int"
}
case object ByteType {
  val name: String = "byte"
}
case object ShortType {
  val name: String = "short"
}
case object LongType {
  val name: String = "long"
}
case object FloatType {
  val name: String = "float"
}
case object DoubleType {
  val name: String = "double"
}
case object BooleanType {
  val name: String = "boolean"
}
case object CharType {
  val name: String = "char"
}
