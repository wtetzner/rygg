package org.bovinegenius.rygg.jil

case class AstBuilder(classes: Classes, addClass: Class => Unit) {
  def lookupField(fieldName: FieldName): StaticFieldAccess = {
    val maybeField = classes.lookup(fieldName.className).flatMap(_.field(fieldName))
    if (maybeField.isEmpty) {
      throw new RuntimeException(s"No such field: ${fieldName.prettyName}")
    } else {
      val field = maybeField.get
      if (field.static) {
        StaticFieldAccess(fieldName, field.fieldType)
      } else {
        throw new RuntimeException(s"Field ${fieldName} is not static")
      }
    }
  }

  def lookupField(fieldName: String): StaticFieldAccess = lookupField(FieldName(fieldName))
  
  def lookupField(obj: Expression, fieldName: FieldName): FieldAccess = {
    val maybeField = classes.lookup(fieldName.className).flatMap(_.field(fieldName))
    if (maybeField.isEmpty) {
      throw new RuntimeException(s"No such field: ${fieldName.prettyName}")
    } else {
      val field = maybeField.get
      if (field.static) {
        throw new RuntimeException(s"Field ${fieldName} is not static")
      } else {
        FieldAccess(obj, fieldName, field.fieldType)
      }
    }
  }
  
  def lookupField(obj: Expression, fieldName: String): FieldAccess = lookupField(obj, FieldName(fieldName))

  def invokeVirtual(obj: Expression, methodName: MethodName, args: List[Expression]): VirtualMethodCall = {
    val maybeMethod = classes.lookup(methodName.className).flatMap(_.resolveMethod(methodName, args.map(_.expressionType)))
    if (maybeMethod.isEmpty) {
      val argsStr = args.map(_.expressionType.prettyName).mkString(", ")
      throw new RuntimeException(s"No matching method for ${methodName.prettyName}(${argsStr})")
    } else {
      val method = maybeMethod.get
      VirtualMethodCall(obj, method, args)
    }
  }

  def invokeVirtual(obj: Expression, methodName: String, args: Expression*): VirtualMethodCall = {
    val name: MethodName = if (methodName.contains(".")) MethodName(methodName) else MethodName(ClassName(obj.expressionType.prettyName), methodName)
    invokeVirtual(obj, name, args.toList)
  }
  
  def const(lit: String): LiteralString = LiteralString(lit)

  def progn(expr: Expression, exprs: Expression*): Expression = {
    if (exprs.isEmpty) {
      expr
    } else {
      var current: Expression = expr
      for (exp <- exprs) {
        current = Sequence(current, expr)
      }
      current
    }
  }
  
  def newClass(sourceFile: String, access: AccessLevel, classType: ClassType, fields: List[Field], methods: List[Method]): Class = {
    val result = Class(sourceFile, access, classType, fields, methods)
    addClass(result)
    result
  }
}