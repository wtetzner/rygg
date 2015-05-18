package org.bovinegenius.rygg.jil

case class AstBuilder(classes: Classes, addClass: Classy => Unit) {
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
  
  def invokeSpecial(obj: Expression, methodName: MethodName, args: List[Expression]): InvokeSpecial = {
    val maybeMethod = classes.lookup(methodName.className).flatMap(_.resolveMethod(methodName, args.map(_.expressionType)))
    if (maybeMethod.isEmpty) {
      val argsStr = args.map(_.expressionType.prettyName).mkString(", ")
      throw new RuntimeException(s"No matching method for ${methodName.prettyName}(${argsStr})")
    } else {
      val method = maybeMethod.get
      InvokeSpecial(obj, method, args)
    }
  }
  
  def invokeSpecial(obj: Expression, methodName: String, args: Expression*): InvokeSpecial = {
    val name: MethodName = if (methodName.contains(".")) MethodName(methodName) else MethodName(ClassName(obj.expressionType.prettyName), methodName)
    invokeSpecial(obj, name, args.toList)
  }
  
  def const(lit: String): StringLiteral = StringLiteral(lit)
  def const(lit: Int): IntLiteral = IntLiteral(lit)
  def const(lit: Long): LongLiteral = LongLiteral(lit)
  def const(lit: Boolean): BooleanLiteral = BooleanLiteral(lit)
  def const(lit: Char): CharLiteral = CharLiteral(lit)
  def const(lit: Short): ShortLiteral = ShortLiteral(lit)

  def progn(expr: Expression, exprs: Expression*): Expression = {
    if (exprs.isEmpty) {
      expr
    } else {
      var current: Expression = expr
      for (exp <- exprs) {
        current = Sequence(current, exp)
      }
      current
    }
  }
  
  def newClass(sourceFile: String, access: AccessLevel, classType: ClassType, fields: List[Field], methods: List[Method]): Class = {
    val result = Class(sourceFile, access, classType, fields, methods, List())
    addClass(result)
    result
  }
  
  def newClass(sourceFile: String, access: AccessLevel, classType: ClassType, fields: List[Field], methods: List[Method], interfaces: List[ClassName]): Class = {
    val result = Class(sourceFile, access, classType, fields, methods, interfaces)
    addClass(result)
    result
  }
  
  def newInterface(sourceFile: String, access: AccessLevel, classType: ClassType, methods: List[MethodSignature]): Interface = {
    val result = Interface(sourceFile, access, classType, methods)
    addClass(result)
    result
  }
  
  def getClassy(className: ClassName, generate: () => Classy): Classy = {
    val result = classes.lookup(className)
    if (result.isEmpty) {
      generate()
    } else {
      result.get
    }
  }
  
  def method(methodName: MemberName, access: AccessLevel, staticness: Staticness, returnType: Type, args: (String, Type)*)(body: () => Expression): Method = {
    val sig = MethodSignature(methodName.asMethodName, access, staticness.isStatic, returnType, args.map(p => Arg.pair2Arg(p)).toList)
    Method(sig, Some(body))
  }

  def method(methodName: MemberName, access: AccessLevel, returnType: Type, args: (String, Type)*)(body: () => Expression): Method = {
    method(methodName, access, NonStatic, returnType, args :_*)(body)
  }

  def constructor(className: ClassName, access: AccessLevel, args: (String, Type)*)(body: () => Expression): Method = {
    method(className("<init>"), access, NonStatic, VoidType, args :_*)(body)
  }

  def setField(obj: Expression, fieldName: String, value: Expression): SetField = {
    SetField(obj, obj.expressionType.asInstanceOf[ClassType].name(fieldName).asFieldName, value)
  }

  def getThis(classType: ClassType): AccessThis = {
    AccessThis(classType)
  }
  
  def getArg(index: Int, argType: Type): AccessArgument = {
    AccessArgument(index, argType)
  }

  def makeNew(classType: ClassType, args: Expression*): New = {
    New(classType, args.toList)
  }

  def field(fieldName: MemberName, access: AccessLevel, staticness: Staticness, finalness: Finalness, fieldType: Type): Field = {
    Field(fieldName.asFieldName, access, finalness.isFinal, staticness.isStatic, fieldType)
  }

  def recordField(fieldName: MemberName, fieldType: Type): Field = {
    field(fieldName, Private, NonStatic, Final, fieldType)
  }

  def let(name: String, value: Expression)(body: (LocalVariableLookup) => Expression): Let = {
    val localVar = LocalVariable(LocalVariableName(name), value.expressionType)
    val bodyExpr = body(LocalVariableLookup(localVar))
    Let(localVar, value, bodyExpr)
  }

  def println(item: Expression): VirtualMethodCall = {
    invokeVirtual(lookupField("java.lang.System.out"), "java.io.PrintStream.println", item)
  }
  
  def invokeStatic(classType: ClassType, methodName: MethodName, args: List[Expression]): InvokeStatic = {
    val maybeMethod = classes.lookup(methodName.className).flatMap(_.resolveMethod(methodName, args.map(_.expressionType)))
    if (maybeMethod.isEmpty) {
      val argsStr = args.map(_.expressionType.prettyName).mkString(", ")
      throw new RuntimeException(s"No matching method for ${methodName.prettyName}(${argsStr})")
    } else {
      val method = maybeMethod.get
      InvokeStatic(classType, method, args)
    }
  }

  def invokeStatic(classType: ClassType, methodName: String, args: Expression*): InvokeStatic = {
    val name: MethodName = if (methodName.contains(".")) MethodName(methodName) else MethodName(classType.name, methodName)
    invokeStatic(classType, name, args.toList)
  }
  
  def boxed(exp: Expression): Expression = {
    exp.expressionType match {
      case VoidType => throw new RuntimeException("Can't box void type")
      case ClassType(_) | ArrayType(_) => exp
      case IntType | BooleanType | ShortType | LongType | CharType | ByteType | FloatType | DoubleType =>
        invokeStatic(exp.expressionType.boxed.asInstanceOf[ClassType], "valueOf", exp)
      
    }
  }
  
  def toString(item: Expression): Expression = {
    val expr = boxed(item)
    if (expr.expressionType == ClassType(ClassName("java.lang.String"))) {
      expr
    } else {
      VirtualMethodCall(expr, MethodSignature(item.expressionType.boxed.asInstanceOf[ClassType].name("toString").asMethodName, Public, false, ClassType(ClassName("java.lang.String")), List()), List())
    }
    //invokeVirtual(item, item.expressionType.boxed.prettyName + ".toString")
  }
  
  private def forStringBuilder(expr: Expression): Expression =
    if (expr.expressionType.primitive || expr.expressionType == ClassType("java.lang.String")) {
      expr
    } else {
      toString(expr)
    }

  def concat(exprs: Expression*): Expression = {
    val items = concatAdjoiningStrings(exprs.toList).map(forStringBuilder _)
    if (items.size == 0) {
      const("")
    } else if (items.size == 1) {
      toString(items(0))
    } else if (items.size == 2) {
      (items(0), items(1)) match {
        case (StringLiteral(lit1), StringLiteral(lit2)) => StringLiteral(lit1 + lit2)
        case _ => concatStrs(items(0), items(1), List())
      }
    } else {
      concatStrs(items(0), items(1), items.drop(2).toList)
    }
  }
  
  private def isStringLiteral(expr: Expression): Boolean = expr match {
    case StringLiteral(_) => true
    case _ => false
  }
  
  private def takeStringLiterals(exprs: List[Expression]): List[Expression] = {
    val segment = exprs.takeWhile(isStringLiteral _)
    if (segment.size == 0) {
      exprs.take(1)
    } else {
      segment
    }
  }
  
  private def split(list: List[Expression]) : List[List[Expression]] = list match {
    case Nil => Nil
    case h::t => val segment = takeStringLiterals(list)
      segment :: split(list drop segment.length)
  }
  
  private def mergeIfStringLiteral(items: List[Expression]): List[Expression] = {
    if (items.size == 0) {
      items
    } else if (isStringLiteral(items(0))) {
      List(StringLiteral(items.map(exp => exp.asInstanceOf[StringLiteral].value).mkString))
    } else {
      items
    }
  }
  
  private def concatAdjoiningStrings(items: List[Expression]): List[Expression] = {
    if (items.size == 0) {
      items
    } else {
      val chunks = split(items)
      chunks.flatMap(mergeIfStringLiteral _)
    }
  }
  
  private def concatStrs(item1: Expression, item2: Expression, items: List[Expression]): Expression = {
    val sb = makeNew(ClassType("java.lang.StringBuilder"))
    var current = invokeVirtual(invokeVirtual(sb, "append", item1), "append", item2)
    for (item <- items) {
      current = invokeVirtual(current, "append", item)
    }
    invokeVirtual(current, "toString")
  }
}
