package org.bovinegenius.rygg.jil

import org.objectweb.asm._
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Opcodes.ACC_PRIVATE
import org.objectweb.asm.Opcodes.ACC_PROTECTED
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.objectweb.asm.Opcodes.ACC_STATIC
import org.objectweb.asm.{Type => AsmType}
import org.objectweb.asm.commons.LocalVariablesSorter

case class CodeGenerator(val classpath: String, val inputClasses: List[Classy]) {
  private var classes: CombinationClasses = CombinationClasses(InputClasses(inputClasses), ResourceClasses(classpath))

  val astBuider: AstBuilder = AstBuilder(classes, cls => { classes = classes.addClass(cls) })

  def writeClass(className: ClassName): Array[Byte] = classes.lookup(className) match {
    case None => throw new RuntimeException(s"No such class: ${className.bytecodeName}")
    case Some(cls: Class) => writeClass(cls)
    case Some(interface: Interface) => writeInterface(interface)
  }

  def writeInputClasses(): Iterator[(ClassName, Array[Byte])] =
    classes.addedClasses.map(c => (c, writeClass(c)))

  private def writeInterface(inteface: Interface): Array[Byte] = {
    null
  }

  private def writeClass(jilClass: Class): Array[Byte] = {
    import org.objectweb.asm.Opcodes._
    val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(Opcodes.V1_6, accessLevel(jilClass.access) | Opcodes.ACC_SUPER, jilClass.classType.name.bytecodeName, null, "java/lang/Object", null);
    cw.visitSource(jilClass.sourceFile, null);

    for (field <- jilClass.fields) {
      writeField(field, cw)
    }
    for (method <- jilClass.methods) {
      writeMethod(method, cw)
    }

    cw.toByteArray()
  }

  private def localVariablesSorter(apiVersion: Int, cw: ClassWriter, access: Int, name: String, desc: String): LocalVariablesSorter =
    new LocalVariablesSorter(Opcodes.ASM5, access, desc, cw.visitMethod(access, name, desc, null, null)) {}
  
  private def writeMethod(method: Method, cw: ClassWriter): Unit = {
    val mv: LocalVariablesSorter = localVariablesSorter(Opcodes.V1_6, cw, accessLevel(method.signature.access) | static(method.signature.static), method.signature.name.name, descriptor(method.signature)) // cw.visitMethod(accessLevel(method.signature.access) | static(method.signature.static), method.signature.name.name, descriptor(method.signature), null, null);
    mv.visitCode()
    writeExpression(method.body, mv)
    mv.visitInsn(returnInstruction(method.signature.returnType))
    
    // Don't need to use real values here, since we're using the COMPUTE_MAXS flag
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }

  private def writeExpression(expression: Option[Expression], mv: LocalVariablesSorter): Unit = expression match {
    case None => ()
    case Some(expr) => writeExpression(expr, mv, EmptyLocalEnvironment)
  }
  
  private def writeExpression(expression: Expression, mv: LocalVariablesSorter, env: LocalEnvironmentMap): Unit = {
    expression match {
      case StaticFieldAccess(fieldName, fieldType) => mv.visitFieldInsn(Opcodes.GETSTATIC, fieldName.className.bytecodeName, fieldName.name, descriptor(fieldType))
      case FieldAccess(expr, fieldName, fieldType) => {
        writeExpression(expr, mv, env)
        mv.visitFieldInsn(Opcodes.GETFIELD, fieldName.className.bytecodeName, fieldName.name, descriptor(fieldType))
      }
      case VirtualMethodCall(expr, sig, args) => {
        writeExpression(expr, mv, env)
        args.foreach { arg => writeExpression(arg, mv, env) }
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, expr.expressionType.bytecodeName, sig.name.name, descriptor(sig), false)
      }
      case StringLiteral(str) => mv.visitLdcInsn(str)
      case Sequence(expr1, expr2) => {
        writeExpression(expr1, mv, env)
        writeExpression(expr2, mv, env)
      }
      case LongLiteral(long) => mv.visitLdcInsn(long)
      case IntLiteral(int) => mv.visitLdcInsn(int)
      case ShortLiteral(lit) => mv.visitLdcInsn(lit)
      case CharLiteral(lit) => mv.visitLdcInsn(lit)
      case BooleanLiteral(bool) => mv.visitLdcInsn(bool)
      case Let(variable, value, body) => {
        val startLabel = new Label
        mv.visitLabel(startLabel)
        val varIndex = mv.newLocal(asmType(variable.varType))
        writeExpression(value, mv, env)
        mv.visitVarInsn(storeInstruction(value.expressionType), varIndex)
        val newEnv = env.withVar(LocalVariableInfo(variable, varIndex))
        writeExpression(body, mv, newEnv)
        val endLabel = new Label
        mv.visitLabel(endLabel)
        mv.visitLocalVariable(variable.name.name, descriptor(variable.varType), null, startLabel, endLabel, varIndex)
      }
      case LocalVariableLookup(variable) => {
        val index = env(variable.name).getOrElse(throw new RuntimeException(s"No such variable: ${variable.name.name}")).index
        mv.visitVarInsn(loadInstruction(variable.varType), index)
      }
      case AccessThis(cls) => {
        mv.visitVarInsn(Opcodes.ALOAD, 0);
      }
      case SetField(obj, field, value) => {
        println(s"SetField field: ${field}")
        writeExpression(obj, mv, env)
        writeExpression(value, mv, env)
        mv.visitFieldInsn(Opcodes.PUTFIELD, obj.expressionType.bytecodeName, field.name, descriptor(value.expressionType))
      }
      case AccessArgument(index, argType) => {
        mv.visitVarInsn(loadInstruction(argType), index)
      }
      case New(classType, args) => {
        mv.visitTypeInsn(Opcodes.NEW, classType.bytecodeName)
        mv.visitInsn(Opcodes.DUP)
        args.foreach { arg => writeExpression(arg, mv, env) }
        val methodSig = MethodSignature(classType.name("<init>").asMethodName, Public, false, VoidType, args.map(e => Arg("name", e.expressionType)))
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, classType.bytecodeName, methodSig.name.name, descriptor(methodSig), false)
      }
      case InvokeSpecial(expr, sig, args) => {
        writeExpression(expr, mv, env)
        args.foreach { arg => writeExpression(arg, mv, env) }
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, expr.expressionType.bytecodeName, sig.name.name, descriptor(sig), false)
      }
      case InvokeStatic(classType, sig, args) => {
        args.foreach { arg => writeExpression(arg, mv, env) }
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, classType.name.bytecodeName, sig.name.name, descriptor(sig), false)
      }
    }
  }

  private def storeInstruction(varType: Type): Int = {
    varType match {
      case ClassType(_) => Opcodes.ASTORE
      case ArrayType(_) => Opcodes.ASTORE
      case IntType | ByteType | ShortType | BooleanType | CharType => Opcodes.ISTORE
      case DoubleType => Opcodes.DSTORE
      case FloatType => Opcodes.FSTORE
      case LongType => Opcodes.LSTORE
      case VoidType => throw new RuntimeException("Can't store a variable that's Void")
    }
  }
  
    private def returnInstruction(varType: Type): Int = {
    varType match {
      case ClassType(_) => Opcodes.ARETURN
      case ArrayType(_) => Opcodes.ARETURN
      case IntType | ByteType | ShortType | BooleanType | CharType => Opcodes.IRETURN
      case DoubleType => Opcodes.DRETURN
      case FloatType => Opcodes.FRETURN
      case LongType => Opcodes.LRETURN
      case VoidType => Opcodes.RETURN
    }
  }
  
  private def loadInstruction(varType: Type): Int = {
    varType match {
      case ClassType(_) => Opcodes.ALOAD
      case ArrayType(_) => Opcodes.ALOAD
      case IntType | ByteType | ShortType | BooleanType | CharType => Opcodes.ILOAD
      case DoubleType => Opcodes.DLOAD
      case FloatType => Opcodes.FLOAD
      case LongType => Opcodes.LLOAD
      case VoidType => throw new RuntimeException("Can't load a variable that's Void")
    }
  }
  
  private def writeField(field: Field, cw: ClassWriter): Unit = {
    println(s"Field: ${field}")
    val fv: FieldVisitor = cw.visitField(accessLevel(field.access) | static(field.static) | isFinal(field.isFinal), field.name.name, descriptor(field.fieldType), null, null)
  }

  private def isFinal(isFinal: Boolean): Int =
    if (isFinal) {
      Opcodes.ACC_FINAL
    } else {
      0
    }
  
  private def methodType(method: MethodSignature): AsmType = {
    val argTypes: List[Type] = method.args.map(_.argType)
    val types: List[AsmType] = argTypes.map((x: Type) => asmType(x))
    AsmType.getMethodType(asmType(method.returnType), types.toArray :_*)
  }

  private def asmType(jilType: Type): AsmType = {
    jilType match {
      case ClassType(name) => AsmType.getObjectType(name.bytecodeName)
      case VoidType => AsmType.VOID_TYPE
      case IntType => AsmType.INT_TYPE
      case ByteType => AsmType.BYTE_TYPE
      case ShortType => AsmType.SHORT_TYPE
      case LongType => AsmType.LONG_TYPE
      case FloatType => AsmType.FLOAT_TYPE
      case DoubleType => AsmType.DOUBLE_TYPE
      case BooleanType => AsmType.BOOLEAN_TYPE
      case CharType => AsmType.CHAR_TYPE
      case ArrayType(inner) => AsmType.getType(s"[${asmType(inner)}")
    }
  }

  private def descriptor(method: MethodSignature): String = methodType(method).getDescriptor()
  private def descriptor(jilType: Type): String = asmType(jilType).getDescriptor()

  private def static(static: Boolean): Int = {
    import org.objectweb.asm.Opcodes._
    if (static) {
      ACC_STATIC
    } else {
      0
    }
  }

  private def accessLevel(level: AccessLevel): Int = {
    import org.objectweb.asm.Opcodes._
    level match {
      case Public => ACC_PUBLIC
      case Private => ACC_PRIVATE
      case Protected => ACC_PROTECTED
      case Package => 0
    }
  }
}

object AsmBuilder extends Opcodes {
    val HelloWorld: Array[Byte] = helloWorld();

    private def helloWorld(): Array[Byte] = {
        val cw: ClassWriter = new ClassWriter(0)
      
        cw.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, "org/bovinegenius/rygg/test/HelloWorld", null, "java/lang/Object",
                null);
        cw.visitSource("<generated>", null);
      
        HelloWorld_init(cw);
        HelloWorld_main(cw);
        HelloWorld_hello(cw);
      
        return cw.toByteArray();
    }

    private def HelloWorld_init(cw: ClassWriter): Unit = {
        val mv: MethodVisitor = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(Opcodes.ALOAD, 0);
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
    }

    private def HelloWorld_main(cw: ClassWriter): Unit = {
        val mv: MethodVisitor = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        mv.visitCode();
        mv.visitTypeInsn(Opcodes.NEW, "org/bovinegenius/rygg/test/HelloWorld");
        mv.visitInsn(Opcodes.DUP);
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "org/bovinegenius/rygg/test/HelloWorld", "<init>", "()V");
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "org/bovinegenius/rygg/test/HelloWorld", "hello", "()V");
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(2, 1);
        mv.visitEnd();
    }

    private def HelloWorld_hello(cw: ClassWriter): Unit = {
        val mv: MethodVisitor = cw.visitMethod(Opcodes.ACC_PUBLIC, "hello", "()V", null, null);
        mv.visitCode();
        mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        mv.visitLdcInsn("Hello, World!!!");
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(2, 1);
        mv.visitEnd();
    }
}
