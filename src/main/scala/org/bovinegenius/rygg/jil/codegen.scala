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

case class CodeGenerator(val classpath: String, val inputClasses: List[Classy]) {
  private val classes: Classes = CombinationClasses(InputClasses(inputClasses), ResourceClasses(classpath))

  val astBuider: AstBuilder = AstBuilder(classes)

  def writeClass(className: ClassName): Array[Byte] = classes.lookup(className) match {
    case None => throw new RuntimeException(s"No such class: ${className.bytecodeName}")
    case Some(cls: Class) => writeClass(cls)
    case Some(interface: Interface) => writeInterface(interface)
  }

  private def writeInterface(inteface: Interface): Array[Byte] = {
    null
  }

  private def writeClass(jilClass: Class): Array[Byte] = {
    println("lookedup: %s".format(classes.lookup(ClassName(PackageName("java.io"), "PrintStream")).map(_.pretty)))
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

  private def writeMethod(method: Method, cw: ClassWriter): Unit = {
    val mv: MethodVisitor = cw.visitMethod(accessLevel(method.signature.access) | static(method.signature.static), method.signature.name.name, descriptor(method.signature), null, null);
    mv.visitCode()
    writeExpression(method.body, mv)
    mv.visitInsn(Opcodes.RETURN)
    // Don't need to use real values here, since we're using the COMPUTE_MAXS flag
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }

  private def writeExpression(expression: Option[Expression], mv: MethodVisitor): Unit = expression match {
    case None => ()
    case Some(expr) => writeExpression(expr, mv)
  }
  
  private def writeExpression(expression: Expression, mv: MethodVisitor): Unit = {
    expression match {
      case StaticFieldAccess(fieldName, fieldType) => mv.visitFieldInsn(Opcodes.GETSTATIC, fieldName.className.bytecodeName, fieldName.name, descriptor(fieldType))
      case FieldAccess(expr, fieldName, fieldType) => {
        writeExpression(expr, mv)
        mv.visitFieldInsn(Opcodes.GETFIELD, fieldName.className.bytecodeName, fieldName.name, descriptor(fieldType))
      }
      case VirtualMethodCall(expr, sig, args) => {
        writeExpression(expr, mv)
        args.foreach { arg => writeExpression(arg, mv) }
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, expr.expressionType.bytecodeName, sig.name.name, descriptor(sig), false)
      }
      case LiteralString(str) => mv.visitLdcInsn(str)
      case Sequence(expr1, expr2) => {
        writeExpression(expr1, mv)
        writeExpression(expr2, mv)
      }
    }
  }

  private def writeField(field: Field, cw: ClassWriter): Unit = {
    val fv: FieldVisitor = cw.visitField(accessLevel(field.access) | static(field.static), field.name.name, descriptor(field.fieldType), null, null)
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
