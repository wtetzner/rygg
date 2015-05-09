package org.bovinegenius.rygg.jil

import org.bovinegenius.rygg.jil.AccessFlags._
import org.bovinegenius.rygg.resources.CompilationResources
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.ClassReader
import org.objectweb.asm.{Type => AsmType}

sealed trait Classes {
  def lookup(className: ClassName): Option[Classy]
}

case class InputClasses(classes: List[Classy]) extends Classes {
  private val classMap: Map[ClassName,Classy] =
    classes.groupBy(_.className).mapValues(_.head)

  override def lookup(className: ClassName): Option[Classy] = classMap.get(className)
}

case class ResourceClasses(classpath: String) extends Classes {
  private def resources: CompilationResources = CompilationResources(classpath)
  
  override def lookup(className: ClassName): Option[Classy] = {
    resources.loadBytes(className.classpath) match {
      case None => None
      case Some(bytes) => {
        val reader = new ClassReader(bytes)
        val visitor = new BytecodeToClassVisitor()
        reader.accept(visitor, 0)
        Some(visitor.result)
      }
    }
  }
}

class BytecodeToClassVisitor() extends ClassVisitor(Opcodes.ASM5) {
  private var classy: Classy = null;
  private var fields: List[Field] = List()
  private var methods: List[MethodSignature] = List()
  
  lazy val result: Classy = {
    if (classy == null) {
      throw new RuntimeException(s"Failed to parse class")
    } else {
      classy match {
        case cls: Class => cls.copy(fields = fields, methods = methods.map(s => Method(s, None)))
        case interface: Interface => interface.copy(methods = methods)
      }
    }
  }

  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    if (access.isSet(Opcodes.ACC_INTERFACE)) {
      classy = Interface("<unknown>", AccessLevel(access), ClassType(InternalName(name).className), List())
    } else {
      classy = Class("<unknown>", AccessLevel(access), ClassType(InternalName(name).className), List(), List())
    }
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object): FieldVisitor = {
    val accessFlags: AccessFlags = access
    fields = fields :+ Field(FieldName(classy.className, name), AccessLevel(accessFlags), accessFlags.isSet(Opcodes.ACC_STATIC), AsmType.getType(desc))
    println(s"field name: $name, desc: $desc, signature: $signature")
    super.visitField(access, name, desc, signature, value)
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    val accessFlags: AccessFlags = access
    val methodType: AsmType = AsmType.getType(desc)
    methods = methods :+ MethodSignature(MethodName(classy.className, name), AccessLevel(accessFlags), accessFlags.isSet(Opcodes.ACC_STATIC), methodType.getReturnType,
        methodType.getArgumentTypes.toList.map(t => Arg("_", t)))
    println(s"method name: $name, desc: $desc, signature: $signature")
    super.visitMethod(access, name, desc, signature, exceptions)
  }
}

case class CombinationClasses(classeses: Classes*) extends Classes {
  override def lookup(className: ClassName): Option[Classy] = {
    for (classes <- classeses) {
      val result = classes.lookup(className)
      if (!result.isEmpty) {
        return result
      }
    }
    return None
  }
}
