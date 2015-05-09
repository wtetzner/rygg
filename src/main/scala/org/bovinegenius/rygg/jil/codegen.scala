package org.bovinegenius.rygg.jil

import java.io.OutputStream
import org.objectweb.asm.Opcodes
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import com.sun.org.apache.bcel.internal.generic.ALOAD
import com.sun.org.apache.bcel.internal.generic.INVOKEVIRTUAL
import com.sun.org.apache.bcel.internal.generic.NEW
import com.sun.org.apache.bcel.internal.generic.GETSTATIC
import com.sun.org.apache.bcel.internal.generic.RETURN
import com.sun.org.apache.bcel.internal.generic.DUP
import com.sun.org.apache.bcel.internal.generic.INVOKESPECIAL

object CodeGenerator {
  def write(jilClass: Class, out: OutputStream): Unit = {
    
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
