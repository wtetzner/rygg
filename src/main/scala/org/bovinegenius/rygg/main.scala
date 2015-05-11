package org.bovinegenius.rygg

import org.bovinegenius.rygg.jil.Class
import org.bovinegenius.rygg.jil.ClassName
import org.bovinegenius.rygg.jil.PackageName
import org.bovinegenius.rygg.jil.Public
import org.bovinegenius.rygg.jil.ClassType
import org.bovinegenius.rygg.jil.FieldName
import org.bovinegenius.rygg.jil.LongType
import org.bovinegenius.rygg.jil.Field
import org.bovinegenius.rygg.jil.CodeGenerator
import org.bovinegenius.rygg.io.IO
import java.io.File
import org.bovinegenius.rygg.jil.MethodName
import org.bovinegenius.rygg.jil.MethodSignature
import org.bovinegenius.rygg.jil.VoidType
import org.bovinegenius.rygg.jil.Arg
import org.bovinegenius.rygg.jil.ArrayType
import org.bovinegenius.rygg.jil.Method
import org.bovinegenius.rygg.jil.VirtualMethodCall
import org.bovinegenius.rygg.jil.LiteralString
import org.bovinegenius.rygg.jil.Sequence
import org.bovinegenius.rygg.jil.AstBuilder

object Main {
  def main(args: Array[String]): Unit = {
    val Array(classpath, inputFile, outputDir) = args
    CodeGen.generateCode()


    val astBuilder: AstBuilder = CodeGenerator(classpath, List()).astBuider
    import astBuilder._

    val className = ClassName("org.bovinegenius.test.TestClass")
    val testClass = Class(
        sourceFile = inputFile,
        access = Public,
        classType = ClassType(className),
        fields = List(
            Field(FieldName(className, "fieldOne"), Public, false, LongType)),
        methods = List(
            Method(
                MethodSignature(MethodName(className, "main"), Public, true, VoidType, List("args" -> ArrayType("java.lang.String"))),
                progn(invokeVirtual(
                    obj = lookupField("java.lang.System.out"),
                    methodName = MethodName("java.io.PrintStream.out.println"),
                    args = List(const("Some stuff"))
                ),
                invokeVirtual(
                    obj = lookupField("java.lang.System.out"),
                    methodName = MethodName("java.io.PrintStream.out.println"),
                    args = List(const("Some stuff"))
                )
                ))
                ))

    val codeGenerator = CodeGenerator(classpath, List(testClass))
    val classFile = codeGenerator.writeClass(testClass.className)
    val outputFile = outputDir + "/" + className.bytecodeName + ".class"

    IO.spit(outputFile, classFile)
    println(s"${inputFile} -> ${outputFile}")
  }
}

