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
import org.bovinegenius.rygg.jil.StaticField
import org.bovinegenius.rygg.jil.VirtualMethodCall
import org.bovinegenius.rygg.jil.LiteralString
import org.bovinegenius.rygg.jil.Sequence

object Main {
  def main(args: Array[String]): Unit = {
    val Array(classpath, inputFile, outputDir) = args
    CodeGen.generateCode()
    
    //val classFile = AsmBuilder.HelloWorld
    //IO.spit(outputFile, classFile)

    val className = ClassName(PackageName("org.bovinegenius.test"), "TestClass")
    val testClass = Class(
        sourceFile = inputFile,
        access = Public,
        classType = ClassType(className),
        fields = List(
            Field(FieldName(className, "fieldOne"), Public, false, LongType)),
        methods = List(
            Method(
                MethodSignature(MethodName(className, "main"), Public, true, VoidType, List(Arg("args", ArrayType(ClassType("java.lang.String"))))),
                Sequence(VirtualMethodCall(
                    obj = StaticField(FieldName("java.lang.System.out"), ClassType("java.io.PrintStream")),
                    signature = MethodSignature(MethodName("java.io.PrintStream.println"), Public, true, VoidType, List(Arg("value", ClassType("java.lang.String")))),
                    args = List(LiteralString("Some stuff"))
                ),
                VirtualMethodCall(
                    obj = StaticField(FieldName("java.lang.System.out"), ClassType("java.io.PrintStream")),
                    signature = MethodSignature(MethodName("java.io.PrintStream.println"), Public, true, VoidType, List(Arg("value", ClassType("java.lang.String")))),
                    args = List(LiteralString("Some stuff"))
                ))
                )))

    val codeGenerator = CodeGenerator(classpath, List(testClass))
    val classFile = codeGenerator.writeClass(testClass.className)
    val outputFile = outputDir + "/" + className.bytecodeName + ".class"
    //new File(outputFile).mkdirs()
    IO.spit(outputFile, classFile)
    println(s"${inputFile} -> ${outputFile}")
  }
}

